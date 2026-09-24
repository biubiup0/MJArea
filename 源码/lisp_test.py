#!/usr/bin/env python3
"""
A very small AutoLISP evaluator, just good enough to load MJArea.lsp and
run its geometry / formatting functions so the maths can be verified
outside AutoCAD.

Usage: python3 lisp_test.py <MJArea.lsp>
"""
import math
import re
import sys


class Sym(str):
    pass


class Dot(list):
    """implements an AutoLISP dotted pair, e.g. (unit . "mm")"""


class LispError(Exception):
    pass


# ------------------------------------------------------------------ reader
def tokenize(src):
    toks = []
    i, n = 0, len(src)
    while i < n:
        c = src[i]
        if c.isspace():
            i += 1
            continue
        if c == ";":
            j = src.find("\n", i)
            i = n if j < 0 else j
            continue
        if c in "()'":
            toks.append(c)
            i += 1
            continue
        if c == '"':
            buf = []
            j = i + 1
            while j < n:
                ch = src[j]
                if ch == "\\":
                    e = src[j + 1]
                    if e == "U" and j + 2 < n and src[j + 2] == "+":
                        buf.append(chr(int(src[j + 3:j + 7], 16)))
                        j += 7
                        continue
                    buf.append({"n": "\n", "t": "\t", "r": "\r",
                                "\\": "\\", '"': '"', "e": "\x1b"}.get(e, e))
                    j += 2
                    continue
                if ch == '"':
                    break
                buf.append(ch)
                j += 1
            if j >= n:
                raise LispError("unterminated string")
            toks.append(("str", "".join(buf)))
            i = j + 1
            continue
        j = i
        while j < n and (not src[j].isspace()) and src[j] not in '();"':
            j += 1
        toks.append(("atom", src[i:j]))
        i = j
    return toks


def atom_value(s):
    try:
        if re.fullmatch(r"[+-]?\d+", s):
            return int(s)
        if re.fullmatch(r"[+-]?(\d+\.\d*|\.\d+|\d+)([eE][+-]?\d+)?", s):
            return float(s)
    except ValueError:
        pass
    return Sym(s.upper())


def parse(toks):
    pos = [0]

    def rd():
        if pos[0] >= len(toks):
            raise LispError("unexpected end of input")
        t = toks[pos[0]]
        pos[0] += 1
        if t == "(":
            lst = []
            while True:
                if pos[0] >= len(toks):
                    raise LispError("unbalanced (")
                if toks[pos[0]] == ")":
                    pos[0] += 1
                    return lst
                if toks[pos[0]] == ("atom", "."):
                    pos[0] += 1
                    tail = rd()
                    if pos[0] < len(toks) and toks[pos[0]] == ")":
                        pos[0] += 1
                    if isinstance(tail, list) and not isinstance(tail, Dot):
                        return Dot(lst + list(tail))
                    return Dot(lst + [tail])
                lst.append(rd())
        if t == ")":
            raise LispError("unexpected )")
        if t == "'":
            return [Sym("QUOTE"), rd()]
        if isinstance(t, tuple):
            if t[0] == "str":
                return t[1]
            return atom_value(t[1])
        return atom_value(t)

    forms = []
    while pos[0] < len(toks):
        forms.append(rd())
    return forms


# ------------------------------------------------------------------ eval
NIL = None
TRUE = Sym("T")


def nil(x):
    return x is None or x == [] or x is False


def truthy(x):
    return not nil(x)


def as_list(x):
    return [] if x is None else x


BUILTINS = {}


def bi(name):
    def deco(f):
        BUILTINS[name] = f
        return f
    return deco


@bi("+")
def _add(*a):
    r = 0
    for x in a:
        r = r + x
    return r


@bi("-")
def _sub(*a):
    if len(a) == 1:
        return -a[0]
    r = a[0]
    for x in a[1:]:
        r = r - x
    return r


@bi("*")
def _mul(*a):
    r = 1
    for x in a:
        r = r * x
    return r


@bi("/")
def _div(*a):
    if len(a) == 1:
        return 1 / a[0]
    r = a[0]
    for x in a[1:]:
        if isinstance(r, int) and isinstance(x, int):
            q = abs(r) // abs(x)
            r = q if (r >= 0) == (x >= 0) else -q
        else:
            r = r / x
    return r


@bi("1+")
def _inc(x):
    return x + 1


@bi("1-")
def _dec(x):
    return x - 1


@bi("=")
def _eq(*a):
    return TRUE if all(a[0] == x for x in a[1:]) else NIL


@bi("equal")
def _equal(a, b, fuzz=None):
    if fuzz is not None and isinstance(a, (int, float)) and isinstance(b, (int, float)):
        return TRUE if abs(a - b) <= fuzz else NIL
    return TRUE if a == b else NIL


@bi("logand")
def _logand(*a):
    r = a[0]
    for x in a[1:]:
        r = r & x
    return r


@bi("min")
def _min(*a):
    return min(a)


@bi("max")
def _max(*a):
    return max(a)


@bi("assoc")
def _assoc(key, lst):
    for item in as_list(lst):
        if isinstance(item, list) and item and item[0] == key:
            return item
    return NIL


@bi("subst")
def _subst(new, old, lst):
    if not isinstance(lst, list):
        return lst
    out = []
    for item in lst:
        if item == old:
            out.append(new)
        elif isinstance(item, list):
            out.append(_subst(new, old, item))
        else:
            out.append(item)
    return Dot(out) if isinstance(lst, Dot) else out


@bi("/=")
def _ne(*a):
    return TRUE if a[0] != a[1] else NIL


@bi("<")
def _lt(*a):
    return TRUE if all(a[i] < a[i + 1] for i in range(len(a) - 1)) else NIL


@bi(">")
def _gt(*a):
    return TRUE if all(a[i] > a[i + 1] for i in range(len(a) - 1)) else NIL


@bi("<=")
def _le(*a):
    return TRUE if all(a[i] <= a[i + 1] for i in range(len(a) - 1)) else NIL


@bi(">=")
def _ge(*a):
    return TRUE if all(a[i] >= a[i + 1] for i in range(len(a) - 1)) else NIL


@bi("rem")
def _rem(a, b):
    return a - b * (abs(a) // abs(b)) * (1 if (a >= 0) == (b >= 0) else -1)


@bi("fix")
def _fix(x):
    return int(math.trunc(x))


@bi("float")
def _float(x):
    return float(x)


@bi("abs")
def _abs(x):
    return abs(x)


@bi("sqrt")
def _sqrt(x):
    return math.sqrt(x)


@bi("expt")
def _expt(a, b):
    return a ** b


@bi("sin")
def _sin(x):
    return math.sin(x)


@bi("cos")
def _cos(x):
    return math.cos(x)


@bi("atan")
def _atan(x):
    return math.atan(x)


@bi("car")
def _car(x):
    if nil(x):
        return NIL
    return x[0]


@bi("cdr")
def _cdr(x):
    if isinstance(x, Dot):
        return x[1]
    if nil(x) or len(x) < 2:
        return NIL
    return x[1:]


@bi("cadr")
def _cadr(x):
    return _car(_cdr(x))


@bi("caddr")
def _caddr(x):
    return _car(_cdr(_cdr(x)))


@bi("cadddr")
def _cadddr(x):
    return _car(_cdr(_cdr(_cdr(x))))


@bi("nth")
def _nth(i, x):
    if nil(x) or i >= len(x):
        return NIL
    return x[i]


@bi("length")
def _length(x):
    return 0 if nil(x) else len(x)


@bi("list")
def _list(*a):
    return list(a)


@bi("cons")
def _cons(a, b):
    if isinstance(b, Dot):
        return Dot([a] + list(b))
    if not isinstance(b, list):
        return Dot([a, b])
    return [a] + as_list(b)


@bi("append")
def _append(*a):
    out = []
    for x in a:
        out += as_list(x)
    return out


@bi("reverse")
def _rev(x):
    return list(reversed(as_list(x)))


@bi("last")
def _last(x):
    return as_list(x)[-1] if not nil(x) else NIL


@bi("member")
def _member(a, x):
    x = as_list(x)
    for i, v in enumerate(x):
        if v == a:
            return x[i:]
    return NIL


@bi("null")
def _null(x):
    return TRUE if nil(x) else NIL


@bi("numberp")
def _numberp(x):
    return TRUE if isinstance(x, (int, float)) and not isinstance(x, bool) else NIL


@bi("mapcar")
def _mapcar(fn, *lists):
    name = str(fn).upper() if isinstance(fn, Sym) else str(fn)
    out = []
    for items in zip(*lists):
        if name in FUNCS:
            out.append(call(name, list(items)))
        else:
            bf = find_builtin(str(fn))
            out.append(bf(*items) if bf else NIL)
    return out


@bi("not")
def _not(x):
    return TRUE if nil(x) else NIL


@bi("distance")
def _distance(p1, p2):
    return math.sqrt(sum((a - b) ** 2 for a, b in zip(p1, p2)))


@bi("strcat")
def _strcat(*a):
    return "".join("" if x is None else str(x) for x in a)


@bi("strlen")
def _strlen(s):
    return len(s)


@bi("substr")
def _substr(s, start, length=None):
    if length is None:
        return s[start - 1:]
    return s[start - 1:start - 1 + length]


@bi("strcase")
def _strcase(s, mode=None):
    return s.upper() if mode is None else s.upper()


@bi("ascii")
def _ascii(s):
    return ord(s[0])


@bi("chr")
def _chr(n):
    return chr(n)


@bi("atof")
def _atof(s):
    try:
        return float(s)
    except ValueError:
        return 0.0


@bi("atoi")
def _atoi(s):
    try:
        return int(float(s))
    except ValueError:
        return 0


@bi("itoa")
def _itoa(n):
    return str(int(n))


@bi("rtos")
def _rtos(v, mode=2, prec=4):
    if mode in (2, None):
        return "%.*f" % (int(prec), v)
    return str(v)


@bi("wcmatch")
def _wcmatch(s, pat):
    for p in pat.split(","):
        rx = re.escape(p.strip()).replace(r"\*", ".*").replace(r"\?", ".")
        if re.fullmatch(rx, s, re.IGNORECASE):
            return TRUE
    return NIL


@bi("type")
def _type(x):
    if x is None:
        return Sym("NIL")
    if isinstance(x, bool):
        return Sym("SYM")
    if isinstance(x, int):
        return Sym("INT")
    if isinstance(x, float):
        return Sym("REAL")
    if isinstance(x, str) and not isinstance(x, Sym):
        return Sym("STR")
    if isinstance(x, list):
        return Sym("LIST")
    return Sym("SYMBOL")


# ---- AutoCAD stubs so the file can be loaded outside AutoCAD -----------
STUB_VARS = {"CMDECHO": 0, "TEXTSTYLE": "Standard", "AREA": 0.0, "PERIMETER": 0.0,
             "VIEWCTR": [0.0, 0.0, 0.0], "LWDISPLAY": 0, "SECURELOAD": 0,
             "APPAUTOLOAD": 14, "TRUSTEDPATHS": ""}
PRINTED = []


@bi("getvar")
def _getvar(name):
    return STUB_VARS.get(name.upper(), 0)


@bi("setvar")
def _setvar(name, val):
    STUB_VARS[name.upper()] = val
    return val


@bi("princ")
def _princ(*a):
    if a:
        PRINTED.append(a[0])
    return None


@bi("prompt")
def _prompt(s):
    PRINTED.append(s)
    return None


@bi("findfile")
def _findfile(f):
    return None


@bi("getenv")
def _getenv(n):
    return None


@bi("tblsearch")
def _tblsearch(t, n):
    return None


@bi("open")
def _open(*a):
    return None


@bi("close")
def _close(f):
    return None


for _name in ("entmake", "entget", "entmod", "entupd", "entlast", "entnext",
              "ssget", "ssname", "sslength", "ssadd", "command", "apply_stub",
              "vl-load-com", "vlax-ename->vla-object", "vlax-variant-value",
              "vlax-safearray->list", "vla-get-Area", "vla-get-Perimeter",
              "vla-GetBoundingBox", "read-line", "write-line", "initget",
              "getpoint", "getkword", "getint", "getreal", "getstring",
              "vl-catch-all-apply", "vl-catch-all-error-p"):
    BUILTINS.setdefault(_name, (lambda *a, **k: None))


def apply_builtin(name, args):
    if name == "apply":
        fn = args[0]
        rest = args[1] if len(args) > 1 else []
        if isinstance(fn, Sym) or isinstance(fn, str):
            if str(fn).upper() in BUILTINS:
                return BUILTINS[str(fn).upper()](*as_list(rest))
            return None
        return None
    raise LispError("no builtin " + name)


def find_builtin(name):
    for cand in (name, name.upper(), name.lower()):
        if cand in BUILTINS:
            return BUILTINS[cand]
    return None


class Env(dict):
    pass


class LocalEnv(Env):
    """function frame: setq of a name that is not a parameter/local writes
    through to the global environment (AutoLISP semantics)."""

    def __init__(self, base):
        super().__init__(base)
        self.base = base
        self.owned = set()

    def __setitem__(self, key, value):
        if key not in self.owned:
            self.base[key] = value
        super().__setitem__(key, value)


def evaluate(form, env):
    if isinstance(form, Sym):
        u = str(form).upper()
        if u == "T":
            return TRUE
        if u == "NIL":
            return NIL
        if u in env:
            return env[u]
        return NIL
    if not isinstance(form, list):
        return form
    if not form:
        return NIL
    head = form[0]
    if isinstance(head, Sym):
        h = str(head).upper()
        if h == "QUOTE":
            return form[1]
        if h == "SETQ":
            i = 1
            val = NIL
            while i < len(form) - 1:
                env[str(form[i]).upper()] = evaluate(form[i + 1], env)
                val = env[str(form[i]).upper()]
                i += 2
            return val
        if h == "DEFUN":
            name = str(form[1]).upper()
            params = as_list(form[2])
            argv, locals_ = [], []
            seen = False
            for p in params:
                if isinstance(p, Sym) and str(p) == "/":
                    seen = True
                    continue
                (locals_ if seen else argv).append(str(p).upper())
            body = form[3:]
            fn = (name, argv, locals_, body, env)
            FUNCS[name] = fn
            return head
        if h == "IF":
            if truthy(evaluate(form[1], env)):
                return evaluate(form[2], env) if len(form) > 2 else NIL
            return evaluate(form[3], env) if len(form) > 3 else NIL
        if h == "COND":
            for clause in form[1:]:
                if truthy(evaluate(clause[0], env)):
                    val = NIL
                    for b in clause[1:]:
                        val = evaluate(b, env)
                    return val
            return NIL
        if h == "PROGN":
            val = NIL
            for b in form[1:]:
                val = evaluate(b, env)
            return val
        if h == "AND":
            val = TRUE
            for b in form[1:]:
                val = evaluate(b, env)
                if nil(val):
                    return NIL
            return val
        if h == "OR":
            for b in form[1:]:
                val = evaluate(b, env)
                if truthy(val):
                    return val
            return NIL
        if h == "WHILE":
            val = NIL
            guard = 0
            while truthy(evaluate(form[1], env)):
                for b in form[2:]:
                    val = evaluate(b, env)
                guard += 1
                if guard > 2000000:
                    raise LispError("while loop guard")
            return val
        if h == "FOREACH":
            var = str(form[1]).upper()
            lst = as_list(evaluate(form[2], env))
            val = NIL
            for item in lst:
                env[var] = item
                for b in form[3:]:
                    val = evaluate(b, env)
            return val
        if h == "APPLY":
            fn = evaluate(form[1], env)
            args = evaluate(form[2], env)
            if isinstance(fn, Sym):
                if str(fn).upper() in FUNCS:
                    return call(str(fn).upper(), as_list(args))
                bf = find_builtin(str(fn))
                if bf:
                    return bf(*as_list(args))
            return NIL
        # normal function call
        if h in FUNCS:
            args = [evaluate(a, env) for a in form[1:]]
            return call(h, args)
        bf = find_builtin(str(head))
        if bf:
            args = [evaluate(a, env) for a in form[1:]]
            return bf(*args)
        raise LispError("no function definition: " + str(head))
    raise LispError("bad head")


FUNCS = {}


def call(name, args):
    _, argv, locals_, body, env = FUNCS[name]
    local = LocalEnv(env)
    local.owned = set(argv) | set(locals_)
    for i, p in enumerate(argv):
        local[p] = args[i] if i < len(args) else NIL
    for l in locals_:
        local[l] = NIL
    val = NIL
    for b in body:
        val = evaluate(b, local)
    return val


def run(src):
    forms = parse(tokenize(src))
    env = Env()
    env["PI"] = math.pi          # AutoLISP built-in constant
    for f in forms:
        evaluate(f, env)
    return env


# ------------------------------------------------------------------ tests
def close(a, b, tol=1e-6):
    return abs(a - b) <= tol


def main():
    path = sys.argv[1]
    src = open(path, encoding="utf-8-sig").read().replace("\r\n", "\n")
    env = run(src)
    failures = []

    def check(label, ok, detail=""):
        print(("  PASS  " if ok else "  FAIL  ") + label + (("   " + detail) if detail else ""))
        if not ok:
            failures.append(label)

    def ev(expr):
        return evaluate(parse(tokenize(expr))[0], env)

    print("loaded %s : %d functions" % (path, len(FUNCS)))

    # 1) 100 x 100 square
    m = ev("(mj:poly-metrics (list (list '(0 0 0) 0.0) (list '(100 0 0) 0.0)"
           " (list '(100 100 0) 0.0) (list '(0 100 0) 0.0)) T)")
    check("square area 10000", close(m[0], 10000.0, 1e-6), "got %r" % m[0])
    check("square perimeter 400", close(m[1], 400.0, 1e-6), "got %r" % m[1])
    check("square centroid 50,50", close(m[2], 50.0) and close(m[3], 50.0),
          "got %r,%r" % (m[2], m[3]))

    # 2) circle R50 built from two bulged edges of length 100
    m = ev("(mj:poly-metrics (list (list '(0 0 0) 1.0) (list '(100 0 0) 1.0)) T)")
    check("R50 circle area pi*r^2", close(m[0], math.pi * 2500, 1e-6), "got %.6f" % m[0])
    check("R50 circle perimeter", close(m[1], 2 * math.pi * 50, 1e-6), "got %.6f" % m[1])
    check("R50 circle centroid 50,0", close(m[2], 50.0, 1e-9) and close(m[3], 0.0, 1e-9),
          "got %r,%r" % (m[2], m[3]))

    # 3) square with one bulged edge (semicircle outside)
    m = ev("(mj:poly-metrics (list (list '(0 0 0) 1.0) (list '(100 0 0) 0.0)"
           " (list '(100 100 0) 0.0) (list '(0 100 0) 0.0)) T)")
    check("bulged square area 10000+half disc",
          close(m[0], 10000 + math.pi * 2500 / 2, 1e-6), "got %.6f" % m[0])
    check("bulged square perimeter 300+pi*50",
          close(m[1], 300 + math.pi * 50, 1e-6), "got %.6f" % m[1])

    # 4) open polyline -> area as if closed, perimeter of the open path
    m = ev("(mj:poly-metrics (list (list '(0 0 0) 0.0) (list '(100 0 0) 0.0)"
           " (list '(100 100 0) 0.0)) nil)")
    check("open polyline area 5000", close(m[0], 5000.0, 1e-6), "got %r" % m[0])
    check("open polyline perimeter 200", close(m[1], 200.0, 1e-6), "got %r" % m[1])

    # 5) arc direction: negative bulge removes half disc
    m = ev("(mj:poly-metrics (list (list '(0 0 0) -1.0) (list '(100 0 0) 0.0)"
           " (list '(100 100 0) 0.0) (list '(0 100 0) 0.0)) T)")
    check("negative bulge area 10000-half disc",
          close(m[0], 10000 - math.pi * 2500 / 2, 1e-6), "got %.6f" % m[0])

    # 6) L shaped polygon (area 10000-2500 = 7500)
    m = ev("(mj:poly-metrics (list (list '(0 0 0) 0.0) (list '(100 0 0) 0.0)"
           " (list '(100 50 0) 0.0) (list '(50 50 0) 0.0) (list '(50 100 0) 0.0)"
           " (list '(0 100 0) 0.0)) T)")
    check("L shape area 7500", close(m[0], 7500.0, 1e-6), "got %r" % m[0])

    # 7) \U+ escape round trip
    s = ev('(mj:esc "面积㎡")')
    check("escape is ascii", all(ord(c) < 128 for c in s), "got %r" % s)
    back = ev('(mj:unesc "%s")' % s.replace("\\", "\\\\"))
    check("escape round trip", back == "面积㎡", "got %r" % back)

    # 8) unit conversion + label
    env["*MJ-CFG*"] = None
    ev("(mj:init)")
    ev("(mj:set 'unit \"mm\")")
    ev("(mj:set 'dec 2)")
    txt = ev("(mj:label-text 12340000.0 45678.0)")
    check("label mm->m2/m", txt == "面积 12.34㎡\\P周长 45.68m", "got %r" % txt)
    ev("(mj:set 'unit \"none\")")
    txt = ev("(mj:label-text 1234.0 56.0)")
    check("label raw units", txt == "面积 1234.00\\P周长 56.00", "got %r" % txt)

    # 9) ini round trip of a value with CHINESE + spaces
    ev("(mj:set 'unit \"mm\")")
    v = ev("(mj:esc \"MJ-面积 标注\")")
    check("escaped ini value ascii", all(ord(c) < 128 for c in v), repr(v))
    back = ev('(mj:unesc "%s")' % v.replace("\\", "\\\\"))
    check("ini value round trip", back == "MJ-面积 标注", repr(back))

    # 10) ellipse perimeter sanity (a=b -> circle)
    p = ev("(mj:ellipse-perim 100.0 100.0)")
    check("ellipse a=b == circle", close(p, 2 * math.pi * 100, 1e-6), "got %.6f" % p)

    # 11) assemble a square out of 4 loose line segments (scrambled + one reversed)
    segs = ("(list (list '(100 0 0) '(0 0 0) 0.0) (list '(0 100 0) '(0 0 0) 0.0)"
            " (list '(100 100 0) '(100 0 0) 0.0) (list '(100 100 0) '(0 100 0) 0.0))")
    v = ev("(mj:assemble %s 1e-6)" % segs)
    check("assemble square: loop found", isinstance(v, list) and len(v) == 4,
          "got %r" % (v,))
    mm = ev("(mj:poly-metrics (mj:assemble %s 1e-6) T)" % segs)
    check("assemble square: area 10000", close(mm[0], 10000.0), "got %r" % mm[0])
    check("assemble square: perimeter 400", close(mm[1], 400.0), "got %r" % mm[1])
    check("assemble square: centroid 50,50",
          close(mm[2], 50.0) and close(mm[3], 50.0), "got %r,%r" % (mm[2], mm[3]))

    # 12) line + arc (bulge) = half disc
    segs2 = ("(list (list '(0 0 0) '(100 0 0) 0.0) (list '(100 0 0) '(0 0 0) 1.0))")
    mm = ev("(mj:poly-metrics (mj:assemble %s 1e-6) T)" % segs2)
    check("assemble line+arc: area half disc",
          close(mm[0], math.pi * 2500 / 2, 1e-6), "got %.6f" % mm[0])
    check("assemble line+arc: perimeter",
          close(mm[1], 100 + math.pi * 50, 1e-6), "got %.6f" % mm[1])

    # 13) a gap must NOT assemble
    segs3 = ("(list (list '(0 0 0) '(100 0 0) 0.0) (list '(100 0 0) '(100 100 0) 0.0)"
             " (list '(100 100 0) '(0 100 0) 0.0))")
    v = ev("(mj:assemble %s 1e-6)" % segs3)
    check("assemble: open chain rejected", v is None, "got %r" % (v,))

    # 14) tolerance: a 0.3 mm gap still closes with a 0.5 mm tolerance
    segs4 = ("(list (list '(0 0 0) '(100 0 0) 0.0) (list '(100 0.3 0) '(100 100 0) 0.0)"
             " (list '(100 100 0) '(0 100 0) 0.0) (list '(0 100 0) '(0 0 0) 0.0))")
    v = ev("(mj:assemble %s 0.5)" % segs4)
    check("assemble: small gap closed by tolerance", isinstance(v, list) and len(v) == 4,
          "got %r" % (v,))

    # 15) point in polygon + inside point fallback
    sq = "(list (list '(0 0 0) 0.0) (list '(100 0 0) 0.0) (list '(100 100 0) 0.0) (list '(0 100 0) 0.0))"
    check("inside-p: inside", ev("(mj:inside-p '(50 50 0) %s)" % sq) is not None)
    check("inside-p: outside", ev("(mj:inside-p '(150 50 0) %s)" % sq) is None)
    mm = ev("(mj:metrics-from-verts %s T)" % sq)
    env["MM"] = mm
    pt = ev("(mj:inside-point MM)")
    check("inside-point: centre of square", close(pt[0], 50.0) and close(pt[1], 50.0),
          "got %r" % (pt,))
    # C shape: area centroid falls outside -> fallback must find a point inside
    cshape = ("(list (list '(0 0 0) 0.0) (list '(100 0 0) 0.0) (list '(100 30 0) 0.0)"
              " (list '(30 30 0) 0.0) (list '(30 70 0) 0.0) (list '(100 70 0) 0.0)"
              " (list '(100 100 0) 0.0) (list '(0 100 0) 0.0))")
    mm = ev("(mj:metrics-from-verts %s T)" % cshape)
    env["MM"] = mm
    pt = ev("(mj:inside-point MM)")
    inside_ok = ev("(mj:inside-p (list %.6f %.6f 0.0) %s)" % (pt[0], pt[1], cshape))
    check("inside-point: C shape fallback really inside", inside_ok is not None,
          "point=%r" % (pt,))

    # 16) type safety of the helpers that read system variables
    check("num->str: string passthrough", ev('(mj:num->str "C:/x/y")') == "C:/x/y")
    check("num->str: nil -> empty", ev("(mj:num->str nil)") == "")
    check("num->str: int", ev("(mj:num->str 12)") == "12")
    check("sysvar of a string sysvar does not blow up",
          ev('(mj:sysvar "TRUSTEDPATHS")') == "")
    check("sysint of an int sysvar", ev('(mj:sysint "SECURELOAD")') == 0)

    # 17) file name sanitising for the batch plot
    check("safe-name keeps normal text", ev('(mj:safe-name "布局1-平面图")') == "布局1-平面图")
    check("safe-name replaces slashes",
          "/" not in ev('(mj:safe-name "a/b:c*d?e")') and ":" not in ev('(mj:safe-name "a/b:c*d?e")'),
          repr(ev('(mj:safe-name "a/b:c*d?e")')))
    check("drawing-name strips .dwg",
          ev("(mj:safe-name \"plan.dwg\")") == "plan.dwg")   # helper is separate
    check("plotdev default", ev("(mj:get 'plotdev)") == "DWG To PDF.pc3")

    # 18) frame detection helpers
    segs_sq = ("(list (list '(0 0 0) '(100 0 0) 0.0) (list '(100 0 0) '(100 100 0) 0.0)"
               " (list '(100 100 0) '(0 100 0) 0.0) (list '(0 100 0) '(0 0 0) 0.0))")
    groups = ev("(mj:group-segs %s 0.1)" % segs_sq)
    check("group-segs: one square -> one group", isinstance(groups, list) and len(groups) == 1,
          "got %r" % (len(groups) if groups else None,))
    segs_two = ("(append %s (list (list '(500 0 0) '(600 0 0) 0.0)"
                " (list '(600 0 0) '(600 100 0) 0.0) (list '(600 100 0) '(500 100 0) 0.0)"
                " (list '(500 100 0) '(500 0 0) 0.0)))" % segs_sq)
    groups = ev("(mj:group-segs %s 0.1)" % segs_two)
    check("group-segs: two squares -> two groups", isinstance(groups, list) and len(groups) == 2,
          "got %r" % (len(groups) if groups else None,))

    # 19) frame sorting
    fr = "(list (list 300 0 400 50) (list 0 0 100 50) (list 100 0 200 50))"
    s = ev("(mj:frame-sort %s \"1\")" % fr)
    check("frame-sort: left to right",
          [f[0] for f in s] == [0, 100, 300], "got %r" % ([f[0] for f in s],))
    fr2 = ("(list (list 0 0 100 50) (list 300 0 400 50) (list 0 300 100 350)"
           " (list 300 300 400 350))")
    s = ev("(mj:frame-sort %s \"2\")" % fr2)
    check("frame-sort: top row first",
          [f[1] for f in s] == [300, 300, 0, 0], "got %r" % ([f[1] for f in s],))

    # 20) mtext cleaning for file names
    check("clean-text plain", ev('(mj:clean-text "平面图")') == "平面图")
    check("clean-text drops braces", ev('(mj:clean-text "A{xx}B")') == "AB")
    check("row-of: same band for one row", ev("(mj:row-of (list 0 0 100 50) 50.0)")
          == ev("(mj:row-of (list 300 0 400 50) 50.0)"))
    check("row-of: different band for next row",
          ev("(mj:row-of (list 0 0 100 50) 50.0)") != ev("(mj:row-of (list 0 300 100 350) 50.0)"),
          "%r vs %r" % (ev("(mj:row-of (list 0 0 100 50) 50.0)"),
                        ev("(mj:row-of (list 0 300 100 350) 50.0)")))

    print()
    if failures:
        print("FAILED: %d test(s): %s" % (len(failures), ", ".join(failures)))
        return 1
    print("ALL TESTS PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
