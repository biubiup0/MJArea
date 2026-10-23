#!/usr/bin/env python3
"""
Static sanity check for an AutoLISP file:
  * every called function is either defined in the file or a known built-in
  * every symbol that is read is a parameter, a local, a global that gets
    set somewhere, or a built-in (catches typos such as mj:smthing)

Usage: python3 lint.py <file.lsp>
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import lisp_test as L          # reuse the reader

SPECIALS = {"QUOTE", "SETQ", "DEFUN", "IF", "COND", "PROGN", "AND", "OR",
            "WHILE", "FOREACH", "APPLY", "LAMBDA", "REPEAT", "FUNCTION"}

BUILTINS = set("""
+ - * / 1+ 1- = /= < > <= >= abs and append apply ascii assoc atan atof atoi
car cdr cadr caddr cadddr caar cddr chr close command cond cons distance
entdel entget entlast entmake entmod entnext entupd eq equal expt findfile
fix float foreach getenv getint getkword getpoint getreal getstring getvar
if initget itoa last layoutlist length list logand mapcar max member min nth null not
open or numberp pi polar princ print prin1 prompt read-line rem repeat reverse rtos setq
setvar sin cos sqrt tan ssadd ssget sslength ssname startapp strcase strcat strlen subst
substr tblnext tblobjname tblsearch entmakex namedobjdict dictsearch dictadd dictremove dictnext namedobjdict entget entmod entdel trans type ver getfiled prog1 vl-directory-files vl-file-size vl-mkdir vla-getboundingbox vla-get-area vla-get-perimeter
vl-catch-all-apply vl-catch-all-error-p vl-load-com vlax-add-cmd
vlax-ename->vla-object vlax-safearray->list vlax-variant-value wcmatch write-line
""".split())


def collect_defuns(forms, out):
    for f in forms:
        if isinstance(f, list) and f and isinstance(f[0], L.Sym) \
                and str(f[0]).upper() == "DEFUN":
            out.add(str(f[1]).upper())
            for sub in f[3:]:
                collect_defuns([sub], out)


def scan(form, ctx):
    if isinstance(form, L.Sym):
        u = str(form).upper()
        if u not in ("T", "NIL"):
            ctx["read"].add(u)
        return
    if not isinstance(form, list) or not form:
        return
    head = form[0]
    if isinstance(head, L.Sym):
        h = str(head).upper()
        if h == "QUOTE" or h == "FUNCTION":
            return                                  # literal data, not code
        if h == "DEFUN":
            params = form[2] if len(form) > 2 else []
            if isinstance(params, list):
                for p in params:
                    if isinstance(p, L.Sym) and str(p) != "/":
                        ctx["params"].add(str(p).upper())
            for b in form[3:]:
                scan(b, ctx)
            return
        if h == "SETQ":
            i = 1
            while i < len(form) - 1:
                ctx["setq"].add(str(form[i]).upper())
                scan(form[i + 1], ctx)
                i += 2
            return
        if h == "FOREACH":
            ctx["params"].add(str(form[1]).upper())     # the loop variable
            scan(form[2], ctx)
            for b in form[3:]:
                scan(b, ctx)
            return
        if h == "COND":
            for clause in form[1:]:
                if not isinstance(clause, list) or not clause:
                    continue
                if isinstance(clause[0], L.Sym):
                    # a bare symbol test (like the common "t" clause)
                    ctx["read"].add(str(clause[0]).upper())
                else:
                    scan(clause[0], ctx)
                for b in clause[1:]:
                    scan(b, ctx)
            return
        if h in SPECIALS:
            for b in form[1:]:
                scan(b, ctx)
            return
        ctx["calls"].add(h)
        for a in form[1:]:
            scan(a, ctx)
        return
    for a in form:
        scan(a, ctx)


def main():
    path = sys.argv[1]
    src = open(path, encoding="utf-8-sig").read().replace("\r\n", "\n")
    forms = L.parse(L.tokenize(src))

    # ---- argument-count checks: these are "syntax errors" at run time ----
    arity = []

    def check_arity(form):
        if isinstance(form, list) and form and isinstance(form[0], L.Sym):
            h = str(form[0]).upper()
            n = len(form) - 1
            if h == "IF" and n not in (2, 3):
                arity.append(("if takes 2 or 3 arguments, found %d" % n, form))
            if h == "SETQ" and n % 2 != 0:
                arity.append(("setq needs an even number of arguments, found %d" % n, form))
        if isinstance(form, list):
            for f in form:
                check_arity(f)

    for f in forms:
        check_arity(f)

    defined = set()
    collect_defuns(forms, defined)

    ctx = {"calls": set(), "params": set(), "setq": set(), "read": set()}
    for f in forms:
        scan(f, ctx)

    known_fns = defined | {b.upper() for b in BUILTINS} | SPECIALS | {"T"}
    unknown_calls = sorted(c for c in ctx["calls"] if c not in known_fns)

    known_vars = (ctx["params"] | ctx["setq"] | {b.upper() for b in BUILTINS}
                  | SPECIALS | {"T", "NIL", "PI"})
    unknown_vars = [s for s in sorted(ctx["read"])
                    if s not in known_vars and s not in defined]

    print("file              : %s" % path)
    print("defined functions : %d" % len(defined))
    print("distinct calls    : %d" % len(ctx["calls"]))
    print("globals set       : %d" % len(ctx["setq"]))
    if arity:
        print("!! WRONG ARGUMENT COUNTS (AutoLISP reports these as syntax errors):")
        for msg, form in arity:
            print("   -", msg)
    else:
        print("arg counts        : ok")
    if unknown_calls:
        print("!! UNKNOWN FUNCTION CALLS:")
        for u in unknown_calls:
            print("   -", u.lower())
    else:
        print("unknown calls     : none")
    if unknown_vars:
        print("!! SYMBOLS READ BUT NEVER SET (typos?):")
        for u in unknown_vars:
            print("   -", u.lower())
    else:
        print("unset symbols     : none")
    return 1 if (unknown_calls or unknown_vars) else 0


if __name__ == "__main__":
    sys.exit(main())
