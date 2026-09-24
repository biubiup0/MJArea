#!/usr/bin/env python3
"""
Build MJArea.lsp from MJArea.src.lsp.

Two output formats:

  utf8 (default)
      Exactly what Autodesk itself ships for AutoCAD for Mac in the
      zh_CN resources: UTF-8 **with BOM**, CRLF line endings and real
      Chinese characters.  This is what the Mac AutoLISP engine reads.

  ascii
      Pure ASCII, every non ASCII character inside a string becomes a
      \\U+xxxx escape (the format AutoCAD for Windows uses).  Keep it as
      a reference / for Windows machines.

Both modes also
  * reject non ASCII characters outside string literals
    (so comments stay English),
  * re-wrap any line longer than MAXLEN characters at a safe point,
  * parse the result to prove parentheses and strings are balanced.

Usage: python3 build.py <src> <out> [utf8|ascii]
"""
import sys

MAXLEN = 190          # Autodesk's own Mac LSP files stay under ~190 chars


class BuildError(Exception):
    pass


def scan_lines(src):
    """yield (line_text, depth_at_line_start, in_string_at_line_start)"""
    depth = 0
    in_str = False
    i = 0
    n = len(src)
    start = 0
    line_depth = 0
    line_instr = False
    while i < n:
        c = src[i]
        if in_str:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_str = False
            i += 1
            continue
        if c == ";":
            j = src.find("\n", i)
            i = n if j < 0 else j
            continue
        if c == '"':
            in_str = True
            i += 1
            continue
        if c == "(":
            depth += 1
        elif c == ")":
            depth -= 1
            if depth < 0:
                raise BuildError("unexpected ) near offset %d" % i)
        if c == "\n":
            yield src[start:i], line_depth, line_instr
            line_depth = depth
            line_instr = in_str
            start = i + 1
        i += 1
    yield src[start:], line_depth, line_instr
    if depth != 0:
        raise BuildError("unbalanced parentheses: %d open" % depth)
    if in_str:
        raise BuildError("unterminated string literal")


def wrap_line(line, depth):
    """break a long line between sibling expressions (depth stays the same)"""
    if len(line) <= MAXLEN:
        return [line]
    indent = line[:len(line) - len(line.lstrip(" "))]
    pieces = []
    cur = ""
    d = depth
    in_str = False
    i = 0
    while i < len(line):
        c = line[i]
        if in_str:
            cur += c
            if c == "\\":
                cur += line[i + 1:i + 2]
                i += 2
                continue
            if c == '"':
                in_str = False
            i += 1
            continue
        if c == '"':
            in_str = True
            cur += c
            i += 1
            continue
        cur += c
        if c == "(":
            d += 1
        elif c == ")":
            d -= 1
            # a safe place to break: just closed a form, back at start depth,
            # and the rest of the line is long enough to be worth splitting
            rest = line[i + 1:]
            if d == depth and len(cur) > 60 and len(indent) + len(rest) > 40:
                pieces.append(cur)
                cur = indent
        i += 1
    pieces.append(cur)
    # fall back to hard wrapping in the pathological case
    out = []
    for p in pieces:
        while len(p) > MAXLEN:
            out.append(p[:MAXLEN])
            p = indent + p[MAXLEN:]
        out.append(p)
    return out


def convert_non_ascii(src, mode):
    """mode 'ascii': escape non ASCII inside strings, forbid it elsewhere
       mode 'utf8' : forbid nothing inside strings, forbid it in comments"""
    out = []
    i = 0
    n = len(src)
    line = 1
    while i < n:
        c = src[i]
        if c == "\n":
            line += 1
            out.append(c)
            i += 1
            continue
        if c == ";":
            j = src.find("\n", i)
            if j < 0:
                j = n
            seg = src[i:j]
            for ch in seg:
                if ord(ch) > 127:
                    raise BuildError("non-ASCII character in a comment (line %d)" % line)
            out.append(seg)
            i = j
            continue
        if c == '"':
            j = i + 1
            buf = ['"']
            while j < n:
                ch = src[j]
                if ch == "\\":
                    buf.append(src[j:j + 2])
                    j += 2
                    continue
                if ch == '"':
                    buf.append('"')
                    j += 1
                    break
                if ord(ch) > 127:
                    if mode == "ascii":
                        buf.append("\\U+%04X" % ord(ch))
                    else:
                        buf.append(ch)
                else:
                    buf.append(ch)
                j += 1
            else:
                raise BuildError("unterminated string literal (line %d)" % line)
            out.append("".join(buf))
            i = j
            continue
        if ord(c) > 127:
            raise BuildError("non-ASCII character outside a string (line %d)" % line)
        out.append(c)
        i += 1
    return "".join(out)


# ---------------------------------------------------------------- checking
def tokenize(src):
    toks = []
    i = 0
    n = len(src)
    ln = 1
    while i < n:
        c = src[i]
        if c == "\n":
            ln += 1
            i += 1
            continue
        if c.isspace() or c == "\ufeff":
            i += 1
            continue
        if c == ";":
            j = src.find("\n", i)
            i = n if j < 0 else j
            continue
        if c in "()'":
            toks.append((c, ln))
            i += 1
            continue
        if c == '"':
            j = i + 1
            while j < n:
                if src[j] == "\\":
                    j += 2
                    continue
                if src[j] == '"':
                    break
                j += 1
            if j >= n:
                raise BuildError("unterminated string (line %d)" % ln)
            toks.append(("str", ln))
            i = j + 1
            continue
        j = i
        while j < n and (not src[j].isspace()) and src[j] not in '();"':
            j += 1
        toks.append(("atom", ln))
        i = j
    return toks


def check_balance(toks):
    depth = 0
    stack = []
    for t, ln in toks:
        if t == "(":
            depth += 1
            stack.append(ln)
        elif t == ")":
            depth -= 1
            if depth < 0:
                raise BuildError("unexpected ) on line %d" % ln)
            stack.pop()
    if depth != 0:
        raise BuildError("unbalanced parentheses: %d block(s) open, started line %s"
                         % (depth, stack[-5:]))


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 2
    src_path, out_path = sys.argv[1], sys.argv[2]
    mode = sys.argv[3] if len(sys.argv) > 3 else "utf8"
    if mode not in ("utf8", "ascii"):
        raise BuildError("mode must be utf8 or ascii")

    src = open(src_path, encoding="utf-8").read().replace("\r\n", "\n")
    src = convert_non_ascii(src, mode)

    lines = []
    for line, depth, _instr in scan_lines(src):
        lines.extend(wrap_line(line, depth))
    text = "\n".join(lines)

    toks = tokenize(text)
    check_balance(toks)

    if mode == "ascii":
        bad = [c for c in text if ord(c) > 127]
        if bad:
            raise BuildError("output is not ASCII")
        data = text.encode("ascii")
    else:
        data = b"\xef\xbb\xbf" + text.encode("utf-8")

    data = data.replace(b"\n", b"\r\n")          # CRLF, like Autodesk's files
    with open(out_path, "wb") as f:
        f.write(data)

    longest = max((len(l) for l in lines), default=0)
    print("OK  %s -> %s   mode=%s  %d bytes  %d lines  %d tokens  longest line %d"
          % (src_path, out_path, mode, len(data), len(lines), len(toks), longest))


if __name__ == "__main__":
    main()
