;;; ============================================================
;;;  MJArea  -  area / perimeter annotation with red thick outline
;;;  Platform : AutoCAD for Mac (also runs on AutoCAD for Windows)
;;;  Language : pure AutoLISP + native command calls
;;;             no DCL, no .NET, no ObjectARX  -> safe on macOS
;;;
;;;  Commands : MJ  MJA  MJSET  MJCLR  MJINFO  MJHELP
;;;             (no LI command on purpose: LI stays the native LIST alias)
;;;
;;;  NOTE FOR MAINTAINERS
;;;    This is the source file.  build.py turns every non ASCII
;;;    character inside a string into a \U+xxxx escape, so the
;;;    shipped MJArea.lsp is pure ASCII and can never be garbled
;;;    by a code page mismatch.
;;;    Rule: comments in English, Chinese text only inside strings.
;;; ============================================================

(setq *mj-ver* "6.1")

;; ------------------------------------------------------------
;; globals
;; ------------------------------------------------------------
(setq *mj-cfg*    nil)      ; settings, assoc list
(setq *mj-catch*  nil)      ; is vl-catch-all-apply available
(setq *mj-com*    nil)      ; is ActiveX (vl-load-com) usable
(setq *mj-ini*    nil)      ; settings file path
(setq *mj-loaded* nil)      ; banner already printed
(setq *mj-last-h* 0.0)      ; text height used by the last label
(setq *mj-warned* nil)      ; font warning already printed
(setq *mj-sum-n*  0)
(setq *mj-sum-a*  0.0)
(setq *mj-sum-p*  0.0)
(setq *mj-echo*   nil)      ; CMDECHO value saved by mj:on
(setq *mj-olderr* nil)      ; previous *error* handler
(setq *mj-trusted* nil)     ; trusted-paths check already done

;; ------------------------------------------------------------
;; string helpers
;; ------------------------------------------------------------

(defun mj:split (str delim / out pos len dlen start)
  (setq out '() pos 1 len (strlen str) dlen (strlen delim))
  (if (= len 0)
    (setq out '())
    (progn
      (while (<= pos len)
        (setq start pos)
        (while (and (<= pos len) (not (= (substr str pos dlen) delim)))
          (setq pos (1+ pos))
        )
        (setq out (cons (substr str start (- pos start)) out))
        (setq pos (+ pos dlen))
      )
      (setq out (reverse out))
    )
  )
  out
)

(defun mj:join (lst delim / out i)
  (setq out "" i 1)
  (foreach s lst
    (if (= i 1) (setq out s) (setq out (strcat out delim s)))
    (setq i (1+ i))
  )
  out
)

(defun mj:replace (str a b / out pos len alen)
  (if (or (null a) (= a ""))
    str
    (progn
      (setq out "" pos 1 len (strlen str) alen (strlen a))
      (while (<= pos len)
        (if (and (<= (+ pos alen -1) len) (= (substr str pos alen) a))
          (progn (setq out (strcat out b)) (setq pos (+ pos alen)))
          (progn (setq out (strcat out (substr str pos 1))) (setq pos (1+ pos)))
        )
      )
      out
    )
  )
)

(defun mj:starts (s p)
  (and (>= (strlen s) (strlen p)) (= (substr s 1 (strlen p)) p))
)

(defun mj:hex4 (n / s d h)
  (setq h "0123456789ABCDEF" s "")
  (while (> n 0)
    (setq d (rem n 16))
    (setq s (strcat (substr h (1+ d) 1) s))
    (setq n (/ (- n d) 16))
  )
  (while (< (strlen s) 4) (setq s (strcat "0" s)))
  s
)

(defun mj:hexval (c / h i r)
  (setq h "0123456789ABCDEF" i 1 r nil)
  (while (and (<= i 16) (null r))
    (if (= (strcase c) (substr h i 1)) (setq r (1- i)))
    (setq i (1+ i))
  )
  r
)

;; NOTE: the Mac AutoLISP reader chokes on a backslash escape inside a
;; string (\\ ), so every literal backslash is built with (chr 92).
(defun mj:bs ()
  (chr 92)
)

;; encode non ASCII characters as \U+xxxx so the ini file stays ASCII
(defun mj:esc (s / i c a out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq c (substr s i 1) a (ascii c))
    (if (> a 127)
      (setq out (strcat out (mj:bs) "U+" (mj:hex4 a)))
      (setq out (strcat out c))
    )
    (setq i (1+ i))
  )
  out
)

;; decode \U+xxxx sequences
(defun mj:unesc (s / len pos out ok code n i d)
  (setq len (strlen s) pos 1 out "")
  (while (<= pos len)
    (if (and (<= (+ pos 6) len) (= (substr s pos 3) (strcat (mj:bs) "U+")))
      (progn
        (setq code (substr s (+ pos 3) 4) ok t n 0 i 1)
        (while (<= i 4)
          (setq d (mj:hexval (substr code i 1)))
          (if (null d) (setq ok nil) (setq n (+ (* n 16) d)))
          (setq i (1+ i))
        )
        (if ok
          (progn (setq out (strcat out (chr n))) (setq pos (+ pos 7)))
          (progn (setq out (strcat out (substr s pos 1))) (setq pos (1+ pos)))
        )
      )
      (progn
        (setq out (strcat out (substr s pos 1)))
        (setq pos (1+ pos))
      )
    )
  )
  out
)

;; ------------------------------------------------------------
;; small utilities
;; ------------------------------------------------------------

(defun mj:putd (lst code val / hit)
  (setq hit (assoc code lst))
  (if hit
    (subst (cons code val) hit lst)
    (append lst (list (cons code val)))
  )
)

(defun mj:putassoc (lst k v / hit)
  (setq hit (assoc k lst))
  (if hit
    (subst (cons k v) hit lst)
    (append lst (list (cons k v)))
  )
)

(defun mj:dxf0 (e / d)
  (setq d (entget e))
  (cdr (assoc 0 d))
)

(defun mj:dirname (path / i)
  (setq i (strlen path))
  (while (and (> i 0) (/= (substr path i 1) "/"))
    (setq i (1- i))
  )
  (if (> i 1) (substr path 1 (1- i)) nil)
)

;; value -> string, safely (strings pass through, nil becomes empty)
(defun mj:num->str (v)
  (cond
    ((null v) "")
    ((= (type v) 'INT) (itoa v))
    ((= (type v) 'REAL) (rtos v 2 6))
    ((= (type v) 'STR) v)
    (t "")
  )
)

;; system variable as an integer, whatever type getvar hands back
(defun mj:sysint (name / v)
  (setq v (if *mj-catch*
            (vl-catch-all-apply 'getvar (list name))
            (getvar name)
          )
  )
  (cond
    ((null v) 0)
    ((= (type v) 'INT) v)
    ((= (type v) 'REAL) (fix v))
    ((= (type v) 'STR) (fix (atof v)))
    (t 0)
  )
)

(defun mj:sysvar (name / r)
  (setq r (if *mj-catch*
            (vl-catch-all-apply 'getvar (list name))
            (getvar name)
          )
  )
  (if (and *mj-catch* (vl-catch-all-error-p r))
    ""
    (mj:num->str r)
  )
)

;; is the string in the list ? (case insensitive, paths)
(defun mj:member-no-case (s lst)
  (if (or (null s) (null lst))
    nil
    (if (member (strcase s) (mapcar 'strcase lst))
      t
      nil
    )
  )
)

;; folders that must be trusted so the loader files run without a prompt
(defun mj:trust-list ( / add p f)
  (setq add '())
  (setq f (findfile "MJArea.lsp"))
  (if f
    (progn
      (setq p (mj:dirname f))
      (if (and p (/= p "")) (setq add (append add (list p))))
    )
  )
  (foreach p (mj:split (mj:sysvar "ACAD") ";")
    (if (and (wcmatch (strcase p) "*MJAREA.BUNDLE*")
             (not (mj:member-no-case p add)))
      (setq add (append add (list p)))
    )
  )
  add
)

;; add the folders above to TRUSTEDPATHS; returns the new value
(defun mj:trust-add ( / tp add p)
  (setq tp (mj:sysvar "TRUSTEDPATHS"))
  (setq add (mj:trust-list))
  (foreach p add
    (if (and p (/= p "")
             (not (mj:member-no-case p (mj:split tp ";"))))
      (setq tp (if (= tp "") p (strcat tp ";" p)))
    )
  )
  (if (not (equal tp (mj:sysvar "TRUSTEDPATHS")))
    (if *mj-catch*
      (vl-catch-all-apply 'setvar (list "TRUSTEDPATHS" tp))
      (setvar "TRUSTEDPATHS" tp)
    )
  )
  (mj:sysvar "TRUSTEDPATHS")
)

;; When SECURELOAD is 1 or 2, code may only be loaded from "trusted
;; locations" (TRUSTEDPATHS), otherwise AutoCAD asks every single start.
;; Register our own folders once, silently.
(defun mj:trust-self ( / sc)
  (if (null *mj-trusted*)
    (progn
      (setq *mj-trusted* t)
      (setq sc (mj:sysint "SECURELOAD"))
      (if (and (numberp sc) (> sc 0) *mj-catch*)
        (mj:trust-add)
      )
    )
  )
)

;; Run a native command with CMDECHO off.
;; IMPORTANT: the Mac AutoLISP engine refuses to pass COMMAND to apply or
;; vl-catch-all-apply ("sort function error: COMMAND"), so every native
;; command is written out literally here and selected by a small code.
;;   1 area of object   2 zoom object   3 zoom previous   4 copy
;;   5 draw order front 6 erase object  7 boundary (point) 8 erase set
;;   9 explode one object
(setq mj:z0 '(0.0 0.0 0.0))

(defun mj:cmd (op a / old)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (cond
    ((= op 1) (command "_.area" "_o" a))
    ((= op 2) (command "_.zoom" "_o" a ""))
    ((= op 3) (command "_.zoom" "_p"))
    ((= op 4) (command "_.copy" a "" mj:z0 mj:z0))
    ((= op 5) (command "_.draworder" a "" "_f"))
    ((= op 6) (command "_.erase" a ""))
    ((= op 7) (command "_.-boundary" a ""))
    ((= op 8) (command "_.erase" a ""))
  )
  (setvar "CMDECHO" old)
  t
)

;; ------------------------------------------------------------
;; settings
;; ------------------------------------------------------------

;; ("ini key" symbol type)   type: nil = string, I = integer, R = real
(setq *mj-keys*
  '(("unit"      unit      nil)
    ("dec"       dec       I)
    ("tpl"       tpl       nil)
    ("txtheight" txtheight R)
    ("txtlayer"  txtlayer  nil)
    ("txtcolor"  txtcolor  I)
    ("outlayer"  outlayer  nil)
    ("outcolor"  outcolor  I)
    ("outlw"     outlw     I)
    ("outmode"   outmode   I)
    ("outwidth"  outwidth  R)
    ("drawout"   drawout   I)
    ("autoplace" autoplace I)
    ("textstyle" textstyle nil)
    ("total"     total     I)
    ("lwdisplay" lwdisplay I)
    ("plotdev"   plotdev   nil)
    ("plotctb"   plotctb   nil)
    ("plotmode"  plotmode  nil)
    ("plotorder" plotorder nil)
    ("plotpaper" plotpaper nil)
    ("plotscale" plotscale nil)
    ("plotdir"   plotdir   nil)
    ("plotspace" plotspace nil)
    ("plotdebug" plotdebug nil)
  )
)

(defun mj:defaults ()
  (setq *mj-cfg*
    (list
      (cons 'unit      "mm")
      (cons 'dec       2)
      (cons 'tpl       "面积 {A}{n}周长 {P}")
      (cons 'txtheight 100.0)
      (cons 'txtlayer  "MJ-面积标注")
      (cons 'txtcolor  256)
      (cons 'outlayer  "MJ-面积红线")
      (cons 'outcolor  1)
      (cons 'outlw     70)
      (cons 'outmode   1)
      (cons 'outwidth  30.0)
      (cons 'drawout   1)
      (cons 'autoplace 1)
      (cons 'textstyle ".")
      (cons 'total     1)
      (cons 'lwdisplay 1)
      (cons 'plotdev   "DWG To PDF.pc3")
      (cons 'plotctb   "monochrome.ctb")
      (cons 'plotmode  "P")
      (cons 'plotorder "1")
      (cons 'plotpaper "")
      (cons 'plotscale "_F")
      (cons 'plotdir   "")
      (cons 'plotspace "M")
      (cons 'plotdebug "0")
    )
  )
)

(defun mj:get (k)
  (cdr (assoc k *mj-cfg*))
)

(defun mj:set (k v)
  (if (assoc k *mj-cfg*)
    (setq *mj-cfg* (subst (cons k v) (assoc k *mj-cfg*) *mj-cfg*))
    (setq *mj-cfg* (append *mj-cfg* (list (cons k v))))
  )
  v
)

;; first writable candidate folder for MJArea.ini
(defun mj:ini-path ( / cands p fp s f d)
  (if *mj-ini*
    *mj-ini*
    (progn
      (setq cands '())
      (setq s (getenv "ACAD"))
      (if (and s (/= s ""))
        (foreach p (mj:split s ";")
          (if (and (/= p "") (null (member p cands)))
            (setq cands (append cands (list p)))
          )
        )
      )
      (setq f (findfile "MJArea.lsp"))
      (if f
        (progn
          (setq d (mj:dirname f))
          (if (and d (null (member d cands)))
            (setq cands (append cands (list d)))
          )
        )
      )
      (setq d (getenv "HOME"))
      (if d
        (progn
          (setq d (strcat d "/Library/Application Support/Autodesk"))
          (if (null (member d cands)) (setq cands (append cands (list d))))
        )
      )
      ;; append mode: never truncates an existing settings file
      (foreach p cands
        (if (null *mj-ini*)
          (progn
            (setq fp (open (strcat p "/MJArea.ini") "a"))
            (if fp
              (progn (close fp) (setq *mj-ini* (strcat p "/MJArea.ini")))
            )
          )
        )
      )
      *mj-ini*
    )
  )
)

(defun mj:ini-load ( / p fp line kv hit sym ty v)
  (setq p (mj:ini-path))
  (if p
    (progn
      (setq fp (open p "r"))
      (if fp
        (progn
          (while (setq line (read-line fp))
            (setq line (mj:replace line (chr 13) ""))
            (if (and (> (strlen line) 2) (not (mj:starts line ";;")))
              (progn
                (setq kv (mj:split line "="))
                (if (and kv (>= (length kv) 2))
                  (progn
                    (setq hit (assoc (car kv) *mj-keys*))
                    (if hit
                      (progn
                        (setq sym (cadr hit) ty (caddr hit))
                        (setq v (mj:unesc (mj:join (cdr kv) "=")))
                        (cond
                          ((= ty 'I) (mj:set sym (fix (atof v))))
                          ((= ty 'R) (mj:set sym (atof v)))
                          (t (mj:set sym v))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          (close fp)
        )
      )
    )
  )
)

(defun mj:ini-save ( / p fp v hit)
  (setq p (mj:ini-path))
  (if p
    (progn
      (setq fp (open p "w"))
      (if fp
        (progn
          (write-line ";; MJArea settings -- use the MJSET command to change them" fp)
          (foreach hit *mj-keys*
            (setq v (mj:get (cadr hit)))
            (write-line
              (strcat (car hit) "="
                      (if (caddr hit) (mj:num->str v) (mj:esc v))
              )
              fp
            )
          )
          (close fp)
          t
        )
        nil
      )
    )
    nil
  )
)

(defun mj:init ()
  (if (null *mj-cfg*)
    (progn (mj:defaults) (mj:ini-load))
  )
  (if (null *mj-catch*)
    (if vl-catch-all-apply (setq *mj-catch* t))
  )
  (mj:trust-self)
  t
)

;; ------------------------------------------------------------
;; global error handler: keep the session tidy if something fails
;; ------------------------------------------------------------

(defun mj:err (msg)
  (setvar "CMDECHO" (if *mj-echo* *mj-echo* 1))
  (setq *error* *mj-olderr*)
  (prompt (strcat "\nMJArea 中断: " msg))
  (princ)
)

(defun mj:on ()
  (setq *mj-olderr* *error*)
  (setq *mj-echo* (getvar "CMDECHO"))
  (setq *error* mj:err)
)

(defun mj:off ()
  (setq *error* *mj-olderr*)
)

;; ------------------------------------------------------------
;; units and text
;; ------------------------------------------------------------

(defun mj:area-factor (u)
  (cond ((= u "mm") 1000000.0)
        ((= u "cm") 10000.0)
        ((= u "m") 1.0)
        (t 1.0)
  )
)

(defun mj:len-factor (u)
  (cond ((= u "mm") 1000.0)
        ((= u "cm") 100.0)
        ((= u "m") 1.0)
        (t 1.0)
  )
)

(defun mj:area-unit (u)
  (if (= u "none") "" "㎡")
)

(defun mj:len-unit (u)
  (if (= u "none") "" "m")
)

(defun mj:conv-area (a)
  (/ a (mj:area-factor (mj:get 'unit)))
)

(defun mj:conv-len (l)
  (/ l (mj:len-factor (mj:get 'unit)))
)

(defun mj:fmt-area (a)
  (strcat (rtos (mj:conv-area a) 2 (mj:get 'dec)) (mj:area-unit (mj:get 'unit)))
)

(defun mj:fmt-len (l)
  (strcat (rtos (mj:conv-len l) 2 (mj:get 'dec)) (mj:len-unit (mj:get 'unit)))
)

(defun mj:label-text (a p / tpl)
  (setq tpl (mj:get 'tpl))
  (setq tpl (mj:replace tpl "{A}" (mj:fmt-area a)))
  (setq tpl (mj:replace tpl "{P}" (mj:fmt-len p)))
  (setq tpl (mj:replace tpl "{a}" (rtos (mj:conv-area a) 2 (mj:get 'dec))))
  (setq tpl (mj:replace tpl "{p}" (rtos (mj:conv-len p) 2 (mj:get 'dec))))
  ;; MTEXT line break is the two characters \P - built without any escape
  (setq tpl (mj:replace tpl "{n}" (strcat (mj:bs) "P")))
  tpl
)

;; ------------------------------------------------------------
;; geometry
;; ------------------------------------------------------------

;; one edge p1 -> p2 with bulge b : returns (arc length  signed segment area)
(defun mj:seg (p1 p2 b / dx dy c th r sn)
  (setq dx (- (car p2) (car p1)) dy (- (cadr p2) (cadr p1)))
  (setq c (sqrt (+ (* dx dx) (* dy dy))))
  (if (or (null b) (equal b 0.0 0.000000000001) (equal c 0.0 0.000000000001))
    (list c 0.0)
    (progn
      (setq th (* 4.0 (atan b)))
      (setq sn (sin (/ th 2.0)))
      (if (equal sn 0.0 0.000000000001)
        (list c 0.0)
        (progn
          (setq r (/ c (* 2.0 sn)))
          (list (* r (abs th)) (* 0.5 r r (- th (sin th))))
        )
      )
    )
  )
)

;; vertices of a LWPOLYLINE -> list of (point bulge)
(defun mj:lw-verts (e / d verts cur)
  (setq d (entget e) verts '() cur nil)
  (foreach g d
    (cond
      ((= 10 (car g))
        (if cur (setq verts (cons cur verts)))
        (setq cur (list (cdr g) 0.0))
      )
      ((= 42 (car g))
        (if cur (setq cur (list (car cur) (cdr g))))
      )
    )
  )
  (if cur (setq verts (cons cur verts)))
  (reverse verts)
)

;; vertices of a classic POLYLINE (walk the VERTEX sub entities)
(defun mj:pl-verts (e / sub d out b)
  (setq out '() sub (entnext e))
  (while (and sub (= "VERTEX" (mj:dxf0 sub)))
    (setq d (entget sub) b (cdr (assoc 42 d)))
    (if (null b) (setq b 0.0))
    (setq out (append out (list (list (cdr (assoc 10 d)) b))))
    (setq sub (entnext sub))
  )
  out
)

;; area / perimeter / centroid of a vertex list -> (area perim cx cy)
;; closed = T : the polyline is closed
;; closed = nil : area is computed as if closed by a straight edge,
;;                the perimeter only counts the existing edges
(defun mj:poly-metrics (verts closed / n i p1 p2 b sg sum len numx numy den cx cy sx sy)
  (setq n (length verts))
  (if (< n 2)
    nil
    (progn
      (setq sum 0.0 len 0.0 numx 0.0 numy 0.0 den 0.0 i 0 sx 0.0 sy 0.0)
      (while (< i n)
        (setq p1 (car (nth i verts)))
        (setq sx (+ sx (car p1)) sy (+ sy (cadr p1)))
        (if (< (1+ i) n)
          (setq p2 (car (nth (1+ i) verts)))
          ;; last vertex: the closing edge goes back to the first vertex
          (setq p2 (car (car verts)))
        )
        ;; the polygon (chord) part, counted for every edge including the
        ;; closing one - this is what makes the area correct
        (setq den (+ den (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1)))))
        (setq sum (+ sum (* 0.5 (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1))))))
        (setq numx (+ numx (* (+ (car p1) (car p2))
                              (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1))))))
        (setq numy (+ numy (* (+ (cadr p1) (cadr p2))
                              (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1))))))
        ;; the arc part (bulge) of this edge, and its real length
        (if (or (< (1+ i) n) closed)
          (progn
            (setq b (cadr (nth i verts)))
            (setq sg (mj:seg p1 p2 b))
            (setq len (+ len (car sg)))
            (setq sum (+ sum (cadr sg)))
          )
        )
        (setq i (1+ i))
      )
      (setq sum (abs sum))
      (if (equal den 0.0 0.000000001)
        ;; degenerate polygon part (e.g. a circle built from two arcs):
        ;; fall back to the average of the vertices
        (setq cx (/ sx n) cy (/ sy n))
        (progn
          (setq cx (/ numx (* 3.0 den)))
          (setq cy (/ numy (* 3.0 den)))
        )
      )
      (list sum len cx cy)
    )
  )
)

;; bounding box of a vertex list -> (minx miny maxx maxy)
(defun mj:verts-box (verts / mnx mny mxx mxy p)
  (foreach v verts
    (setq p (car v))
    (if (null mnx)
      (setq mnx (car p) mny (cadr p) mxx (car p) mxy (cadr p))
      (progn
        (if (< (car p) mnx) (setq mnx (car p)))
        (if (< (cadr p) mny) (setq mny (cadr p)))
        (if (> (car p) mxx) (setq mxx (car p)))
        (if (> (cadr p) mxy) (setq mxy (cadr p)))
      )
    )
  )
  (list mnx mny mxx mxy)
)

;; ------------------------------------------------------------
;; assembling a closed loop out of separate lines / arcs / polylines
;; ------------------------------------------------------------

;; 2D point comparison with a tolerance
(defun mj:samept (p q tol / dx dy)
  (if (or (null p) (null q))
    nil
    (progn
      (setq dx (- (car p) (car q)) dy (- (cadr p) (cadr q)))
      (< (sqrt (+ (* dx dx) (* dy dy))) tol)
    )
  )
)

;; bulge value of an ARC entity (start angle a1 -> end angle a2, CCW)
(defun mj:arc-bulge (a1 a2 / th)
  (setq th (- a2 a1))
  (while (< th 0.0) (setq th (+ th (* 2.0 pi))))
  (tan (/ th 4.0))
)

;; segments (p1 p2 bulge) of a LINE or an ARC entity
(defun mj:ent-segs (e / d ty c r a1 a2)
  (setq d (entget e) ty (cdr (assoc 0 d)))
  (cond
    ((= ty "LINE")
      (list (list (cdr (assoc 10 d)) (cdr (assoc 11 d)) 0.0))
    )
    ((= ty "ARC")
      (setq c (cdr (assoc 10 d)) r (cdr (assoc 40 d)))
      (setq a1 (cdr (assoc 50 d)) a2 (cdr (assoc 51 d)))
      (list (list (polar c a1 r) (polar c a2 r) (mj:arc-bulge a1 a2)))
    )
    (t nil)
  )
)

;; segments of a vertex list -> list of (p1 p2 bulge)
(defun mj:verts->segs (verts closed / n i out p1 p2 b)
  (setq n (length verts) i 0 out '())
  (while (< i (if closed n (1- n)))
    (setq p1 (car (nth i verts)))
    (setq p2 (car (nth (rem (1+ i) n) verts)))
    (setq b (cadr (nth i verts)))
    (if (null b) (setq b 0.0))
    (setq out (append out (list (list p1 p2 b))))
    (setq i (1+ i))
  )
  out
)

(defun mj:drop-nth (lst i / out n)
  (setq out '() n 0)
  (foreach x lst
    (if (/= n i) (setq out (append out (list x))))
    (setq n (1+ n))
  )
  out
)

;; try to chain a list of segments into one closed loop
;; returns the vertex list (point bulge) or nil
(defun mj:assemble (segs tol / verts pool e from to b found rev i n guard cur start)
  (setq pool segs n (length segs) verts '() guard 0)
  (if (< n 2)
    nil
    (progn
      ;; start with the first segment
      (setq e (car pool) from (car e) to (cadr e) b (caddr e))
      (setq start from)
      (setq verts (list (list from b)))
      (setq cur to pool (cdr pool))
      (setq found t)
      (while (and found pool (< guard 100000))
        (setq guard (1+ guard))
        (if (mj:samept cur start tol)
          (setq found nil)
          (progn
            (setq i 0 e nil rev nil)
            (foreach s pool
              (if (null e)
                (cond
                  ((mj:samept cur (car s) tol) (setq e s rev nil))
                  ((mj:samept cur (cadr s) tol) (setq e s rev t))
                )
              )
            )
            (if e
              (progn
                ;; remove it from the pool (by identity of index)
                (setq i 0)
                (foreach s pool
                  (if (equal s e) (setq pool (mj:drop-nth pool i)))
                  (setq i (1+ i))
                )
                ;; record the vertex the edge starts at, then move to its far end
                (if rev
                  (progn
                    (setq verts (append verts (list (list cur (- (caddr e))))))
                    (setq cur (car e))
                  )
                  (progn
                    (setq verts (append verts (list (list cur (caddr e)))))
                    (setq cur (cadr e))
                  )
                )
              )
              (setq found nil)
            )
          )
        )
      )
      (if (and (null pool) (mj:samept cur start tol) (> (length verts) 1))
        verts
        nil
      )
    )
  )
)

;; is a point inside the polygon ? (ray casting, bulges ignored)
(defun mj:inside-p (pt verts / n i x y xi yi xj yj inside)
  (setq n (length verts) i 0 x (car pt) y (cadr pt) inside nil)
  (while (< i n)
    (setq xi (car (car (nth i verts))) yi (cadr (car (nth i verts))))
    (setq xj (car (car (nth (rem (1+ i) n) verts))))
    (setq yj (cadr (car (nth (rem (1+ i) n) verts))))
    (if (and (/= (> yi y) (> yj y))
             (< x (+ xi (/ (* (- xj xi) (- y yi)) (- yj yi)))))
      (setq inside (not inside))
    )
    (setq i (1+ i))
  )
  inside
)

;; a point that is really inside the region: area centroid, else bbox centre,
;; else the middle of the widest horizontal slice
(defun mj:inside-point (m / c v box cx cy mnx mny mxx mxy rows cols i j x y
                          best bestw w first inside)
  (setq c (cdr (assoc 'c m)) v (cdr (assoc 'verts m)) box (cdr (assoc 'box m)))
  (cond
    ((null v) c)
    ((mj:inside-p c v) c)
    ((and box
          (mj:inside-p (list (/ (+ (car box) (caddr box)) 2.0)
                             (/ (+ (cadr box) (cadddr box)) 2.0) 0.0) v))
     (list (/ (+ (car box) (caddr box)) 2.0)
           (/ (+ (cadr box) (cadddr box)) 2.0) 0.0)
    )
    (t
      (setq mnx (car box) mny (cadr box) mxx (caddr box) mxy (cadddr box))
      (setq rows 15 cols 40 best nil bestw 0.0)
      (setq i 1)
      (while (<= i rows)
        (setq y (+ mny (* (- mxy mny) (/ (float i) (1+ rows)))))
        (setq j 0 first nil inside nil)
        (while (<= j cols)
          (setq x (+ mnx (* (- mxx mnx) (/ (float j) cols))))
          (if (mj:inside-p (list x y 0.0) v)
            (progn (if (null first) (setq first x)) (setq inside t))
            (progn
              (if (and inside first)
                (progn
                  (setq w (- x first))
                  (if (> w bestw)
                    (setq bestw w best (list (/ (+ first x) 2.0) y 0.0))
                  )
                )
              )
              (setq inside nil first nil)
            )
          )
          (setq j (1+ j))
        )
        (setq i (1+ i))
      )
      (if best best c)
    )
  )
)

;; create a closed polyline out of a vertex list (used for the red outline)
(defun mj:make-pline (verts layer color lw width / lst)
  (setq lst (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                  (cons 8 layer) (cons 62 (mj:layer-color color))
                  (cons 370 lw)
                  '(100 . "AcDbPolyline")
                  (cons 90 (length verts))
                  '(70 . 1)))
  (if (and width (> width 0.0))
    (setq lst (append lst (list (cons 43 width))))
  )
  (foreach v verts
    (setq lst (append lst (list (cons 10 (car v)))))
    (if (and (cadr v) (not (equal (cadr v) 0.0 0.000000000001)))
      (setq lst (append lst (list (cons 42 (cadr v)))))
    )
  )
  (entmake lst)
)

;; Ramanujan approximation of an ellipse perimeter
(defun mj:ellipse-perim (a b / h)
  (setq h (/ (expt (- a b) 2) (expt (+ a b) 2)))
  (* pi (+ a b) (- 1.0 (* 0.25 h) (* 0.015625 h h) (* 0.00390625 h h h)))
)

;; ------------------------------------------------------------
;; measuring entities
;; ------------------------------------------------------------

(defun mj:com-on ( / r)
  (if (null *mj-com*)
    (setq *mj-com*
      (if (and vl-load-com vl-catch-all-apply)
        (progn
          (setq r (vl-catch-all-apply 'vl-load-com nil))
          (if (vl-catch-all-error-p r) nil t)
        )
        nil
      )
    )
  )
  *mj-com*
)

(defun mj:vla (e / r)
  (if (mj:com-on)
    (progn
      (setq r (vl-catch-all-apply 'vlax-ename->vla-object (list e)))
      (if (vl-catch-all-error-p r) nil r)
    )
    nil
  )
)

;; (area perimeter) through ActiveX, nil when unavailable
(defun mj:measure-com (e / o a p)
  (setq o (mj:vla e))
  (if o
    (progn
      (setq a (vl-catch-all-apply 'vla-get-Area (list o)))
      (setq p (vl-catch-all-apply 'vla-get-Perimeter (list o)))
      (if (and (not (vl-catch-all-error-p a)) (not (vl-catch-all-error-p p)))
        (list (float a) (float p))
        nil
      )
    )
    nil
  )
)

;; (area perimeter) through the AREA command : works for every closed object
(defun mj:measure-cmd (e)
  (mj:cmd 1 e)
  (list (getvar "AREA") (getvar "PERIMETER"))
)

;; centre of an entity through the ActiveX bounding box
(defun mj:center-com (e / o r mn mx l1 l2)
  (setq o (mj:vla e))
  (if o
    (progn
      (setq r (vl-catch-all-apply 'vla-GetBoundingBox (list o 'mn 'mx)))
      (if (vl-catch-all-error-p r)
        nil
        (progn
          (setq l1 (vl-catch-all-apply 'vlax-safearray->list
                                       (list (vlax-variant-value mn))))
          (setq l2 (vl-catch-all-apply 'vlax-safearray->list
                                       (list (vlax-variant-value mx))))
          (if (or (vl-catch-all-error-p l1) (vl-catch-all-error-p l2))
            nil
            (list (/ (+ (car l1) (car l2)) 2.0)
                  (/ (+ (cadr l1) (cadr l2)) 2.0) 0.0)
          )
        )
      )
    )
    nil
  )
)

;; centre through ZOOM Object (fallback, restores the previous view)
(defun mj:center-zoom (e / c)
  (mj:cmd 2 e)
  (setq c (getvar "VIEWCTR"))
  (mj:cmd 3 nil)
  c
)

;; metrics of a vertex list -> assoc list in the format used everywhere else
(defun mj:metrics-from-verts (verts closed / r)
  (setq r (mj:poly-metrics verts closed))
  (if (and r (> (car r) 0.0))
    (list (cons 'a (car r)) (cons 'p (cadr r))
          (cons 'c (list (caddr r) (cadddr r) 0.0))
          (cons 'closed closed) (cons 'ty "LOOP") (cons 'ext nil)
          (cons 'verts verts) (cons 'box (mj:verts-box verts)))
    nil
  )
)

;; bounding box of a segment list -> (minx miny maxx maxy)
(defun mj:segs-box (segs / mnx mny mxx mxy p)
  (foreach s segs
    (foreach p (list (car s) (cadr s))
      (if (null mnx)
        (setq mnx (car p) mny (cadr p) mxx (car p) mxy (cadr p))
        (progn
          (if (< (car p) mnx) (setq mnx (car p)))
          (if (< (cadr p) mny) (setq mny (cadr p)))
          (if (> (car p) mxx) (setq mxx (car p)))
          (if (> (cadr p) mxy) (setq mxy (cadr p)))
        )
      )
    )
  )
  (list mnx mny mxx mxy)
)

;; assemble loose segments and measure the resulting closed loop
(defun mj:loop-metrics (segs / box diag tol verts)
  (setq box (mj:segs-box segs))
  (setq diag (distance (list (car box) (cadr box) 0.0)
                       (list (caddr box) (cadddr box) 0.0)))
  (setq tol (/ diag 20000.0))
  (if (< tol 0.000000001) (setq tol 0.000000001))
  (setq verts (mj:assemble segs tol))
  (if verts (mj:metrics-from-verts verts t) nil)
)

;; main measuring entry point -> assoc list or nil
(defun mj:measure (e / d ty ext verts closed r ap)
  (setq d (entget e) ty (cdr (assoc 0 d)) ext (cdr (assoc 210 d)) r nil)
  (cond
    ((or (= ty "LWPOLYLINE") (= ty "POLYLINE"))
      (setq verts (if (= ty "LWPOLYLINE") (mj:lw-verts e) (mj:pl-verts e)))
      (setq closed (= 1 (logand 1 (fix (cond ((cdr (assoc 70 d))) (t 0))))))
      (setq r (mj:metrics-from-verts verts closed))
      (if r (setq r (mj:putassoc r 'ext ext)))
    )
    ((= ty "CIRCLE")
      (setq ap (cdr (assoc 40 d)))
      (setq r (list (cons 'a (* pi ap ap)) (cons 'p (* 2.0 pi ap))
                    (cons 'c (cdr (assoc 10 d))) (cons 'closed t)
                    (cons 'ty ty) (cons 'ext ext)
                    (cons 'box (list (- (car (cdr (assoc 10 d))) ap)
                                     (- (cadr (cdr (assoc 10 d))) ap)
                                     (+ (car (cdr (assoc 10 d))) ap)
                                     (+ (cadr (cdr (assoc 10 d))) ap)))))
    )
    ((= ty "ELLIPSE")
      (setq ap (distance '(0 0 0) (cdr (assoc 11 d))))
      (setq r (list (cons 'a (* pi ap (* ap (cdr (assoc 40 d)))))
                    (cons 'p (mj:ellipse-perim ap (* ap (cdr (assoc 40 d)))))
                    (cons 'c (cdr (assoc 10 d))) (cons 'closed t)
                    (cons 'ty ty) (cons 'ext ext)
                    (cons 'box (list (- (car (cdr (assoc 10 d))) ap)
                                     (- (cadr (cdr (assoc 10 d))) ap)
                                     (+ (car (cdr (assoc 10 d))) ap)
                                     (+ (cadr (cdr (assoc 10 d))) ap)))))
    )
  )
  ;; everything else: ActiveX, then the AREA command
  (if (null r)
    (progn
      (setq ap (mj:measure-com e))
      (if (null ap) (setq ap (mj:measure-cmd e)))
      (if (and ap (> (car ap) 0.0))
        (progn
          (setq d (mj:center-com e))
          (if (null d) (setq d (mj:center-zoom e)))
          (setq r (list (cons 'a (car ap)) (cons 'p (cadr ap)) (cons 'c d)
                        (cons 'closed t) (cons 'ty ty) (cons 'ext ext)
                        (cons 'box nil)))
        )
      )
    )
  )
  r
)

;; ------------------------------------------------------------
;; drawing helpers
;; ------------------------------------------------------------

(defun mj:layer-color (c)
  (if (or (null c) (< c 1) (> c 255)) 7 c)
)

(defun mj:ensure-layer (name color lw)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 (mj:layer-color color))
                   '(6 . "Continuous") (cons 370 lw)))
  )
  (tblsearch "LAYER" name)
)

(defun mj:current-style ()
  (getvar "TEXTSTYLE")
)

(defun mj:text-style ( / n)
  (setq n (mj:get 'textstyle))
  (if (or (null n) (= n ".") (= n ""))
    (mj:current-style)
    (progn
      (if (not (tblsearch "STYLE" n)) (mj:clone-style n))
      (if (tblsearch "STYLE" n) n (mj:current-style))
    )
  )
)

;; create a style by copying the fonts of the current style
(defun mj:clone-style (name / sd f bf)
  (setq sd (tblsearch "STYLE" (mj:current-style)))
  (if sd
    (progn
      (setq f (cdr (assoc 3 sd)) bf (cdr (assoc 4 sd)))
      (if (null f) (setq f "txt"))
      (if (null bf) (setq bf ""))
      (entmake (list '(0 . "STYLE") '(100 . "AcDbSymbolTableRecord")
                     '(100 . "AcDbTextStyleTableRecord")
                     (cons 2 name) '(70 . 0) '(40 . 0.0) '(41 . 1.0)
                     '(50 . 0.0) '(71 . 0) '(42 . 2.5)
                     (cons 3 f) (cons 4 bf)))
    )
    nil
  )
)

;; one time warning when the text style cannot show Chinese
(defun mj:check-style ( / sd f bf bad)
  (setq sd (tblsearch "STYLE" (mj:text-style)))
  (if (and sd (null *mj-warned*))
    (progn
      (setq f (strcase (cdr (assoc 3 sd))) bf (cdr (assoc 4 sd)))
      (setq bad (and f (wcmatch f "*.SHX") (or (null bf) (= bf ""))))
      (if bad
        (progn
          (setq *mj-warned* t)
          (prompt "\n提示: 当前文字样式用的是 SHX 单线字体，可能显示不了中文。")
          (prompt "\n      如果标注出现问号，请用 STYLE 改成中文字体，")
          (prompt "\n      或运行 MJSET 修改“标注文字样式”。")
        )
      )
    )
  )
)

(defun mj:make-mtext (pt txt h layer sty ext / r)
  (setq r (entmake
    (append
      (list '(0 . "MTEXT") '(100 . "AcDbEntity") (cons 8 layer))
      (list '(100 . "AcDbMText")
            (cons 10 pt) (cons 40 h) (cons 41 0.0)
            (cons 71 5) (cons 72 1) (cons 1 txt)
            (cons 7 sty) (cons 50 0.0))
      (if (and ext (not (equal ext '(0.0 0.0 1.0) 0.000000001)))
        (list (cons 210 ext))
      )
    )
  ))
  r
)

;; highlight: red thick outline of the region.
;;   e = boundary entity -> copy it (perfect geometry, any object type)
;;   e = nil            -> build a closed polyline from the metrics vertices
(defun mj:draw-outline (e m / new d lay col lw mode w ty verts)
  (setq lay (mj:get 'outlayer) col (mj:get 'outcolor)
        lw (mj:get 'outlw) mode (mj:get 'outmode) w (mj:get 'outwidth))
  (mj:ensure-layer lay col lw)
  (cond
    ;; region assembled from several lines/arcs -> build one closed polyline
    ((null e)
      (setq verts (cdr (assoc 'verts m)))
      (if verts (mj:make-pline verts lay col lw (if (= mode 2) w 0.0)))
    )
    ;; single closed object -> copy it, so circles/ellipses/splines stay exact
    (t
      (mj:cmd 4 e)
      (setq new (entlast))
      (if (and new (not (eq new e)))
        (progn
          (setq d (entget new) ty (cdr (assoc 0 d)))
          (setq d (mj:putd d 8 lay))
          (setq d (mj:putd d 62 col))
          (setq d (mj:putd d 6 "ByLayer"))
          (if (= mode 2)
            (cond
              ((= ty "LWPOLYLINE") (setq d (mj:putd d 43 w)))
              ((= ty "POLYLINE") (progn (setq d (mj:putd d 40 w))
                                        (setq d (mj:putd d 41 w))))
              (t (setq d (mj:putd d 370 lw)))
            )
            (setq d (mj:putd d 370 lw))
          )
          (entmod d)
          (entupd new)
          (mj:cmd 5 new)
          new
        )
        nil
      )
    )
  )
)

(defun mj:erase-ent (e)
  (if (and e (entget e))
    (mj:cmd 6 e)
  )
)

;; ------------------------------------------------------------
;; annotate one region
;; ------------------------------------------------------------

(defun mj:auto-height (m / box diag a)
  (setq box (cdr (assoc 'box m)) a (cdr (assoc 'a m)))
  (if (and box (car box) (> (length box) 3))
    (setq diag (distance (list (car box) (cadr box) 0.0)
                         (list (caddr box) (cadddr box) 0.0)))
    (setq diag (* 10.0 (sqrt (abs a))))
  )
  (/ diag 22.0)
)

(defun mj:annotate (e m tmp / a p c h pt txt sty ext)
  (setq a (cdr (assoc 'a m)) p (cdr (assoc 'p m)) c (cdr (assoc 'c m))
        ext (cdr (assoc 'ext m)))
  (setq h (mj:get 'txtheight))
  (if (or (null h) (<= h 0.0)) (setq h (mj:auto-height m)))
  (if (or (null h) (< h 0.000000001)) (setq h 1.0))
  (setq *mj-last-h* h)
  (if (= (mj:get 'autoplace) 1)
    (setq pt (mj:inside-point m))
    (setq pt (getpoint (if c c '(0 0 0)) "\n指定文字位置: "))
  )
  (if (null pt) (setq pt c))
  (if (null pt) (setq pt '(0 0 0)))
  (mj:check-style)
  (setq sty (mj:text-style))
  (mj:ensure-layer (mj:get 'txtlayer) 7 -1)
  (setq txt (mj:label-text a p))
  (if (null (mj:make-mtext pt txt h (mj:get 'txtlayer) sty ext))
    (progn
      (prompt "\n错误: 无法创建标注文字（图层是否被锁定或冻结？）")
      nil
    )
    (progn
      (if (= (mj:get 'drawout) 1) (mj:draw-outline e m))
      (if tmp (mj:erase-ent e))
      (setq *mj-sum-n* (1+ *mj-sum-n*)
            *mj-sum-a* (+ *mj-sum-a* a)
            *mj-sum-p* (+ *mj-sum-p* p))
      (prompt (strcat "\n   面积 " (mj:fmt-area a) "   周长 " (mj:fmt-len p)))
      t
    )
  )
)

(defun mj:closed-warn (m)
  (if (and (assoc 'closed m) (null (cdr (assoc 'closed m))))
    (prompt "\n注意: 该对象不是闭合多段线，面积按“两端点直线闭合”计算。")
  )
)

;; ------------------------------------------------------------
;; internal point mode : build the boundary with -BOUNDARY
;; ------------------------------------------------------------

;; every entity created after "old" (used to catch what BOUNDARY created)
(defun mj:new-ents (old / e out)
  (setq out '())
  (setq e (if old (entnext old) (entnext)))
  (while e
    (setq out (append out (list e)))
    (setq e (entnext e))
  )
  out
)

;; BO: pick an internal point, AutoCAD builds the closed boundary for us.
;; If island detection created several outlines, keep the biggest one (the
;; room) and erase the rest, so the drawing stays clean.
(defun mj:boundary-from-point (pt / old news e m a best besta)
  (setq old (entlast))
  ;; only the command line version of BOUNDARY is used on purpose
  (mj:cmd 7 pt)
  (setq news (mj:new-ents old))
  (setq best nil besta -1.0)
  (foreach e news
    (setq m (mj:measure e))
    (if m
      (progn
        (setq a (cdr (assoc 'a m)))
        (if (and (numberp a) (> a besta))
          (setq besta a best e)
        )
      )
    )
  )
  (foreach e news
    (if (not (eq e best)) (mj:erase-ent e))
  )
  best
)

;; click inside a space -> BO -> area / perimeter text + red outline
(defun mj:point-run (pt / e m)
  (while pt
    (setq e (mj:boundary-from-point pt))
    (if e
      (progn
        (setq m (mj:measure e))
        (if m
          (progn (mj:closed-warn m) (mj:annotate e m t))
          (progn
            (prompt "\n警告: 无法计算该区域的面积。")
            (mj:erase-ent e)
          )
        )
      )
      (progn
        (prompt "\n没能生成边界 (BO 失败)：")
        (prompt "\n  1) 确认该点确实落在封闭区域内；")
        (prompt "\n  2) 区域边界要首尾相接，不能有缺口；")
        (prompt "\n  3) 或者改用“选择已有线框”的方式 (输入 S)。")
      )
    )
    (setq pt (getpoint "\n继续点取下一个区域内部一点 (回车结束): "))
  )
)

;; ------------------------------------------------------------
;; header / summary
;; ------------------------------------------------------------

(defun mj:unit-name (u)
  (cond ((= u "mm") "毫米")
        ((= u "cm") "厘米")
        ((= u "m") "米")
        (t "不换算")
  )
)

(defun mj:header ()
  (prompt (strcat "\nMJArea " *mj-ver* "  图纸单位: " (mj:unit-name (mj:get 'unit))
                  "   面积单位: " (mj:area-unit (mj:get 'unit))
                  "   周长单位: " (mj:len-unit (mj:get 'unit))))
  (prompt (strcat "\n标注图层: " (mj:get 'txtlayer)
                  "   红线图层: " (mj:get 'outlayer)
                  "   设置: MJSET  帮助: MJHELP"))
)

(defun mj:total-summary (n a p)
  (strcat "合计 " (itoa n) " 个区域   面积 " (mj:fmt-area a)
          "   周长 " (mj:fmt-len p))
)

(defun mj:place-total ( / pt txt)
  (if (> *mj-sum-n* 1)
    (progn
      (prompt (strcat "\n" (mj:total-summary *mj-sum-n* *mj-sum-a* *mj-sum-p*)))
      (if (= (mj:get 'total) 1)
        (progn
          (initget "Y N")
          (if (not (equal "N" (getkword "\n是否把合计写到图上 [是(Y)/否(N)] <是>: ")))
            (progn
              (setq pt (getpoint "\n指定合计文字的位置: "))
              (if pt
                (progn
                  (setq txt (mj:total-summary *mj-sum-n* *mj-sum-a* *mj-sum-p*))
                  (mj:ensure-layer (mj:get 'txtlayer) 7 -1)
                  (if (null (mj:make-mtext pt txt
                                           (if (> *mj-last-h* 0.0) *mj-last-h* 1.0)
                                           (mj:get 'txtlayer) (mj:text-style) nil))
                    (prompt "\n错误: 无法创建合计文字。")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; main command
;; ------------------------------------------------------------

;; The real implementations.  Commands must NOT be called from Lisp on the
;; Mac engine ("no function definition: C:MJ"), so every command below only
;; calls these ordinary functions.

;; ---- flow 1: pick the wireframe that is already there ----
(defun mj:cmd-select ( / ss i n e m d ty verts vc segs clsd)
  (prompt "\n[选择已有线框] 可多选、可框选:")
  (prompt "\n  可以是直线/圆弧拼成的闭合框，也可以是闭合多段线、圆、椭圆、面域、填充。")
  (setq ss (ssget (list (cons 0 "LINE,ARC,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE,SPLINE,REGION,HATCH"))))
  (if ss
    (progn
      ;; ---- 1. sort the selection into "closed objects" and "loose pieces"
      (setq i 0 n (sslength ss) segs '() clsd '() verts '() vc nil)
      (while (< i n)
        (setq e (ssname ss i) d (entget e) ty (cdr (assoc 0 d)))
        (cond
          ((or (= ty "LINE") (= ty "ARC"))
            (setq segs (append segs (mj:ent-segs e)))
          )
          ((or (= ty "LWPOLYLINE") (= ty "POLYLINE"))
            (setq verts (if (= ty "LWPOLYLINE") (mj:lw-verts e) (mj:pl-verts e)))
            (setq vc (= 1 (logand 1 (fix (if (cdr (assoc 70 d)) (cdr (assoc 70 d)) 0)))))
            (if vc
              (setq clsd (append clsd (list e)))
              (setq segs (append segs (mj:verts->segs verts nil)))
            )
          )
          (t (setq clsd (append clsd (list e))))
        )
        (setq i (1+ i))
      )
      ;; ---- 2. every single closed object is measured on its own
      (foreach e clsd
        (setq m (mj:measure e))
        (if m
          (progn (mj:closed-warn m) (mj:annotate e m nil))
          (prompt "\n警告: 无法计算该对象的面积，已跳过。")
        )
      )
      ;; ---- 3. loose pieces are chained into one closed loop
      (if segs
        (progn
          (prompt (strcat "\n正在由 " (itoa (length segs)) " 段线/弧拼合区域..."))
          (setq m (mj:loop-metrics segs))
          (if m
            (progn
              (prompt (strcat "\n  拼合成功: " (itoa (length (cdr (assoc 'verts m)))) " 个顶点"))
              (mj:annotate nil m nil)
            )
            (progn
              (prompt "\n警告: 选中的线/圆弧没有拼成闭合的框（有缺口或端点没接上），已跳过。")
              (prompt "\n      可改用: 输入 MJ 后直接回车 -> 在区域内部点一下。")
            )
          )
        )
      )
      (if (and (null clsd) (null segs))
        (prompt "\n没有选中可用的对象。")
      )
    )
    (prompt "\n没有选中对象。")
  )
)

;; ---- main entry: click a point (BO) or type S to select the wireframe ----
(defun mj:cmd-main ( / pt)
  (mj:init)
  (mj:on)
  (mj:header)
  (setq *mj-sum-n* 0 *mj-sum-a* 0.0 *mj-sum-p* 0.0)
  (initget "S")
  (setq pt (getpoint "\n点取区域内部一点 (可连续点多个) 或 [选择已有线框(S)]: "))
  (cond
    ((eq (type pt) 'STR) (mj:cmd-select))
    (pt (mj:point-run pt))
    (t (mj:cmd-select))
  )
  (mj:place-total)
  (if (= (mj:get 'lwdisplay) 1)
    (if *mj-catch*
      (vl-catch-all-apply 'setvar (list "LWDISPLAY" 1))
      (setvar "LWDISPLAY" 1)
    )
  )
  (mj:off)
  (princ)
)

(defun c:MJ ()
  (mj:cmd-main)
)

;; other entry names.  On some builds the Mac engine refuses to register a
;; two letter Lisp command ("unknown command: MJ"), so MJA / MJAREA are the
;; reliable names, and the installer also adds "MJ, *MJA" to acadUser.pgp so
;; the MJ shortcut still works.
(defun c:MJA ()
  (mj:cmd-main)
)

(defun c:MJAREA ()
  (mj:cmd-main)
)

(defun c:MJP ()
  (mj:cmd-main)
)

;; ------------------------------------------------------------
;; entry points for the "My Tools" toolbar group
;; ------------------------------------------------------------

;; where the uninstall helper lives (placed there by the installer)
(defun mj:uninstaller-path ( / p)
  (setq p (strcat (getenv "HOME")
                  "/Library/Application Support/Autodesk/ApplicationAddins/"
                  "卸载插件.command"))
  (if (findfile p) p nil)
)

;; ------------------------------------------------------------
;; batch plot :  every layout of the current drawing
;;               -> PDF file   or   -> straight to the printer
;; ------------------------------------------------------------

;; ---------- frame recognition (rectangle around each drawing) ----------

;; does this segment share an end point with anything in the group ?
(defun mj:seg-touch (s grp tol / hit)
  (setq hit nil)
  (foreach g grp
    (if (or (mj:samept (car s) (car g) tol)
            (mj:samept (car s) (cadr g) tol)
            (mj:samept (cadr s) (car g) tol)
            (mj:samept (cadr s) (cadr g) tol))
      (setq hit t)
    )
  )
  hit
)

;; split a loose segment list into connected groups (one group = one frame)
(defun mj:group-segs (segs tol / groups rest grp rest2 s changed seed)
  (setq groups '() rest segs)
  (while rest
    (setq seed (car rest) rest (cdr rest) grp (list seed) changed t)
    (while changed
      (setq changed nil rest2 '())
      (foreach s rest
        (if (mj:seg-touch s grp tol)
          (progn (setq grp (append grp (list s)) changed t))
          (setq rest2 (append rest2 (list s)))
        )
      )
      (setq rest rest2)
    )
    (setq groups (append groups (list grp)))
  )
  groups
)

;; selection -> list of frames, each frame = (minx miny maxx maxy)
(defun mj:frames-from-sset (ss / i n e d ty verts closed segs frames box grp loop tol)
  (setq i 0 n (if ss (sslength ss) 0) segs '() frames '())
  (while (< i n)
    (setq e (ssname ss i) d (entget e) ty (cdr (assoc 0 d)))
    (cond
      ((or (= ty "LWPOLYLINE") (= ty "POLYLINE"))
        (setq verts (if (= ty "LWPOLYLINE") (mj:lw-verts e) (mj:pl-verts e)))
        (setq closed (= 1 (logand 1 (fix (if (cdr (assoc 70 d)) (cdr (assoc 70 d)) 0)))))
        (if (and closed (>= (length verts) 3))
          (progn
            (setq box (mj:verts-box verts))
            (setq frames (append frames (list box)))
          )
          (setq segs (append segs (mj:verts->segs verts nil)))
        )
      )
      ((or (= ty "LINE") (= ty "ARC"))
        (setq segs (append segs (mj:ent-segs e)))
      )
    )
    (setq i (1+ i))
  )
  ;; loose lines: group into frames and chain each group into a closed loop
  (if segs
    (progn
      (setq box (mj:segs-box segs))
      (setq tol (/ (distance (list (car box) (cadr box) 0.0)
                             (list (caddr box) (cadddr box) 0.0)) 20000.0))
      (if (< tol 0.000000001) (setq tol 0.000000001))
      (foreach grp (mj:group-segs segs tol)
        (setq loop (mj:assemble grp tol))
        (if (and loop (>= (length loop) 3))
          (setq frames (append frames (list (mj:verts-box loop))))
        )
      )
    )
  )
  frames
)

;; a rising row number for one frame (higher on the sheet = smaller number)
(defun mj:row-of (f h)
  (fix (/ (cadddr f) (* 1.5 h)))
)

(defun mj:drop-one (lst v / out done)
  (setq out '() done nil)
  (foreach x lst
    (if (and (null done) (equal x v))
      (setq done t)
      (setq out (append out (list x)))
    )
  )
  out
)

(defun mj:sort-nums (lst / out best)
  (setq out '())
  (while lst
    (setq best nil)
    (foreach x lst (if (or (null best) (< x best)) (setq best x)))
    (setq out (append out (list best)))
    (setq lst (mj:drop-one lst best))
  )
  out
)

;; sort frames: mode "1" = left to right, mode "2" = top row first then left to right
(defun mj:frame-sort (frames mode / out best hs h)
  (setq out '())
  (setq hs '())
  (foreach f frames (setq hs (append hs (list (- (cadddr f) (cadr f))))))
  (setq hs (mj:sort-nums hs))
  (setq h (if hs (nth (/ (length hs) 2) hs) 1.0))
  (if (<= h 0.0) (setq h 1.0))
  (while frames
    (setq best nil)
    (foreach f frames
      (if (null best)
        (setq best f)
        (cond
          ((= mode "2")
            (if (or (> (mj:row-of f h) (mj:row-of best h))
                    (and (= (mj:row-of f h) (mj:row-of best h))
                         (< (car f) (car best))))
              (setq best f)
            )
          )
          (t
            (if (or (< (car f) (car best))
                    (and (< (abs (- (car f) (car best))) 0.0001)
                         (> (cadddr f) (cadddr best))))
              (setq best f)
            )
          )
        )
      )
    )
    (setq out (append out (list best)))
    (setq frames (mj:drop-one frames best))
  )
  out
)

;; biggest text inside a frame -> used as the PDF file name
(defun mj:clean-text (s / out i c skip depth)
  (setq out "" i 1 depth 0)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (cond
      ((= c "{") (setq depth (1+ depth)) (setq i (1+ i)))
      ((= c "}") (if (> depth 0) (setq depth (1- depth))) (setq i (1+ i)))
      ((= c (mj:bs)) (setq i (+ i 2)))
      ((= depth 0)
        (if (or (= c "\n") (= c "\r"))
          (setq out (strcat out " "))
          (setq out (strcat out c))
        )
        (setq i (1+ i))
      )
      (t (setq i (1+ i)))
    )
  )
  out
)

(defun mj:text-in-frame (p1 p2 / ss i n e d h best besth txt)
  (setq ss (ssget "W" p1 p2 (list (cons 0 "TEXT,MTEXT"))))
  (setq i 0 n (if ss (sslength ss) 0) best nil besth 0.0)
  (while (< i n)
    (setq e (ssname ss i) d (entget e) h (cdr (assoc 40 d)))
    (if (null h) (setq h 0.0))
    (if (> h besth)
      (progn
        (setq besth h)
        (setq txt (mj:clean-text (if (cdr (assoc 1 d)) (cdr (assoc 1 d)) "")))
        (if (> (strlen txt) 0) (setq best txt))
      )
    )
    (setq i (1+ i))
  )
  (if best (mj:safe-name best) nil)
)

;; ---------- diagnostics: show the real -PLOT prompt sequence once ----------
;; insert an external DWG/DXF (e.g. a Tianzheng file that a friend converted
;; to T3) into the current drawing, right on top of the original geometry


;; ---------- proxy (Tianzheng) objects : display and flatten ----------

;; turn every layer on / thaw / unlock, then regenerate - used to find out
;; whether hidden proxy graphics are simply switched off


;; show the proxy graphics that are stored inside the drawing




;; try to explode the proxy objects into plain entities
;; WARNING: on AutoCAD for Mac "explode" on a proxy object DELETES it
;; (there is no Tianzheng engine to convert it).  So this command only ever
;; touches ONE object and asks first.






;; what is really inside this drawing ?






;; turn on the command log, run one dummy -PLOT (last answer = No, so nothing
;; is really printed) and leave the log behind so the prompt order can be read






;; is this a usable output device name ?
(defun mj:device-ok (dev)
  (if (or (null dev) (= dev ""))
    nil
    (if (wcmatch (strcase dev) "*.PC3") t nil)
  )
)

;; size of a file in KB (0 when it does not exist)
(defun mj:file-kb (f / s)
  (setq s (if (and vl-file-size f) (vl-file-size f) nil))
  (if (null s) 0 (fix (/ s 1024.0)))
)

;; list the PDF files of a folder - used to find out where a plot really went
(defun mj:report-pdfs (dir / lst f)
  (if (and vl-directory-files dir)
    (progn
      (setq lst (vl-directory-files dir "*.pdf" 1))
      (if lst
        (progn
          (prompt (strcat "\n  该文件夹里的 PDF（" (itoa (length lst)) " 个）："))
          (foreach f lst (prompt (strcat "\n     " f)))
        )
        (prompt "\n  该文件夹里没有任何 PDF。")
      )
    )
  )
)

;; ask for the PDF output device (Enter keeps the default)
(defun mj:ask-dev (def / s)
  (setq s (getstring T (strcat "\nPDF 输出设备 (直接回车就用 " def ") <" def ">: ")))
  (cond
    ((or (null s) (= s "")) def)
    ((mj:device-ok s) s)
    (t (progn
         (prompt (strcat "\n  设备名无效，改用：" def))
         def
       )
    )
  )
)

;; ask for the PDF output folder (Enter keeps the default, must be absolute)
(defun mj:ask-dir (def / s)
  (setq s (getstring T (strcat "\nPDF 输出文件夹 (直接回车就用 " def ") <" def ">: ")))
  (cond
    ((or (null s) (= s "")) def)
    ((= (substr s 1 1) "/") s)
    (t (progn
         (prompt (strcat "\n  不是完整的路径，改用：" def))
         def
       )
    )
  )
)

;; characters that are not allowed in a file name (built without escapes)
(defun mj:badchars ()
  (list "/" (mj:bs) ":" "*" "?" (chr 34) "<" ">" "|")
)

(defun mj:safe-name (s / out i c)
  (setq out "" i 1)
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (if (member c (mj:badchars))
      (setq out (strcat out "_"))
      (setq out (strcat out c))
    )
    (setq i (1+ i))
  )
  out
)

;; folder of the current drawing (with a trailing slash)
(defun mj:drawing-dir ( / d)
  (setq d (getvar "DWGPREFIX"))
  (if (null d) (setq d ""))
  d
)

(defun mj:drawing-name ( / n)
  (setq n (getvar "DWGNAME"))
  (if (null n) (setq n "drawing"))
  ;; strip the .dwg ending
  (if (wcmatch (strcase n) "*.DWG")
    (setq n (substr n 1 (- (strlen n) 4)))
  )
  (mj:safe-name n)
)

;; all paper space layouts of the current drawing (without "Model")
(defun mj:layouts ( / lst out l)
  (setq lst (if layoutlist (layoutlist) nil))
  (setq out '())
  (foreach l lst
    (if (/= (strcase l) "MODEL")
      (setq out (append out (list l)))
    )
  )
  out
)

;; plot one layout.  file = nil  -> send to the layout's own printer
;;                              -> otherwise write a PDF to that path
;; AutoCAD for Mac -PLOT prompt order, straight from the official Mac help
;; (AutoCAD-MAC-Core :: -PLOT (Command)):
;;
;;   Detailed plot configuration [Yes/No] <No>:
;;   Enter a layout name or [?] <current>:
;;   Enter a page setup name <>:                  <- short form only
;;   Enter an output device name or [?] <current>:
;;   Write the plot to a file [Yes/No] <current>:
;;   Enter file name: <dwgname-layoutname.plt>:
;;   Save changes to layout [Yes/No] <No>:
;;   Proceed with Plot [Yes/No] <Y>:
;;
;; With "detailed = Yes" the page setup prompt does NOT appear; instead:
;;   layout -> device -> paper size -> paper units -> orientation ->
;;   plot upside down -> plot area (Window + 2 points) -> scale -> offset ->
;;   plot styles -> style table -> lineweights -> shade plot ->
;;   write to file -> file name -> save changes -> proceed
;; Layout print.  Uses the *detailed* flow so the plot style table (CTB) can
;; be applied; the plot area stays "Layout", i.e. the layout's own settings.
(defun mj:plot-layout (lay dev ctb file / echo)
  (setq echo (getvar "CMDECHO"))
  ;; NOTE: CMDECHO must stay ON while feeding -PLOT on AutoCAD for Mac
  ;; (with CMDECHO 0 the prompts do not consume the queued arguments and the
  ;;  whole command falls apart - measured on the user's machine).
  (setvar "CMDECHO" 1)
  (if file
    (command "_.-plot" "_Y" lay dev "" "" "" "" "_L"
             "" "_C" "" ctb "" "" file "" "_Y")
    (command "_.-plot" "_Y" lay dev "" "" "" "" "_L"
             "" "_C" "" ctb "" "" "" "_Y")
  )
  (setvar "CMDECHO" echo)
  (if file
    (if (findfile file)
      t
      (progn
        (prompt (strcat "\n  !! 没有生成 " file))
        (mj:report-pdfs (mj:dirname file))
        nil
      )
    )
    t
  )
)

;; paper size shorthand -> the exact name the plot driver expects
(defun mj:paper-name (s / u)
  (if (or (null s) (= s ""))
    ""
    (progn
      (setq u (strcase s))
      (cond
        ((= u "A0")  "ISO A0 (1189.00 x 841.00 MM)")
        ((= u "A0P") "ISO A0 (841.00 x 1189.00 MM)")
        ((= u "A1")  "ISO A1 (841.00 x 594.00 MM)")
        ((= u "A1P") "ISO A1 (594.00 x 841.00 MM)")
        ((= u "A2")  "ISO A2 (594.00 x 420.00 MM)")
        ((= u "A2P") "ISO A2 (420.00 x 594.00 MM)")
        ((= u "A3")  "ISO A3 (420.00 x 297.00 MM)")
        ((= u "A3P") "ISO A3 (297.00 x 420.00 MM)")
        ((= u "A4")  "ISO A4 (297.00 x 210.00 MM)")
        ((= u "A4P") "ISO A4 (210.00 x 297.00 MM)")
        ;; "F" = full bleed, i.e. no printable-area margin on the sheet
        ((= u "A0F") "ISO full bleed A0 (1189.00 x 841.00 MM)")
        ((= u "A1F") "ISO full bleed A1 (841.00 x 594.00 MM)")
        ((= u "A2F") "ISO full bleed A2 (594.00 x 420.00 MM)")
        ((= u "A3F") "ISO full bleed A3 (420.00 x 297.00 MM)")
        ((= u "A4F") "ISO full bleed A4 (297.00 x 210.00 MM)")
        (t s)
      )
    )
  )
)

;; plot a rectangle (window) : the frame around one drawing
;;   scale : "_F" = fit,  otherwise "1=100" style
;;   file  : nil = send to the printer, otherwise the PDF path
(defun mj:plot-window (lay dev p1 p2 scale file paper ctb / echo)
  (setq echo (getvar "CMDECHO"))
  ;; CMDECHO must stay ON here - see the note in mj:plot-layout
  (setvar "CMDECHO" 1)
  ;; detailed flow on Mac (measured on the user's machine):
  ;;   layout -> device -> paper size -> paper units -> orientation ->
  ;;   upside down -> plot area (Window + 2 points) -> scale -> offset ->
  ;;   plot styles? -> style table name -> lineweights? -> shade plot ->
  ;;   file name -> save changes -> proceed      (no page setup prompt!)
  (if file
    (command "_.-plot" "_Y" lay dev paper "" "" "" "_W" p1 p2 scale
             "_C" "" ctb "" "" file "" "_Y")
    (command "_.-plot" "_Y" lay dev paper "" "" "" "_W" p1 p2 scale
             "_C" "" ctb "" "" "" "_Y")
  )
  (setvar "CMDECHO" echo)
  (if file
    (if (findfile file)
      t
      (progn
        (prompt (strcat "\n  !! 没有生成 " file))
        (mj:report-pdfs (mj:dirname file))
        nil
      )
    )
    t
  )
)

;; ---------- batch plot by selecting the frames ----------
(defun mj:plot-frames ( / ss frames k order lay scale dev dir rule paper ctb
                          i n ok fail file fname txt bp p1 p2)
  (prompt "\n请框选要打印的图框（矩形 / 闭合多段线 / 围成矩形的直线）:")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE,POLYLINE,LINE,ARC"))))
  (setq frames (mj:frames-from-sset ss))
  (if (null frames)
    (prompt "\n没有识别到图框。用矩形或闭合多段线把要打印的图纸圈起来再试一次。")
    (progn
      (prompt (strcat "\n识别到 " (itoa (length frames)) " 个图框。"))
      (initget "P D")
      (setq k (getkword "\n输出方式 [导出 PDF(P) / 直接打印到打印机(D)] <P>: "))
      (if (null k) (setq k "P"))
      (initget "1 2")
      (setq order (getkword "\n打印顺序 [从左到右(1) / 先上排再从左到右(2)] <1>: "))
      (if (null order) (setq order "1"))
      (setq frames (mj:frame-sort frames order))
      (initget "M L")
      (setq lay (getkword "\n图框画在哪个空间 [模型空间(M) / 当前布局(L)] <M>: "))
      (if (null lay) (setq lay "M"))
      (setq lay (if (= lay "M") "Model" (getvar "CTAB")))
      (setq scale (getstring T "\n打印比例 [回车=布满图纸，或输入 1=100 这样的比例]: "))
      (if (or (null scale) (= scale "")) (setq scale "_F"))
      (setq paper (getstring T "\n图纸尺寸 [回车=沿用布局里的纸张；\n  A0/A1/A2/A3/A4 (横版)，竖版加 P (如 A3P)，\n  无边距纸张加 F (如 A3F)，边距最省]: "))
      (setq paper (mj:paper-name paper))
      (if (/= paper "")
        (prompt (strcat "\n  纸张将使用: " paper))
      )
      ;; black & white always - no question asked
      (setq ctb (mj:mono-style))
      (if (not (mj:ctb-exists ctb))
        (prompt (strcat "\n  注意: 没找到 " ctb "，将沿用布局里的打印样式设置"))
      )
      (if (= k "P")
        (progn
          (setq dev (mj:ask-dev (mj:get 'plotdev)))
          (setq dir (mj:ask-dir (strcat (mj:drawing-dir) "PDF")))
          (if (not (= (substr dir (strlen dir)) "/")) (setq dir (strcat dir "/")))
          (if (and vl-mkdir vl-catch-all-apply)
            (vl-catch-all-apply 'vl-mkdir (list (substr dir 1 (1- (strlen dir)))))
          )
          (initget "1 2")
          (setq rule (getkword "\n文件命名 [图纸名-序号(1) / 用图框里最大的文字(2)] <1>: "))
          (if (null rule) (setq rule "1"))
        )
        (progn
          (setq dev (getstring T "\n打印机设备名 <回车 = 沿用布局里已设好的打印机>: "))
          (if (null dev) (setq dev ""))
          (setq dir "")
        )
      )
      ;; remember the choices, so "MJPLOTQ" (tool bar button) needs no questions
      (mj:set 'plotmode k)
      (mj:set 'plotorder order)
      (mj:set 'plotpaper paper)
      (mj:set 'plotscale scale)
      (mj:set 'plotctb ctb)
      (mj:set 'plotspace (if (= lay "Model") "M" "L"))
      (if (/= dev "") (mj:set 'plotdev dev))
      (if (/= dir "") (mj:set 'plotdir dir))
      (mj:ini-save)
      (mj:plot-frames-core frames k order lay scale dev paper ctb dir rule)
    )
  )
)

;; shared core: print the given frames with the given settings
(defun mj:plot-frames-core (frames k order lay scale dev paper ctb dir rule
                            / i n ok fail file fname txt bp p1 p2)
  (progn
      (prompt (strcat "\n将按顺序处理 " (itoa (length frames)) " 个图框:"))
      (setq n 0)
      (foreach f frames
        (setq n (1+ n))
        (prompt (strcat "\n   " (itoa n) ")  "
                        (rtos (- (caddr f) (car f)) 2 0) " x "
                        (rtos (- (cadddr f) (cadr f)) 2 0)
                        "   左下 (" (rtos (car f) 2 0) ", " (rtos (cadr f) 2 0) ")"
                        "   右上 (" (rtos (caddr f) 2 0) ", " (rtos (cadddr f) 2 0) ")"))
      )
      (initget "Y N")
      (if (equal "N" (getkword "\n开始吗？[是(Y)/否(N)] <是>: "))
        (prompt "\n已取消。")
        (progn
          (setq bp (mj:sysint "BACKGROUNDPLOT"))
          (if (> bp 0)
            (if *mj-catch*
              (vl-catch-all-apply 'setvar (list "BACKGROUNDPLOT" 0))
              (setvar "BACKGROUNDPLOT" 0)
            )
          )
          (setq i 0 ok 0 fail 0)
          (foreach f frames
            (setq p1 (list (car f) (cadr f) 0.0)
                  p2 (list (caddr f) (cadddr f) 0.0)
                  file nil)
            (if (= k "P")
              (progn
                (setq txt (if (= rule "2") (mj:text-in-frame p1 p2) nil))
                (if (null txt)
                  (setq txt (strcat (mj:drawing-name) "-" (itoa (1+ i))))
                )
                (setq file (strcat dir (mj:safe-name txt) ".pdf"))
                (setq n 1)
                (while (findfile file)
                  (setq file (strcat dir (mj:safe-name txt) "-" (itoa n) ".pdf"))
                  (setq n (1+ n))
                )
              )
            )
            (prompt (strcat "\n[" (itoa (1+ i)) "/" (itoa (length frames)) "] "
                            (if file file "打印机")
                            "\n      纸张: " (if (= paper "") "沿用布局" paper)
                            "   样式: " (if (= ctb ".") "不使用" ctb)
                            "   比例: " (if (= scale "_F") "布满图纸" scale)))
            (if (mj:plot-window lay dev p1 p2 scale file paper ctb)
              (progn
                (setq ok (1+ ok))
                (if file
                  (progn
                    (setq n (mj:file-kb file))
                    (prompt (strcat "\n      已生成 " (itoa n) " KB"))
                    (if (< n 5)
                      (prompt "\n      ⚠ 内容几乎是空的：请检查这个图框是否框住了图形")
                    )
                  )
                )
              )
              (setq fail (1+ fail))
            )
            (setq i (1+ i))
          )
          (if (> bp 0)
            (if *mj-catch*
              (vl-catch-all-apply 'setvar (list "BACKGROUNDPLOT" bp))
              (setvar "BACKGROUNDPLOT" bp)
            )
          )
          (prompt (strcat "\n完成：成功 " (itoa ok) " 个"
                          (if (> fail 0) (strcat "，失败 " (itoa fail) " 个") "")))
          (if (and (= k "P") (> ok 0)) (prompt (strcat "\nPDF 文件夹： " dir)))
        )
      )
  )
)

(defun mj:pick-layouts (all / s nums i n out k)
  (prompt "\n图纸里的布局：")
  (setq i 1)
  (foreach l all
    (prompt (strcat "\n   " (itoa i) " = " l))
    (setq i (1+ i))
  )
  (setq s (getstring T "\n输入要打印的布局序号 (用逗号隔开，例如 1,3,4): "))
  (setq out '() nums (mj:split s ","))
  (foreach k nums
    (setq n (atoi k))
    (if (and (> n 0) (<= n (length all)))
      (setq out (append out (list (nth (1- n) all))))
    )
  )
  out
)

(defun c:MJPLOT ( / mode)
  (mj:init)
  (mj:on)
  (prompt "\n============ MJ 批量打印 ============")
  (initget "F L")
  (setq mode (getkword "\n打印方式 [框选图框批量打印(F) / 按图纸布局打印(L)] <F>: "))
  (if (null mode) (setq mode "F"))
  (if (= mode "F") (mj:plot-frames) (mj:plot-layouts))
  (mj:off)
  (princ)
)

;; ---- quick batch plot: uses the settings remembered from the last run ----
;; toolbar button -> select frames -> done, no questions at all
(defun c:MJPLOTQ ( / ss frames k order lay scale dev paper ctb dir rule)
  (mj:init)
  (mj:on)
  (setq k (mj:get 'plotmode) order (mj:get 'plotorder)
        scale (mj:get 'plotscale) paper (mj:get 'plotpaper)
        ctb (mj:mono-style) dev (mj:get 'plotdev) dir (mj:get 'plotdir))
  (prompt "\n======= MJ 批量打印 · 快速 =======")
  (prompt (strcat "\n  输出方式: " (if (= k "D") "直接打印到打印机" "导出 PDF")))
  (if (= k "P")
    (progn
      (prompt (strcat "\n  PDF 设备: " dev))
      (prompt (strcat "\n  PDF 文件夹: " (if (= dir "") (strcat (mj:drawing-dir) "PDF") dir)))
    )
  )
  (prompt (strcat "\n  纸张尺寸: " (if (= paper "") "沿用布局里已设好的" paper)))
  (prompt (strcat "\n  打印样式: " ctb "（固定黑白）"))
  (prompt (strcat "\n  打印比例: " (if (= scale "_F") "布满图纸" scale)))
  (prompt "\n请框选图框:")
  (setq ss (ssget (list (cons 0 "LWPOLYLINE,POLYLINE,LINE,ARC"))))
  (setq frames (mj:frames-from-sset ss))
  (if (null frames)
    (prompt "\n没有识别到图框。")
    (progn
      (setq frames (mj:frame-sort frames order))
      (setq lay (if (= (mj:get 'plotspace) "L") (getvar "CTAB") "Model"))
      (if (and (= k "P") (= dir ""))
        (setq dir (strcat (mj:drawing-dir) "PDF"))
      )
      (if (and (= k "P") (/= dir ""))
        (progn
          (if (not (= (substr dir (strlen dir)) "/")) (setq dir (strcat dir "/")))
          (if (and vl-mkdir vl-catch-all-apply)
            (vl-catch-all-apply 'vl-mkdir (list (substr dir 1 (1- (strlen dir)))))
          )
        )
      )
      (setq rule "1")
      (mj:plot-frames-core frames k order lay scale dev paper ctb dir rule)
    )
  )
  (mj:off)
  (princ)
)

(defun mj:plot-layouts ( / k scope lays dev dir rule i n ok fail file fname all bp ctb)
  (mj:init)
  (prompt "\n[按布局打印]")
  (setq all (mj:layouts))
  (if (null all)
    (prompt "\n当前图纸里没有布局（只有模型空间），请先建好布局。")
    (progn
      ;; ---- 1. output mode ----
      (initget "P D")
      (setq k (getkword "\n输出方式 [导出 PDF(P) / 直接打印到打印机(D)] <P>: "))
      (if (null k) (setq k "P"))
      ;; ---- 2. which layouts ----
      (initget "A C S")
      (setq scope (getkword "\n打印范围 [全部布局(A) / 当前布局(C) / 挑选布局(S)] <A>: "))
      (if (null scope) (setq scope "A"))
      (cond
        ((= scope "C") (setq lays (list (getvar "CTAB"))))
        ((= scope "S") (setq lays (mj:pick-layouts all)))
        (t (setq lays all))
      )
      (if (null lays)
        (prompt "\n没有选择任何布局。")
        (progn
          ;; ---- 3. device / folder / file name ----
          (if (= k "P")
            (progn
              (setq dir (strcat (mj:drawing-dir) "PDF"))
              (setq dir (mj:ask-dir dir))
              (if (not (= (substr dir (strlen dir)) "/"))
                (setq dir (strcat dir "/"))
              )
              (if (and vl-mkdir vl-catch-all-apply)
                (vl-catch-all-apply 'vl-mkdir (list (substr dir 1 (1- (strlen dir)))))
              )
              (setq dev (mj:ask-dev (mj:get 'plotdev)))
              (initget "1 2")
              (setq rule (getkword "\n文件名规则 [图纸名-布局名(1) / 布局名(2)] <1>: "))
              (if (null rule) (setq rule "1"))
            )
            (progn
              (prompt "\n直接打印：使用每个布局自己的打印机与页面设置。")
              (setq dev "")
              (setq dir "")
            )
          )
          ;; black & white always - no question asked
          (setq ctb (mj:mono-style))
          (if (not (mj:ctb-exists ctb))
            (prompt (strcat "\n  注意: 没找到 " ctb "，将沿用布局里的打印样式设置"))
          )
          ;; ---- 4. summary and go ----
          (prompt (strcat "\n将处理 " (itoa (length lays)) " 个布局:"))
          (foreach lay lays (prompt (strcat "  " lay)))
          (initget "Y N")
          (if (equal "N" (getkword "\n开始打印？[是(Y)/否(N)] <是>: "))
            (prompt "\n已取消。")
            (progn
              (setq bp (mj:sysint "BACKGROUNDPLOT"))
              (if (> bp 0)
                (if *mj-catch*
                  (vl-catch-all-apply 'setvar (list "BACKGROUNDPLOT" 0))
                  (setvar "BACKGROUNDPLOT" 0)
                )
              )
              (setq i 0 ok 0 fail 0)
              (foreach lay lays
                (setq file nil)
                (if (= k "P")
                  (progn
                    (setq fname (if (= rule "2") lay (strcat (mj:drawing-name) "-" lay)))
                    (setq file (strcat dir (mj:safe-name fname) ".pdf"))
                  )
                )
                (prompt (strcat "\n[" (itoa (1+ i)) "/" (itoa (length lays)) "] " lay
                                (if file (strcat "  ->  " file) "  ->  打印机")))
                (if (mj:plot-layout lay dev ctb file)
                  (setq ok (1+ ok))
                  (setq fail (1+ fail))
                )
                (setq i (1+ i))
              )
              (if (> bp 0)
                (if *mj-catch*
                  (vl-catch-all-apply 'setvar (list "BACKGROUNDPLOT" bp))
                  (setvar "BACKGROUNDPLOT" bp)
                )
              )
              (prompt (strcat "\n完成：成功 " (itoa ok) " 个"
                              (if (> fail 0) (strcat "，失败 " (itoa fail) " 个") "")))
              (if (= k "P")
                (progn
                  (prompt (strcat "\nPDF 文件夹： " dir))
                  (mj:report-pdfs dir)
                )
              )
            )
          )
        )
      )
    )
  )
  (mj:off)
  (princ)
)

;; 3) Tianzheng DWG -> T3  -  placeholder


;; 4) uninstall
(defun c:MJUNINSTALL ( / p)
  (mj:init)
  (mj:on)
  (setq p (mj:uninstaller-path))
  (prompt "\n[我的工具] 卸载插件")
  (prompt "\n  卸载会删除插件本体、快捷键、工具栏按钮，并还原被改过的设置文件。")
  (initget "Y N")
  (if (equal "Y" (getkword "\n  确定要卸载吗？[是(Y)/否(N)] <否>: "))
    (if p
      (progn
        (prompt (strcat "\n  卸载程序: " p))
        (if startapp
          (progn
            (prompt "\n  正在为你打开…（若没有自动弹出，请到 Finder 里双击它）")
            (if *mj-catch*
              (vl-catch-all-apply 'startapp (list "open" p))
              (startapp "open" p)
            )
          )
          (prompt "\n  请到 Finder 里双击上面这个文件完成卸载。")
        )
      )
      (prompt "\n  没找到卸载程序，请到安装包目录里双击“卸载插件.command”。")
    )
    (prompt "\n  已取消。")
  )
  (mj:off)
  (princ)
)

;; last try for the two letter name: register it through the ActiveX
;; command API as well (silently ignored when it is not available)
(if (and vlax-add-cmd vl-catch-all-apply)
  (vl-catch-all-apply 'vlax-add-cmd (list "MJ" 'mj:cmd-main))
)

;; ------------------------------------------------------------
;; settings wizard
;; ------------------------------------------------------------

(defun mj:color-name (c)
  (cond ((= c 1) "红") ((= c 2) "黄") ((= c 3) "绿") ((= c 4) "青")
        ((= c 5) "蓝") ((= c 6) "洋红") ((= c 7) "白") ((= c 256) "随层")
        (t (itoa c))
  )
)

(defun mj:show-set ()
  (prompt "\n--------------- 当前设置 ---------------")
  (prompt (strcat "\n  1  图纸单位 ............ " (mj:unit-name (mj:get 'unit))))
  (prompt (strcat "\n  2  小数位数 ............ " (itoa (mj:get 'dec))))
  (prompt (strcat "\n  3  文字高度 ............ "
                  (if (<= (mj:get 'txtheight) 0.0) "自动" (rtos (mj:get 'txtheight) 2 2))))
  (prompt (strcat "\n  4  标注文字样式 ........ "
                  (if (= (mj:get 'textstyle) ".") "当前样式" (mj:get 'textstyle))))
  (prompt (strcat "\n  5  标注格式 ............ " (mj:get 'tpl)))
  (prompt (strcat "\n  6  标注图层 / 颜色 ...... " (mj:get 'txtlayer)
                  " / " (mj:color-name (mj:get 'txtcolor))))
  (prompt (strcat "\n  7  红线图层 / 颜色 / 线宽  " (mj:get 'outlayer)
                  " / " (mj:color-name (mj:get 'outcolor))
                  " / " (rtos (/ (mj:get 'outlw) 100.0) 2 2) "mm"))
  (prompt (strcat "\n  8  加粗方式 ............ "
                  (if (= (mj:get 'outmode) 1) "线宽"
                      (strcat "多段线带宽 " (rtos (mj:get 'outwidth) 2 2)))))
  (prompt (strcat "\n  9  绘制红色轮廓 ........ " (if (= (mj:get 'drawout) 1) "是" "否")))
  (prompt (strcat "\n 10  文字位置 ............ "
                  (if (= (mj:get 'autoplace) 1) "自动(形心)" "手动指定")))
  (prompt (strcat "\n 11  多区域标注合计 ...... " (if (= (mj:get 'total) 1) "是" "否")))
  (prompt (strcat "\n 12  自动打开线宽显示 .... " (if (= (mj:get 'lwdisplay) 1) "是" "否")))
  (prompt (strcat "\n 13  设置文件 ............ "
                  (if (mj:ini-path) (mj:ini-path) "无法写入")))
  (prompt "\n  0  保存并退出")
  (princ)
)

(defun c:MJSET ( / k loop)
  (mj:init)
  (mj:on)
  (setq loop t)
  (while loop
    (mj:show-set)
    (initget "1 2 3 4 5 6 7 8 9 10 11 12 13 0")
    (setq k (getkword "\n输入要修改的项目编号 <0>: "))
    (cond
      ((or (null k) (= k "0")) (setq loop nil))
      ((= k "1")
        (initget "1 2 3 4")
        (setq k (getkword "\n图纸单位 [毫米(1)/厘米(2)/米(3)/不换算(4)] <1>: "))
        (if k (mj:set 'unit (nth (1- (atoi k)) '("mm" "cm" "m" "none"))))
      )
      ((= k "2")
        (setq k (getint (strcat "\n小数位数 0-4 <" (itoa (mj:get 'dec)) ">: ")))
        (if k (mj:set 'dec (max 0 (min 4 k))))
      )
      ((= k "3")
        (setq k (getreal (strcat "\n文字高度 (0 = 自动) <"
                                 (rtos (mj:get 'txtheight) 2 2) ">: ")))
        (if k (mj:set 'txtheight k))
      )
      ((= k "4")
        (setq k (getstring T (strcat "\n文字样式名 (\".\" = 当前样式) <"
                                     (mj:get 'textstyle) ">: ")))
        (if (and k (/= k "")) (mj:set 'textstyle k))
      )
      ((= k "5")
        (prompt "\n通配符: {A} 面积+单位  {P} 周长+单位  {a} 面积数值  {p} 周长数值  {n} 换行")
        (setq k (getstring T (strcat "\n标注格式 <" (mj:get 'tpl) ">: ")))
        (if (and k (/= k "")) (mj:set 'tpl k))
      )
      ((= k "6")
        (setq k (getstring T (strcat "\n标注图层名 <" (mj:get 'txtlayer) ">: ")))
        (if (and k (/= k "")) (mj:set 'txtlayer k))
        (setq k (getint (strcat "\n标注颜色 (1红 2黄 3绿 4青 5蓝 6洋红 7白 256随层) <"
                                (itoa (mj:get 'txtcolor)) ">: ")))
        (if k (mj:set 'txtcolor k))
      )
      ((= k "7")
        (setq k (getstring T (strcat "\n红线图层名 <" (mj:get 'outlayer) ">: ")))
        (if (and k (/= k "")) (mj:set 'outlayer k))
        (setq k (getint (strcat "\n红线颜色 <" (itoa (mj:get 'outcolor)) ">: ")))
        (if k (mj:set 'outcolor k))
        (setq k (getreal (strcat "\n红线线宽 mm (0.13 0.25 0.35 0.50 0.70 1.00 1.40 2.00) <"
                                 (rtos (/ (mj:get 'outlw) 100.0) 2 2) ">: ")))
        (if k (mj:set 'outlw (fix (+ 0.5 (* 100.0 k)))))
      )
      ((= k "8")
        (initget "1 2")
        (setq k (getkword "\n加粗方式 [按线宽(1)/多段线带宽(2)] <1>: "))
        (if k
          (progn
            (mj:set 'outmode (atoi k))
            (if (= (atoi k) 2)
              (progn
                (setq k (getreal (strcat "\n带宽 (图纸单位) <"
                                         (rtos (mj:get 'outwidth) 2 2) ">: ")))
                (if k (mj:set 'outwidth k))
              )
            )
          )
        )
      )
      ((= k "9") (mj:set 'drawout (if (= (mj:get 'drawout) 1) 0 1)))
      ((= k "10") (mj:set 'autoplace (if (= (mj:get 'autoplace) 1) 0 1)))
      ((= k "11") (mj:set 'total (if (= (mj:get 'total) 1) 0 1)))
      ((= k "12") (mj:set 'lwdisplay (if (= (mj:get 'lwdisplay) 1) 0 1)))
      ((= k "13")
        (prompt (strcat "\n设置保存在: "
                        (if (mj:ini-path) (mj:ini-path) "（没有找到可写目录）")))
      )
    )
  )
  (if (mj:ini-save)
    (prompt "\n设置已保存。")
    (prompt "\n警告: 设置没能写入文件（只在本次会话中有效）。")
  )
  (mj:off)
  (princ)
)

;; ------------------------------------------------------------
;; clear annotations
;; ------------------------------------------------------------

(defun mj:erase-layer (name / ss n)
  (setq ss (ssget "X" (list (cons 8 name))))
  (if ss
    (progn
      (setq n (sslength ss))
      (if (mj:cmd 8 ss)
        (prompt (strcat "\n已删除 " (itoa n) " 个对象 (图层: " name ")"))
        (prompt "\n删除失败（图层可能被锁定）。")
      )
    )
    (prompt (strcat "\n图层 " name " 上没有对象。"))
  )
)

(defun c:MJCLR ( / k)
  (mj:init)
  (mj:on)
  (prompt "\n[1] 只删标注文字   [2] 只删红色轮廓   [3] 两个都删   [0] 取消")
  (initget "1 2 3 0")
  (setq k (getkword "\n请选择 <0>: "))
  (cond
    ((= k "1") (mj:erase-layer (mj:get 'txtlayer)))
    ((= k "2") (mj:erase-layer (mj:get 'outlayer)))
    ((= k "3") (progn (mj:erase-layer (mj:get 'txtlayer))
                      (mj:erase-layer (mj:get 'outlayer))))
    (t (prompt "\n已取消。"))
  )
  (mj:off)
  (princ)
)

;; ------------------------------------------------------------
;; diagnostics
;; ------------------------------------------------------------

(defun mj:selftest ( / m1 m2 ok)
  (setq ok t)
  (setq m1 (mj:poly-metrics
             (list (list '(0 0 0) 0.0) (list '(100 0 0) 0.0)
                   (list '(100 100 0) 0.0) (list '(0 100 0) 0.0)) T))
  (prompt (strcat "\n  自检1  100x100 正方形 -> 面积 " (rtos (car m1) 2 2)
                  "  周长 " (rtos (cadr m1) 2 2)
                  "  形心 " (rtos (caddr m1) 2 1) "," (rtos (cadddr m1) 2 1)))
  (if (or (> (abs (- (car m1) 10000.0)) 0.01)
          (> (abs (- (cadr m1) 400.0)) 0.02))
    (setq ok nil)
  )
  (setq m2 (mj:poly-metrics
             (list (list '(0 0 0) 1.0) (list '(100 0 0) 1.0)) T))
  (prompt (strcat "\n  自检2  R50 圆(两个半圆) -> 面积 " (rtos (car m2) 2 2)
                  "  周长 " (rtos (cadr m2) 2 2)))
  (if (or (> (abs (- (car m2) 7853.98)) 1.0)
          (> (abs (- (cadr m2) 314.16)) 0.1))
    (setq ok nil)
  )
  (if ok
    (prompt "\n  自检结果: 通过")
    (prompt "\n  自检结果: 失败 (请把以上内容发给我)")
  )
)

(defun c:MJINFO ( / )
  (mj:init)
  (mj:on)
  (prompt (strcat "\n========= MJArea " *mj-ver* " 运行信息 ========="))
  (prompt (strcat "\n插件文件   : " (if (findfile "MJArea.lsp")
                                       (findfile "MJArea.lsp")
                                       "未在支持路径中找到")))
  (prompt (strcat "\n设置文件   : " (if (mj:ini-path) (mj:ini-path) "不可写")))
  (prompt (strcat "\nActiveX    : " (if (mj:com-on) "可用" "不可用(使用内置算法)")))
  (prompt (strcat "\nSECURELOAD : " (mj:sysvar "SECURELOAD")
                  "    APPAUTOLOAD : " (mj:sysvar "APPAUTOLOAD")))
  (prompt (strcat "\nTRUSTEDPATHS : " (mj:sysvar "TRUSTEDPATHS")))
  (prompt (strcat "\n当前单位   : " (mj:unit-name (mj:get 'unit))
                  "   标注样式 : " (mj:text-style)
                  "   文字图层 : " (mj:get 'txtlayer)))
  (mj:selftest)
  (mj:off)
  (princ)
)

;; Make the plug-in folders trusted so AutoCAD stops asking on every start.
(defun c:MJTRUST ( / sc before after)
  (mj:init)
  (mj:on)
  (setq sc (mj:sysint "SECURELOAD"))
  (setq before (mj:sysvar "TRUSTEDPATHS"))
  (prompt (strcat "\nSECURELOAD = " (mj:sysvar "SECURELOAD")))
  (prompt (strcat "\n原有受信任位置: " (if (= before "") "（空）" before)))
  (prompt "\n需要加入的目录:")
  (foreach p (mj:trust-list) (prompt (strcat "\n  " p)))
  (setq after (mj:trust-add))
  (prompt (strcat "\n现在的受信任位置: " after))
  (if (> sc 0)
    (prompt "\n完成：以后启动 AutoCAD 不会再弹“文件加载 - 安全问题”对话框。")
    (prompt "\n说明：SECURELOAD = 0，本身就不会弹这个对话框。")
  )
  (mj:off)
  (princ)
)

;; ------------------------------------------------------------
;; help
;; ------------------------------------------------------------

(defun c:MJHELP ( / )
  (prompt "\n============ MJArea 面积/周长标注 ============")
  (prompt "\nMJ      两种用法:")
  (prompt "\n  (1) 在封闭区域内部点一下 -> 自动 BO 生成闭合边界，")
  (prompt "\n      在该区域中心写出面积、周长文字，并画出红色加粗轮廓；")
  (prompt "\n      可以连着点多个区域，最后回车结束。")
  (prompt "\n  (2) 输入 S 或直接回车 -> 选择已有线框:")
  (prompt "\n      直线/圆弧拼成的闭合框、闭合多段线、圆、椭圆、闭合样条、")
  (prompt "\n      面域、图案填充，可多选、可框选。")
  (prompt "\nMJP     同 MJ（点取内部点入口，方便单独设快捷键）")
  (prompt "\nMJSET   设置: 单位 / 小数位 / 文字高度 / 图层 / 红色线宽 / 格式")
  (prompt "\nMJCLR   删除本插件生成的标注文字和红线")
  (prompt "\nMJINFO  显示运行信息 + 内置自检")
  (prompt "\nMJHELP  显示本帮助")
  (prompt "\nMJTRUST 把插件目录加入“受信任位置”（消除每次启动的加载询问）")
  (prompt "\nMJPLOT  批量打印：把图纸里的布局批量导出 PDF，或直接送打印机")
  (prompt "\nMJPLOTQ 快速批量打印：用上次的设置，框选图框后直接出图（工具栏按钮）")
  (prompt "\nMJA     与 MJ 相同（备用名称）")
  (prompt "\n")
  (prompt "\n我的工具（工具栏）: 面积周长 / 批量打印")
  (prompt "\n我的工具（菜单栏）: 面积周长 / 批量打印 / 卸载插件")
  (prompt "\n可标注: 闭合多段线、圆、椭圆、闭合样条、面域、填充。")
  (prompt "\n设置保存在 MJArea.ini 中，下次打开图纸依然有效。")
  (princ)
)

;; ------------------------------------------------------------
;; load banner
;; ------------------------------------------------------------
(if (null *mj-loaded*)
  (progn
    (setq *mj-loaded* t)
    (prompt (strcat "\nMJArea " *mj-ver*
                    " 已加载  (面积/周长标注 + 红线加粗)"))
    (prompt "\n命令: MJ (点内部点自动BO / 输 S 选线框)   备用名 MJA MJP")
    (prompt "\n设置 MJSET   清除 MJCLR   自检 MJINFO   帮助 MJHELP")
    (prompt (strcat "\n自检: "
                    (if (and mj:cmd-main mj:header mj:assemble)
                      "内部函数正常"
                      "内部函数缺失，请把这一行发给作者"
                    )))
  )
)
(princ)
;; always plot black & white: pick the mono table that matches the drawing
;; (colour dependent drawings use .ctb, named plot style drawings use .stb)
(defun mj:mono-style ( / m)
  (setq m (mj:sysint "PSTYLEMODE"))
  (if (= m 1) "monochrome.stb" "monochrome.ctb")
)

;; folder(s) where plot style tables live (they are NOT in the search path,
;; so findfile alone cannot find them)
(defun mj:style-dirs ( / sup out d)
  (setq out '())
  (setq sup (findfile "MJArea.lsp"))
  (if sup
    (progn
      ;; .../<lang>/Support/MJArea.lsp -> .../<lang> -> .../<lang>/Plotters/Plot Styles
      (setq d (mj:dirname (mj:dirname sup)))
      (if d
        (progn
          (setq d (strcat d "/Plotters/Plot Styles"))
          (setq out (append out (list d)))
        )
      )
    )
  )
  (setq d (mj:sysvar "PLOTSTYLEPATH"))
  (if (and d (/= d "")) (setq out (append out (list d))))
  out
)

(defun mj:ctb-exists (name / ok p)
  (setq ok nil)
  (if (findfile name) (setq ok t))
  (foreach p (mj:style-dirs)
    (if (and (not ok) (findfile (strcat p "/" name))) (setq ok t))
  )
  ok
)

;; plot style table : ask once, keep default from the settings
