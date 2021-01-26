(defun c:TXT_Flip (/ i ss e eLst)

  (if (setq i -1 ss (ssget "_:L" '((0 . "TEXT,MTEXT"))))
    (while (setq e (ssname ss (setq i (1+ i))))
      (setq eLst (entget e))
      (entmod (subst (cons 50 (+ pi (cdr (assoc 50 eLst))))
                     (assoc 50 eLst) eLst))))

  (princ))

(defun c:RotateTxT ( / a e i r s )
    (if (and
            (setq a (getangle "\nSpecify Angle: "))
            (setq s (ssget "_:L" '((0 . "TEXT,MTEXT"))))
        )
        (repeat (setq i (sslength s))
            (setq e (entget (ssname s (setq i (1- i))))
                  r (assoc 50 e)
            )
            (entmod (subst (cons 50 (+ (cdr r) a)) r e))
        )
    )
    (princ)
)

;;  SpinText.LSP
;;  To Rotate Text and/or Mtext entities by the same amount
;;  about each one's individual insertion point.
;;  Kent Cooper, November 2008

(defun C:TXT_Spin (/ ttype selmode trotd trotr textset titem tdata)
  (initget "Text Mtext Both")
  (setq ttype (getkword "Type[s] of text - Text/Mtext/Both (T/M/B) <B>: "))
  (if (or (not ttype) (= ttype "Both")) (setq ttype "*TEXT"))
  (initget "All User")
  (setq
    selmode (getkword "Selection - All in drawing/User selected (A/U) <A>: ")
    trotd (getreal "Rotation (from current) in degrees <180>: ")
    trotr
      (if trotd
        (/ (* trotd pi) 180)
        pi
      ); end if and trotr
  ); end setq
  (if (/= selmode "User")
    (setq textset (ssget "_X" (list (cons 0 ttype))))
    (setq textset (ssget (list (cons 0 ttype))))
  ); end if
  (while (> (sslength textset) 0)
    (setq
      titem (ssname textset 0)
      tdata (entget titem)
      tdata (subst (cons 50 (+ (cdr (assoc 50 tdata)) trotr)) (assoc 50 tdata) tdata)
    ); end setq
    (entmod tdata)
    (entupd titem)
    (ssdel titem textset)
  ); end while
); end defun
(prompt "Type TXT_Spin to rotate Text and/or Mtext entities about their insertion points.")