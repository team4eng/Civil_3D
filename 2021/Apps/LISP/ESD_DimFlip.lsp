;;by David Kozina
(defun c:DIM_FLIP  (/ ss i ent ele)
  (prompt "n* Select Dimensions to Flip Text *")
  (setq ss (ssget '((0 . "DIMENSION")))
        i  (1- (sslength ss)))
  (while (not (minusp i))
    (setq ent (ssname ss i)
          ele (entget ent))
    (entmod
      (subst (cons 51 (- (abs (cdr (assoc 51 ele))) PI)) (assoc 51 ele) ele))
    (entupd ent)
    (setq i (1- i))))

;;;by Daniel J. Altamura, R.A.
(defun c:DIMFm (/ DIM_CNT DIM_SS1 DIM_OBJ DIM_ANG)
  (defun DTR (A) (* PI (/ A 180.0)))
  (command ".undo" "group")
  (setq DIM_CNT 0
        DIM_SS1
         (ssget '((0 . "DIMENSION"))))
  (if DIM_SS1
    (progn (repeat (sslength DIM_SS1)
             (setq DIM_OBJ (entget (ssname DIM_SS1 DIM_CNT)))
             (setq DIM_ANG (- (cdr (assoc 51 DIM_OBJ)) (DTR 180)))
             (setq DIM_OBJ (subst (cons 51 DIM_ANG) (assoc 51 DIM_OBJ) DIM_OBJ))
             (entmod DIM_OBJ)
             (princ (strcat "\n* Flipping Dimension Text *"
                            (itoa DIM_CNT)
                            " of "
                            (itoa (sslength DIM_SS1))))
             (setq DIM_CNT (1+ DIM_CNT)))
           (princ (strcat "\n* Flipping Dimension Text *"
                          (itoa DIM_CNT)
                          " of "
                          (itoa (sslength DIM_SS1))))))
  (command ".undo" "end")
  (princ))