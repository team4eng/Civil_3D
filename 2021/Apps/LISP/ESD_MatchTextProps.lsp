;*****************************************************************************
;                          TTT.LSP
;This program changes a selected text string to match any other selected text
;string.
;*****************************************************************************

(DEFUN C:TXT_MATCH ()

   (SETQ ED(ENTGET (CAR (NENTSEL "\nPick TEXT to change:"))))
   (SETQ EL (ENTGET (CAR (NENTSEL "\nPick TEXT to match:"))))
 (COMMAND "CMDECHO" "1")
  (SETQ ED (SUBST (CONS 7 (CDR (ASSOC 7 EL)))
                 (ASSOC 7 ED)
                ED
   )
 )
(ENTMOD ED)
  (SETQ ED (SUBST (CONS 40 (CDR (ASSOC 40 EL)))
                 (ASSOC 40 ED)
                ED
   )
)

(ENTMOD ED)
  (SETQ ED (SUBST (CONS 8 (CDR (ASSOC 8 EL)))
                 (ASSOC 8 ED)
                ED
   )
)
(ENTMOD ED)
(SETQ ED (SUBST (CONS 51 (CDR (ASSOC 51 EL)))
                (ASSOC 51 ED)
                ED
   )
)
(ENTMOD ED)
)


(defun c:TXT_MAS (/ s ent ss i e)
  (if (and (setq s (car (entsel "\n Select Single text :")))
           (wcmatch (cdr (assoc 0 (setq ent (entget s)))) "*TEXT")
           (progn (prompt " Select Texts to match ... ")
                  (setq ss (ssget "_:L" '((0 . "*TEXT"))))
           )
      )
    (repeat (setq i (sslength ss))
      (setq e (entget (ssname ss (setq i (1- i)))))
      (entmod
        (append e (list (assoc 7 ent) (assoc 40 ent)))
      )
    )
  )
  (princ)
)

(defun c:TXT_MAR (/ s ent ss i e)
  (if (and (setq s (car (entsel "\n Select Single text :")))
           (wcmatch (cdr (assoc 0 (setq ent (entget s)))) "*TEXT")
           (progn (prompt " Select Texts to match ... ")
                  (setq ss (ssget "_:L" '((0 . "*TEXT"))))
           )
      )
    (repeat (setq i (sslength ss))
      (setq e (entget (ssname ss (setq i (1- i)))))
      (entmod
        (append e (list (assoc 50 ent)))
      )
    )
  )
  (princ)
)

