(defun c:BRK@ ( / e1)
 (setq e1(entsel "Select object: "))
    (command "break" e1 "f" pause "@" )
      (princ)
)
(defun c:bb () (c:brk-at))