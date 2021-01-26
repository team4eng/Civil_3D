(defun c:lyr_orderback (/ ss laynm)
(setq laynm (getstring "\n -> Enter the name of layer you wish to move to back of draw order: ")
ss (ssget "X" (list (cons 8 laynm)))
)
(if ss
(command "draworder" ss "" "B")
(princ (strcat "\nNo Objects on Layer "
laynm
"!"
)
)
)
(princ)
)