(defun C:ZP ()
(setvar "cmdecho" 0)
(command ".zoom" "P")
(princ)
)
(defun C:ZE ()
(setvar "cmdecho" 0)
(command ".zoom" "E" )
(princ)
)
(defun C:ZO ()
(setvar "cmdecho" 0)
(command ".zoom" "0.5x")
(princ)
)