(defun c:AVX()
   (SETVAR "CMDECHO" 0)
   (command "netload" "PolylineEdit_19.dll" "AVX")
   (SETVAR "CMDECHO" 1)
   (princ)
)

(defun c:DVX()
   (SETVAR "CMDECHO" 0)
   (command "netload" "PolylineEdit_19.dll" "DVX")
   (SETVAR "CMDECHO" 1)
   (princ)
)