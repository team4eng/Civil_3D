(defun c:xcopy ( / ns l p ) ;;; Marko Ribar Program 2015 from topic http://www.theswamp.org/index.php?topic=50585.0

(setq l (last (setq ns (nentsel "\nPick nested reference to copy from parent reference"))))

(setq p (cadr ns))

(command "_.-REFEDIT" "_non" p)

(repeat (- (length l) 2)

(command "_N")

)

(command "_O" "_N" "_non" p "" "_Y")

(command "_.COPY" "_non" p "")

(while (< 0 (getvar 'cmdactive))

(command "\\")

)

(command "_.REFSET" "_R" (entlast) "")

(command "_.REFCLOSE" "_D")

(princ)
)


