(defun C:testone()
(progn
		(if (not c:ESDC3DVER)(load "ESD_AECC3DVersion.lsp"))
		)
(C:ESDC3DVER)
(command "_indexctl" "3")
(alert "\nCompleted Save.")
(princ)
)
