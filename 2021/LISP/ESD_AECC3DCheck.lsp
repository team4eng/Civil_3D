(defun C:ESDC3DCHK ()
		(if (/= (menugroup "Civil") "Civil")
		(progn
			(if (not c:ESDDWGVER)(load "ESD_AECDWGVersion.lsp"))
			(c:ESDDWGVER)
		)
		(progn
			(if (not c:ESDC3DVER)(load "ESD_AECC3DVersion.lsp"))
			(c:ESDC3DVER)
		)
		)
		(princ "Check has been completed")
(princ)
)