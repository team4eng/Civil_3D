;;; Written by: Sam Lucido & Updated by PWDESD
;;; Updated: 01-02-10
;;;
;;; 	Simple routine to display the layer of an object
;;;	selected by using the alert function in AutoCAD
;;;
(prompt "\nESD_ObjectLayer.lsp loaded...  Enter LQ to run  ")
;;;
(defun C:LQ ()
 (prompt
    "\nPick object..... ")
 (setq name (cdr (assoc 8 (entget (car (nentsel))))))
  (alert (strcat "The object is on layer:   " (princ name)))
  (princ)
)
