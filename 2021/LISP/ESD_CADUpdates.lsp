
; the following command will ensure the 
; appropriate OpenDCL ARX file is loaded.
(command "_OPENDCL")

(defun c:CADUpdates ()
   ; call the method to load the HelloWorld.odcl file.
   (dcl_Project_Load "ESD_CADUpdates" T)
   ; call the method to show the Hello World dialog box example.
   (dcl_Form_Show CADUpdates_Form1)
   (princ)
)


; This fires just before the form is shown.
(defun C:CADUpdates_Form1_OnInitialize (/ fname file line l)
    ; if the file can be found, continue
    (if (setq fname (findfile "C:\\Autodesk\\Civil_3D\\[Common]\\CADUpdates.txt"))
    (progn
      ;open the file, as read only
      (setq file (open fname "r")
        ; set a blank string to start from
        l     ""
      );_setq
      ;read the first line, and if it's not empty, continue
      (if (not (= (setq line (read-line file)) nil))
    ;loop thru the file, line by line, until the current line is empty
    (while (not (= line nil))
      ; if it's the first line, just assign it
      (if (= l "")
        (setq l line)
        ; multiple lines need to have a line break added in
        (setq l (strcat l "\n" line))
      ) ;_if
      (setq line (read-line file))
    ) ;_while
      ) ;_if
      ; close the file now that we're done reading it
      (close file)
      ; one more check to make sure we have something to display
      (if (not (= l ""))
    ;display the text to the user
    (dcl_Control_SetCaption CADUpdates_Form1_Label1 l) ; Change the labels Caption.
      ) ;_if
    ) ;_progn
  ) ;_if
) ;_defun

; This gets called when the OK button is clicked
(defun C:CADUpdates_Form1_TextButton1_OnClicked (/)
   (dcl_Form_Close CADUpdates_Form1) ; Close the form.
   (princ)
)
