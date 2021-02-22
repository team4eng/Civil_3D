(DEFUN S::STARTUP ()
  (vl-load-com)
  (IF (NOT STARTUP-HAS-RUN)
    (PROGN
      (princ "\nRunning Initial Startup...")
      ;---------------- start stuff to run once per session --------------------
		(command "_indexctl" "3")
		(command "_layernotify" "0")
		(command "_layereval" "0")
		(command "_imageframe" "2")
		(command "_wipeoutframe" "2")
		(command "_pdfframe" "2")  
		(command "_xclipframe" "2") 
		(command "filedia" "1")
		(command "cmddia" "1")
		(command "PICKFIRST" "1")
		(setvar "dynpromt" 1)
		(setvar "WHIPTHREAD" 3)
		(setvar "TOOLTIPS" 1)
		(setvar "PEDITACCEPT" 1)
		(setvar "SELECTIONPREVIEW" 0)
		(setvar "DRAWORDERCTL" 3)
		(setvar "PALETTEOPAQUE" 1)
		(setvar "HPQUICKPREVIEW" 0)
		(setvar "HIGHLIGHT" 1)
		(setvar "ISAVEPERCENT" 75)
		(setvar "PLINEGEN" 1)
		(setvar "dynmode" 3)
		(setvar "INSUNITS" 0)
		(setvar "INSUNITSDEFSOURCE" 0)
		(setvar "INSUNITSDEFTARGET" 0)
		(setvar "TREEDEPTH" 3020)
		(setvar "MAXSORT" 32767)
		(setenv "MAXHATCH" "10000000")
		(setvar "qpmode" 0)
		(setvar "lwdisplay" 0)
		(setvar "hpname" "solid")
		(arxload "DOSLib24x64.arx")
        (if (findfile "C:\\Program Files (x86)\\Common Files\\OpenDCL")(progn (load "ESD_CADUpdates.lsp")(C:CADUpdates)))
      ;---------------- end stuff to run once per session --------------------
    )
  )
  ;set ran flag
  (SETQ STARTUP-HAS-RUN 1)
  (vl-propagate 'STARTUP-HAS-RUN) ;so future dwg's get this variable
  ;---------------- start stuff to run on every drawing --------------------
  (if (findfile "COA_PWD_ESD.lsp")(progn (load "COA_PWD_ESD.lsp")(mystartup)))
  ;(if (findfile "ESD_AECC3DVERSION.lsp")(load "ESD_AECC3DVERSION.lsp"))
  ;(if (findfile "ESD_AECDWGVERSION.lsp")(load "ESD_AECDWGVERSION.lsp"))
  (if (findfile "ESD_AECC3DCheck.lsp")(progn (load "ESD_AECC3DCheck.lsp")(C:ESDC3DCHK)))
  ;---------------- end stuff to run on every drawing --------------------
  (princ "\nDone with acad startup.\n")
  (princ)
)
