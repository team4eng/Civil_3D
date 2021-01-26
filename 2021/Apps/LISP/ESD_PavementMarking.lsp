;;;;;; White Pavement Marking

(defun c:PM_SW04 ()
  (SL "04IN" "WHIT-04IN" "Solid" "White")
);_defun

(defun c:PM_SW06 ()
  (SL "06IN" "WHIT-06IN" "Solid" "White")
);_defun

(defun c:PM_SW08 ()
  (SL "08IN" "WHIT-08IN" "Solid" "White")
);_defun

(defun c:PM_SW12 ()
  (SL "12IN" "WHIT-12IN" "Solid" "White")
);_defun

(defun c:PM_SW24 ()
  (SL "24IN" "WHIT-24IN" "Solid" "White")
);_defun

(defun c:PM_DSHW ()
  (SL "04IN" "WHIT-DASH" "10' Dash 30' Gap" "White")
);_defun

(defun c:PM_DDTW ()
  (SL "04IN" "WHIT-DDOT" "10' Dash 30' Gap with Marker" "White")
);_defun

(defun c:PM_SWPK ()
  (SL "04IN" "PRKG" "Parking" "White")
);_defun

(defun c:PM_DW11 ()
  (SL "12IN" "WHIT-1S1G" "1' Dash 1' Gap" "White")
);_defun

(defun c:PM_DW22 ()
  (SL "04IN" "WHIT-2S2G" "2' Dash 2' Gap" "White")
);_defun

(defun c:PM_DW24 ()
  (SL "04IN" "WHIT-2S4G" "2' Dash 4' Gap" "White")
);_defun

(defun c:PM_DW26 ()
  (SL "06IN" "WHIT-2S6G" "2' Dash 6' Gap" "White")
);_defun

(defun c:PM_DW39 ()
  (SL "06IN" "WHIT-3S9G" "3' Dash 9' Gap" "White")
);_defun


;;;;;; Yellow Pavement Marking
 
(defun c:PM_SY04 ()
  (SL "04IN" "YELO-04IN" "Solid" "Yellow")
);_defun

(defun c:PM_SY12 ()
  (SL "12IN" "YELO-12IN" "Solid" "Yellow")
);_defun

(defun c:PM_DSHY ()
  (SL "04IN" "YELO-DASH" "10' Dash 30' Gap" "Yellow")
);_defun

(defun c:PM_DDTY ()
  (SL "04IN" "YELO-DDOT" "10' Dash 30' Gap with Marker" "Yellow")
);_defun

(defun c:PM_DY39 ()
  (SL "06IN" "YELO-3S9G" "3' Dash 9' Gap" "Yellow")
);_defun


;;; Takes the layer suffix as input
(defun SL (sizeSuffix LayerPrefix DashStyle ColorType / layerName oldlayer LayerType LayerID)
  (initget 1 "Survey GIS Civil")
  (setq LayerType (getkword "\nChoose [Survey/GIS/Civil]: "))
  (cond
	( (= LayerType "Survey") (setq LayerID "V-STRP-"))
	( (= LayerType "GIS") (setq LayerID "VI-STRP-"))
	( (= LayerType "Civil") (setq LayerID "C-STRP-"))
	)
  (setq layerName (strcat LayerID LayerPrefix)                ; set the layer you want to work with
    flag (tblsearch "LAYER" layerName)                    ; check if the layer exists
    size (atoi (substr sizeSuffix 1 2)))                    ; store the size as a number
  (alert (strcat "This will create a " LayerType " " (rtos size) "\" " DashStyle " " ColorType " " "Marking"))
  
  ;then do this if exist
  (if flag
    (progn ;grouping statement
      (setq oldlayer (getvar "CLAYER"))                        ; store the current layer
      (setvar "CLAYER" layerName)                        ; set the layer 
      (command "plinewid" (/ size 12.0))                    ; set the pline width as a decimal of the size provided
      (command "_.pline")                            ; start drawing a pline
      (while (= 1 (logand 1 (getvar 'CMDACTIVE)))                ; continue until the user is done
    (command pause)
      );_while
      (command "plinewid" "0")                            ; restore the pline width setting
      (setvar "CLAYER" oldlayer)                        ; restore the previous layer
    );_progn
  ;else - and give warning if it doesn't exist
  (alert (strcat "There is no " layerName " layer. Import the Layer using Design Center"))
  ) ; end if
  (princ)
);_defun
