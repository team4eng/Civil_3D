(defun C:AVL()
  (setq en(car (entsel "\n Select a line to make horizontal: ")))
  (setq enlist(entget en))
  (setq pnt1 (cdr (assoc 10 enlist)))
  (setq pnt2 (cdr (assoc 11 enlist)))
  (setq ang1 (angle pnt1 pnt2))
  (setq ang2 (/ (* ang1 180) pi))
  (setq ang3 (* ang2 -1))
 
  (command "dview" en "" "tw" ang3 "")

 ;This function rotates the camera in a paperspace viewport using the dview command
;after selecting a line, it uses the angle of that line for input to the twist option of dview
 )

;***
;***	Align view by selecting two points.
;***

(defun C:AVP ( / P1 P2)
 (setq R (getvar "REGENMODE"))
 (setvar "REGENMODE" 1)
 (setvar "CMDECHO" 0)
 (command "UCS" "W")
 (princ "\nAlign view by selecting two points, left to right:")
 (setq P1 (getpoint "\nEnter First Point :"))
 (setq P2 (getpoint P1 "\nSecond Point :"))
 (command "DVIEW" "" "TW" (/ (* -180 (angle P1 P2)) pi) "")
 (command "snapang" P1 P2)
 (setvar "REGENMODE" R)
 (setvar "CMDECHO" 1)
 (princ)
)