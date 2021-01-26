;;;       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;;;       +              Viewport Lock/Unlock                      +
;;;       +            Created by C. Alan Butler                   +
;;;       +             Copyright 2005-2006                        +
;;;       +   by Precision Drafting & Design All Rights Reserved.  +
;;;       +    Contact at ab2draft@TampaBay.rr.com                 +
;;;       +          look for updates in TheSwamp.org              +
;;;       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED 
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR 
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED. 
;;;   
;;;  You are hereby granted permission to use, copy and modify this 
;;;  software without charge, provided you do so exclusively for 
;;;  your own use or for use by others in your organization in the 
;;;  performance of their normal duties, and provided further that 
;;;  the above copyright notice appears in all copies and both that
;;;  copyright notice and the limited warranty and restricted rights
;;;  notice below appear in all supporting documentation.
;;;  
;;;  Version 05.12.07

;;; The following are several routines for locking & unlocking viewports
;;; They are dependent on subroutines vplock & vp_sel 
;;;
;;;  c:vpunlockall   unlock all vp & set color to green
;;;  c:vplockall     lock all vp and set color to red
;;;  c:vplock        lock one vp & set color to red
;;;  c:vpunlock      unlock one vp & set color to green
;;;  c:vptogle       toggle vp lock & color until ENTER
;;;  c:vpl           choice to user to lock or unlock, one or all
;;;  c:vplockctab    lock all vp in ctab, set color to red
;;;  c:vpunlockctab  UNlock all vp in ctab, set color to green
;;;
;;;  vplock  sub     lock or unlock one or all vp and set color 
;;;  vp_sel  sub     user pick vp - return the ename of vp or nil 


;;;      Viewport Colors
;;;  Viewports will have there color changed to the following:
;;;   use any valid color number or
;;;  acRed, acYellow, acGreen, acCyan, acBlue, acMagenta, acWhite
(setq *vpColorLk    acRed      ; Lock Color
      *vpColorUnLk  acGreen    ; UnLock Color
)
;;; Prevent color change by using -1
;;; (setq *vpColorLk  -1  *vpColorUnLk -1)
;;; To return to Color ByLayer use 256



;;;  I prefer buttons but for the command liners
;;;  Routine Short Cuts
    (defun c:vplo()  (c:vplock))      
    (defun c:vpla()  (c:vplockall))
    (defun c:vpulo() (c:vpunlock))   
    (defun c:vpula() (c:vpunlockall)) 
    (defun c:vpt()   (c:vptogle))    
    (defun c:vplc()  (c:vplockctab))  
    (defun c:vpulc() (c:vpunlockctab))

;;=====================================================

;;;  05.03.07  Added 2 routines to this file
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  force vp's to layer  "Viewport Borders"
;;  ignore vp's on layer "vpPlot"      
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  (defun c:vp2LayAll () (vp2lay ""))

;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  Find vp not on layer "Viewport Borders"      
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  Report findigs to command Line
;;  (defun c:vpFind (/ vpLayer lay obj result id1) 

;;=====================================================



;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;      unlock all vp & set color to green       
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vpunlockall () 
  (vplock nil nil *vpColorUnLk nil)
  (prompt "\n***  All viewports are UnLocked  ***")
  (princ)
)

;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;       lock all vp and set color to red        
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vplockall () 
  (vplock nil t *vpColorLk nil)
  (prompt "\n***  All viewports are Locked  ***")
  (princ)
)

;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;       lock one vp & set color to red          
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vplock (/ vp)
  (if (and (setq vp (vp_sel nil))
           (vplock vp t *vpColorLk nil)
      )
    (prompt "\n***  Viewport is Locked  ***")
    (prompt "\n***  Not Locked  ***")
  )
  (princ)
)


;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;      unlock one vp & set color to green       
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vpunlock (/ vp)
  (if (and (setq vp (vp_sel nil))
           (vplock vp nil *vpColorUnLk nil)
      )
    (prompt "\n***  Viewport is Unlocked  ***")
    (prompt "\n***  Not Unlock  ***")
  )
  (princ)
)



;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;     toggle vp lock & color until ENTER        
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vptogle (/ vp)
  (vl-load-com)
  (while (setq vp (vp_sel nil))
    (cond
     ((eq :vlax-true (vla-get-displaylocked
                       (vlax-ename->vla-object vp)))
      (vplock vp nil *vpColorUnLk nil)
      (prompt "\n***  Viewport is UnLocked  ***")
     )
     (t
       (vplock vp t *vpColorLk nil)
       (prompt "\n***  Viewport is Locked  ***")
     )
    )
  )
  (princ)
)

;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;   lock all vp in current tab, set color to red    
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vplockctab()
  (if (= (getvar "ctab") "Model")
    (prompt "\n***  Command not allowed in Model Space  ***")
    (progn
      (vplock nil t *vpColorLk (getvar "ctab"))
      (prompt "\n***  All viewports in this tab are Locked  ***")
    )
  )
  (princ)
)
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;   UNlock all vp in current tab, set color to green  
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vpunlockctab()
  (if (= (getvar "ctab") "Model")
    (prompt "\n***  Command not allowed in Model Space  ***")
    (progn
      (vplock nil nil *vpColorUnLk (getvar "ctab"))
      (prompt "\n***  All viewports in this tab are UnLocked  ***")
    )
  )
  (princ)
)


;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;    user choice for one or all & lock/unlock   
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  User types vpl ol ENTER at the command line to
;;  run the One Lock routine
(defun c:vpl (/ ans)
  (initget 1 "LO LA UO UA Toggle LC UC")
  (setq ans (getkword "\nEnter option [LockOne LockAll UnlockOne UnlockAll Toggle LockCtab UnlockCtab]"))
  (cond
    ((= ans "LO") (c:vplock))       ; lock one
    ((= ans "LA") (c:vplockall))    ; lock all
    ((= ans "UO") (c:vpunlock))     ; unlock one
    ((= ans "UA") (c:vpunlockall))  ; unlock all
    ((= ans "Toggle") (c:vptogle))  ; toggle lock
    ((= ans "LC") (c:vplockctab))   ; lock all in tab
    ((= ans "UC") (c:vpunlockctab)) ; unlock all in tab
  )
  (princ)
  
)



;;; -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=-
;;;               S u b     F u n c t i o n s                      
;;; -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=- -=<*>=-

;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  lock or unlock one or all vp and set color   
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun vplock (ent lockflag color tabname / lay obj targetobj rtnflag)
  ;; ent   vp or if nil process all viewports in the drawing
  ;; lockflag  true = lock  and nil = unlock
  ;; color = color name or number or nil for default
  ;; tabname = layout tab name, only vps in this tab
  (vl-load-com)
  (if (null color) ; use default colors
    (setq color (if lockflag *vpColorLk *vpColorUnLk))
  )
  (if lockflag
    (setq lockflag :vlax-true)
    (setq lockflag :vlax-false)
  )
  (or (= (type tabname) 'STR) (setq tabname "")) ; default to ALL
  (if (= tabname "Model")
    (progn (alert "Not allowed in Model Space") (quit))
  )
  
  (if ent
    (cond ; looking for object
      ((listp ent)(setq targetobj (vlax-ename->vla-object (car ent))))
      ((= (type ent) 'ENAME) (setq targetobj (vlax-ename->vla-object ent)))
      ((= (type ent) 'VLA-Object) (setq targetobj ent)) 
      (t (alert "Wrong data type in vplock") (quit))
    ) ; end cond
  )
  
  (vlax-for lay ; for each layout
                (vla-get-layouts
                  (vla-get-activedocument
                    (vlax-get-acad-object)
                  )
                )
    (if (and (or (= tabname "")
                 (= (vla-get-name lay) tabname))
             (eq :vlax-false (vla-get-modeltype lay)))
      (vlax-for obj (vla-get-block lay) ; for each obj in layout
        (if (and (= (vla-get-objectname obj) "AcDbViewport")
                 (or (null targetobj)
                     (equal obj targetobj)
                 )
            )
          (progn
            (vla-put-displaylocked obj lockflag)
            (if (not (and (numberp color)(minusp color)))
              (progn
                (vla-put-color obj color)
                (if (setq ent (assoc 340 (entget (vlax-vla-object->ename obj))))
                 (vla-put-color (vlax-ename->vla-object (cdr ent)) color)
                )
              )
            )
            (setq rtnflag t)
          )
        )
      )
    )
  )
  rtnflag
)


;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  user pick vp - return the ename of vp or nil 
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun vp_sel (selflg / vpflag sel-vport entvport vptest)
  ;;  selflg if nil vp is not activated
  
  (if (= (getvar "TileMode") 1) ; in model space
    (alert "****  You must be in Paper Space to run this routine.  ****")
    ;;------------------------------------------------
    (progn ;else in a layout
      (if (/= (getvar "cvport") 1)
        (command "_pspace") ; close the view port
      )
      (setq vpflag (getvar "cvport")) ; get viewport #
      (setvar "ErrNo" 0) ; reset variable

      (while (= vpflag 1) ; No active viewport, Loop until one is picked 
        (setq sel-vport (car (entsel "\nSelect view port: ")))
        (cond
          ((= 52 (getvar "ErrNo")) ; user pressed ENTER, so quit
           (setq vpflag nil)
          )
        ((= sel-vport nil)
          (alert
            "You must select a viewport\r\nor press ENTER to quit\n    --=<  Try again!  >=--"
          )
         )
          ((setq entvport (entget sel-vport))
            (if (and ;; not all vp objects are LWPolylines
                     ;;(= (cdr (assoc 0 entvport)) "LWPOLYLINE")
                     (setq vptest (member '(102 . "{ACAD_REACTORS") entvport))
                     (setq vptest (member '(102 . "}") (reverse vptest)))
                     (assoc 330 vptest)
                )
              (setq entvport (entget (cdr (assoc 330 vptest))))
            )

            (if (= (cdr (assoc 0 entvport)) "VIEWPORT") ;  got the viewport  
              (if selflg
                (progn ; Make VP active
                  (setq vpflag (cdr (assoc 69 entvport)))
                  (command "mspace") ; activate the last vp active
                  (setvar "cvport" vpflag) ; switch to this vp
                )
                (setq vpflag nil) ; no active exit
              )
            )
          )
        ) ;  endif cond  sel-vport 
      ) ;endwhile (= vpFlag 1)
    )
  )
  ;;  return the ename of vp or nil
  (cond (entvport (cdr(assoc -1 entvport))))
)






;;  05.03.07
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  force vp's to layer  "Viewport Borders"
;;  ignore vp's on layer "vpPlot"      
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
(defun c:vp2LayAll () (vp2lay ""))
(defun vp2lay (tabname / lay obj targetobj rtnflag)
  (vl-load-com)
  
;;  CAB version
;;  returns nil if make failed
(defun MakeLayer (lyrname acDoc / lyrobj) 
  (vl-load-com) 
  (if 
    (not 
      (vl-catch-all-error-p 
        (setq lyrobj 
          (vl-catch-all-apply 
              'vla-add 
              (list (vla-get-layers acDoc) lyrname)) 
        ) 
      ) 
    ) 
    lyrobj 
  ) 
) 


  
  (if (null color) ; use default colors
    (setq color (if lockflag *vpColorLk *vpColorUnLk))
  )

  (or (= (type tabname) 'STR) (setq tabname "")) ; default to ALL
  (if (= tabname "Model")
    (progn (alert "Not allowed in Model Space") (quit))
  )
  
  ;; make Layer "Viewport Borders" color=12  NoPlot
  ;;  try to rename first!
  (setq vpLayer "Viewport Borders")
  (if (and (not (tblsearch "layer" vpLayer))
        (setq lyrobj (MakeLayer vpLayer (vla-get-activedocument (vlax-get-acad-object))
       )))
    (progn
      (vla-put-color lyrobj 12)
      (vla-put-plottable lyrobj :vlax-false)
      (vlax-release-object lyrobj)
    )
   )


  
  (vlax-for lay ; for each layout
                (vla-get-layouts
                  (vla-get-activedocument
                    (vlax-get-acad-object)
                  )
                )
    (if (and (or (= tabname "")
                 (= (vla-get-name lay) tabname))
             (eq :vlax-false (vla-get-modeltype lay)))
      (vlax-for obj (vla-get-block lay) ; for each obj in layout
        (if (and (= (vla-get-objectname obj) "AcDbViewport")
                 (or (null targetobj) (equal obj targetobj))
            )
         (if (/= (strcase (vla-get-layer obj)) "VPPLOT")
          (progn
            (vla-put-layer obj vpLayer)
            (if (setq ent (assoc 340 (entget (vlax-vla-object->ename obj))))
              (vla-put-layer (vlax-ename->vla-object (cdr ent)) vpLayer)
            )
            (setq rtnflag t)
          ))
        )
      )
    )
  )
  rtnflag
)



;;  05.03.07
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  Find vp not on layer "Viewport Borders"      
;;  +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
;;  Report findigs to command Line
(defun c:vpFind (/ vpLayer lay obj result id1) 
  (setq vpLayer "Viewport Borders")
  (vlax-for lay (vla-get-layouts(vla-get-activedocument (vlax-get-acad-object)))
    (setq id1 nil) ; ignore the first vp
    (if (eq :vlax-false (vla-get-modeltype lay))
      (progn
        (princ (strcat "\n*** TAB - " (vla-get-name lay) "  ***"))
      (vlax-for obj (vla-get-block lay) ; for each obj in layout
        (if (and (= (vla-get-objectname obj) "AcDbViewport")
                 (or id1 (not (setq id1 t))))
         (if (/= (vla-get-layer obj) vpLayer)
          (progn ; collect vports
            (princ (strcat "\n" (vla-get-name lay) " -> " (vla-get-layer obj)))
            (setq result (cons (List lay obj) result))
          ))
        )
      ))
    )
  )
  (princ)
)