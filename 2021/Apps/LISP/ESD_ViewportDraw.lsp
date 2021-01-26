(defun c:vg (/ actdoc vpss vpent vport ent entdata enttype ptlist vptype vprad mjrax radrto vpcntps vpcnt vpsc mdspace
             newptlist vpmodel cnt mnpt mspace mxpt rot temp1 temp2 tmplist vpclpd vpen vpid vptwist
            )


  ; Will draw in model space, where the viewport is showing.  If you are in a viewport, then it will draw that one,
  ;  if you are not, then it will let you select the viewport.
  ; Sub's - 'list->variant

  (setq actdoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-endundomark actdoc)
  (vla-startundomark actdoc)
  (setq mspace (vla-get-mspace actdoc))
  (if
    ;; not allowed in Model Layout,
    ;; but in PS get active VP else if not active allow user to select a VP
    (and
      (/= (strcase (getvar "ctab")) "MODEL")
      (if (equal (setq vpid (getvar "cvport")) 1)
        (setq vpen (vp_sel)
              vpss (ssadd vpen)
        )

        (setq vpss (ssget "x"
                          (list '(0 . "VIEWPORT")
                                (cons 69 vpid)
                                (cons 410 (getvar "ctab")) ; limit to current layout
                          )
                   )
        )
      )
      (setq cnt -1) ; loop counter if several vp are selected. not valid with my vp_sel as only one is allowed.
    )
    
     ;;-----------------------------------------------------------
     (while (setq vpent (ssname vpss (setq cnt (1+ cnt))))
       (setq ptlist nil)
       (setq newptlist nil)
       (setq vptype nil)
       (setq vport (vlax-ename->vla-object vpent))
       (setq vpid (cdr (assoc 69 (entget vpent)))) ; CAB moved here

       (if (setq ent (cdr (assoc 340 (entget vpent))))
         (cond
           ((= (setq enttype (cdr (assoc 0 (setq entdata (entget ent))))) "LWPOLYLINE")
            (setq tmplist (vlax-get (vlax-ename->vla-object ent) 'coordinates))
            (while tmplist ; CAB  FlatList->2pair+zero   (A B 0)
              (setq ptlist (cons (list (car tmplist) (cadr tmplist) 0.0) ptlist))
              (setq tmplist (cddr tmplist))
            )
            (setq vptype "Poly")
           )
           ((= enttype "CIRCLE")
            (setq vprad (cdr (assoc 40 entdata)))
            (setq vptype "Circle")
           )
           ((= enttype "ELLIPSE")
            (setq mjrax (cdr (assoc 11 entdata)))
            (setq radrto (cdr (assoc 40 entdata)))
            (setq vptype "Ellipse")
           )
         )

         (progn ; standard viewport
           (vla-getboundingbox vport 'mnpt 'mxpt)
           (setq mnpt (safearray-value mnpt))
           (setq mxpt (safearray-value mxpt))
           (setq temp1 (list (car mxpt) (cadr mnpt) (caddr mnpt)))
           (setq temp2 (list (car mnpt) (cadr mxpt) (caddr mnpt)))
           (setq ptlist (list mnpt temp1 mxpt temp2))
           ;;(setq vpid (cdr (assoc 69 (entget vpent)))) ; moved
         )
       ) ; endif

       (setq vpcntps (vlax-get vport 'center)) ; vp center in PS

       ;;=====================================================================
       ;;  Problem here vl-catch-all-apply never returns nil
       ;|
       (if (vl-catch-all-apply 'vla-put-mspace (list actdoc :vlax-true))
         (progn
           (vla-zoomall (vlax-get-acad-object))
           (vla-put-mspace actdoc ':vlax-true)
         )
       )
       |;


       ;;(vla-setvariable actdoc "cvport" vpid) ; already set

       (vla-put-mspace actdoc :vlax-true) ; CAB
       (setvar "cvport" vpid) ; CAB activate correct vp
       
       ;;====================================================================
       ;;            V i e w p o r t   i s   A C T I V E                     
       ;;====================================================================
       

       (setq vpcnt (trans (getvar "viewctr") 1 0))
  ;;(setq vpcntps (trans vpcntps 0 2)) ; vp center in MS
       (setq vpcntps (trans (trans vpcntps 3 2) 2 0)) ; vp center in MS - new CAB


       (setq mdspace (vla-get-modelspace actdoc))

  ;;(setq ptlist (mapcar '(lambda (x) (trans x 0 2)) ptlist))


       ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

       (setq vpsc (vla-get-customscale vport))
       ;;(command "point" "non" (getvar "viewctr")) ; CAB debug
       (setq vptwist (vla-get-twistangle vport)) ; CAB
       (setq vpclpd (vla-get-clipped vport)) ; CAB


       (if ptlist ; strip Z - CAB
         ;;  translate point list, PS->DCS, DCS->World
         ;;  create a flat list with Z removed
         ;;  convert to safearray
         (setq ptlist (mapcar '(lambda (x) (trans (trans x 3 2) 2 0)) ptlist)
               ptlist (apply 'append (mapcar '(lambda (x) (list (car x) (cadr x))) ptlist))
               ptlist (list->variant ptlist vlax-vbdouble)
         )
       )

       (cond
        ;| ;;  does not account for bulges 
          ((= vptype "Poly")
          (setq vpmodel (vla-addlightweightpolyline mdspace ptlist))
          (if (= (vla-get-closed vpmodel) ':vlax-false)
            (vla-put-closed vpmodel ':vlax-true)
          )
          (vla-move vpmodel (vlax-3d-point vpcntps) (vlax-3d-point vpcnt))
         )|;


         ((= vptype "Circle")
          (setq vpmodel (vla-addcircle mdspace (vlax-3d-point vpcntps) (* vprad (/ 1.0 vpsc))))
         )


         ((= vptype "Ellipse")
          ;;  attempts to deal with DXF code 11 have failed, need more info
          ;(setq mjrax (trans (trans mjrax 3 2) 2 0))
                ;radrto (* radrto (/ 1.0 vpsc)))
          ;(setq vpmodel (vlax-invoke mdspace 'addellipse vpcntps mjrax radrto))
          
          (setq vpmodel (vlax-invoke mdspace 'addellipse vpcntps mjrax radrto))
          (or (zerop vptwist)
              (setq rot (cadr (assoc vptype '(("Poly" -2) ("Ellipse" -1) (nil -2)))))
          )
          (and rot (vla-rotate vpmodel (vlax-3d-point vpcntps) (* rot vptwist))) ; CAB
          (vla-scaleentity vpmodel (vlax-3d-point vpcntps) (/ 1.0 vpsc))
          
          ;(vla-scaleentity vpmodel (vlax-3d-point vpcntps) (/ 1.0 vpsc))
         )
         ( ; standard viewport
          (vla-put-closed (setq vpmodel (vla-addlightweightpolyline mdspace ptlist))
                          :vlax-true
          )
          ;;(vla-move vpmodel (vlax-3d-point vpcntps) (vlax-3d-point vpcnt))
         )
       )

       ;; "Defpoints" layer doesn't work in some cases - CAB
       ;; (vl-catch-all-apply 'vla-put-layer (list vpmodel "Defpoints"))
     )
     (prompt "\n No Viewport active.")
  )
  (vla-put-mspace actdoc mspace) ; restore vp state 
  (vla-endundomark actdoc)
  (and vpmodel (vlax-release-object vpmodel))
  (and actdoc (vlax-release-object actdoc))

  ;;  attemt at preventing the error
  (command "_.line" '(0 0) '(1 1) "")
  (entdel (entlast))
  ;;===============================

  
  (princ)
)
  ;-------------------------------------------
(defun list->variant (lst vartype)
  ; By Frank Oquendo
  (vlax-make-variant
    (vlax-safearray-fill
      (vlax-make-safearray vartype (cons 0 (1- (length lst))))
      lst
    )
  )
)
;;;       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;;;       +                 Viewport Select                        +
;;;       +            Created by C. Alan Butler                   +
;;;       +               Copyright 2005                           +
;;;       +   by Precision Drafting & Design All Rights Reserved.  +
;;;       +    Contact at ab2draft@TampaBay.rr.com                 +
;;;       ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
;;;  Returns the viewport entity name, ignoring second entities used when
;;;  a non-standard viewport is created, circle, ellipse, LWpolyline
(defun vp_sel (/ vpflag sel-vport entvport vptest)
  (if (= (getvar "TileMode") 1) ; in model space
    ;;------------------------------------------------
    (progn
      (alert "****  You must be in Paper Space to run this routine.  ****")
    )
    ;;------------------------------------------------
    (progn ;else in a layout
      (if (/= (getvar "cvport") 1)
        (command "_.pspace") ; close the view port
      )
      (setq vpflag (getvar "cvport")) ; get viewport # 
      (while (= vpflag 1) ; No active viewport, Loop until one is picked 
        (setq sel-vport (car (entsel "\nSelect view port: ")))
        (if (= sel-vport nil)
          (alert
            "You must select a viewport\r\n    --=<  Try again!  >=--"
          )
          (progn
            (setq entvport (entget sel-vport))
            (if (and ;; not all vp objects are LWPolylines
                     ;;(= (cdr (assoc 0 entvport)) "LWPOLYLINE")
                     (setq vptest (member '(102 . "{ACAD_REACTORS") entvport))
                     (setq vptest (member '(102 . "}") (reverse vptest)))
                     (assoc 330 vptest)
                )
              (setq entvport (entget (cdr (assoc 330 vptest))))
            )
            ;;  Make VP active
            (if (= (cdr (assoc 0 entvport)) "VIEWPORT")
              (progn
                (setq vpflag (cdr (assoc 69 entvport)))
                (command "mspace") ; activate the last vp active
                (setvar "cvport" vpflag) ; switch to this vp
              ) ;  endif  viewport 
            )
          )
        ) ;  endif cond  sel-vport 
      ) ;endwhile (= vpFlag 1)
      (command "_.pspace") ; close the view port
    )
  )
  ;;  return the ename of vp or nil
  (cond (entvport (cdr (assoc -1 entvport))))
)