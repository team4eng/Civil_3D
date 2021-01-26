;; EDIT Emmanuel Delay:  the block only rotates, it's not moved to the curve entity
;; https://www.cadtutor.net/forum/topic/67097-align-block-to-object/

;;;¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,;;;
;;;ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,¤º°`°º¤;;;
;;                                                                               ;;
;;                                                                               ;;
;;                          --=={  Block Align  }==--                            ;;
;;                                                                               ;;
;;  Will align a block to a CurveObject [Arc,Line,Polyline,Ellipse,etc], with    ;;
;;  controllable offset and perpendicularity.                                    ;;
;;                                                                               ;;
;;  User is prompted for block specification, block will be inserted if not      ;;
;;  selected. User can also specify whether block should be rotated to suit      ;;
;;  readability.                                                                 ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  BA                                                         ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  AUTHOR:                                                                      ;;
;;                                                                               ;;
;;  Copyright © Lee McDonnell, April 2010. All Rights Reserved.                  ;;
;;                                                                               ;;
;;      { Contact: Lee Mac @ TheSwamp.org, CADTutor.net }                        ;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;  VERSION:                                                                     ;;
;;                                                                               ;;
;;  ø 1.0   ~¤~    29th April 2010   ~¤~   º First Release                       ;;
;;...............................................................................;;
;;  ø 1.1   ~¤~    30th April 2010   ~¤~   º Added Scale option                  ;;
;;...............................................................................;;
;;                                                                               ;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=;;
;;                                                                               ;;
;;;¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,;;;
;;;ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,¤º°`°º¤;;;

(defun c:bas nil (c:BLK_AlignS))

(defun c:BLK_AlignS ( / *error* B! BBOX CURV DOC LLST OBJ OLDPOS OLDROT SEL SPC )
  (vl-load-com)
  ;; Lee Mac  ~  29.04.10

  (defun *error* ( msg )
    (and lLst   (LayerRestore lLst))
    (and OldPos (vla-put-insertionPoint obj OldPos))
    (and OldRot (vla-put-Rotation obj OldRot))
    (and b!     (not (vlax-erased-p Obj))
      (vl-catch-all-apply
        (function vla-delete) (list obj)
      )
    )
    (or (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " msg " **")))
    (princ)
  )
           

  (setq spc  (if (or (eq AcModelSpace (vla-get-ActiveSpace
                                        (setq doc (vla-get-ActiveDocument
                                                    (vlax-get-acad-object)))))
                     
                     (eq :vlax-true   (vla-get-MSpace doc)))
               
               (vla-get-ModelSpace doc)
               (vla-get-PaperSpace doc))

        lLst (LayerUnlock))

  (while
    (progn
      (setq Sel
        (SelectionOrText
          (strcat "\nSpecify Block to Align"
            (if (eq "" (getvar 'INSNAME)) ": "
              (strcat " <" (getvar 'INSNAME) "> : ")
            )
          )
          2
        )
      )
      (cond
        (
          (vl-consp sel)

          (if (not (= 4 (length sel)))
            (princ "\n** Invalid Object Selected **")
            (not
              (setq obj
                (vlax-ename->vla-object
                  (car
                    (last sel)
                  )
                )
              )
            )
          )
        )
        (
          (eq 'STR (type sel))

          (if (eq "" sel) (setq sel (getvar 'INSNAME)))

          (if (not (or (tblsearch "BLOCK" sel)
                       (setq sel (findfile (strcat sel ".dwg")))))
            
            (princ "\n** Block not Found **")
            (if
              (and
                (setq scl
                  (cond
                    (
                      (getreal "\nSpecify Block Scale <1.0> : ")
                    )
                    (
                      1.0
                    )
                  )
                )                    
                (not
                  (vl-catch-all-error-p
                    (setq Obj
                      (vl-catch-all-apply (function vla-InsertBlock)
                        (list spc (vlax-3D-point (getvar 'VIEWCTR)) sel scl scl scl 0.)
                      )
                    )
                  )
                )
              )
              (progn
                (setvar 'INSNAME (vl-filename-base sel))
                (not
                  (setq b!
                    (not
                      (vla-put-Visible Obj :vlax-false)
                    )
                  )
                )                
              )
              (princ "\n** Error Inserting Block **")
            )
          )
        )
      )
    )
  )
 
  (if
    (and Obj
      (setq Curv
        (CurveIfFoo isCurveObject "\nSelect Curve: ")
      )
    )
    (progn
      (initget "Yes No")
      (setq Red (not (eq "No" (getkword "\nMake Block Readable? [Y/N] <Yes> : "))))
      
      (setq bbox (GetBoundingBox Obj))
      (or  b! (and (setq OldPos (vla-get-InsertionPoint Obj))
                   (setq OldRot (vla-get-Rotation Obj))))
      (and b! (vla-put-Visible Obj :vlax-true))

      (setq b!
        (not
          (AlignObjToCurve_mod Obj Curv
            (/ (- (cadadr bbox) (cadar bbox)) 2.) Red
          )
        )
      )
    )
  )

  (LayerRestore lLst)

  (princ)
)   
       

(defun isCurveObject ( ent )
  (not
    (vl-catch-all-error-p
      (vl-catch-all-apply
        (function vlax-curve-getEndParam) (list ent)))))


(defun GetBoundingBox ( obj / ll ur )
  (if (vlax-method-applicable-p obj 'GetBoundingBox)
    (progn
      (vla-getBoundingBox obj 'll 'ur)

      (mapcar (function vlax-safearray->list) (list ll ur))
    )
  )
)  


(defun CurveifFoo ( foo str / sel ent )
  (while
    (progn
      (setq sel (entsel str))
      
      (cond (  (vl-consp sel)

               (if (not (foo (setq ent (car sel))))
                 (princ "\n** Invalid Object Selected **"))))))
  ent)


(defun SelectionOrText ( str cur )
  (and str (princ str))
  (
    (lambda ( result / gr code data )  
      (while
        (progn
          (setq gr (grread t 13 cur) code (car gr) data (cadr gr))

          (cond
            (
              (and (= 3 code) (listp data))

              (setq result (nentselp data)) nil
            )
            (
              (= 2 code)

              (cond
                (
                  (<= 32 data 126)

                  (setq result (strcat result (princ (chr data))))
                )
                (
                  (= 13 data) nil
                )
                (
                  (and (= 8 data) (< 0 (strlen result)))

                  (setq result (substr result 1 (1- (strlen result))))
                  (princ (vl-list->string '(8 32 8)))
                )
                (
                  t
                )
              )
            )
            (
              (= 25 code) nil
            )
            (
              t
            )
          )
        )
      )
      result
    )
    ""
  )
)

(defun AlignObjToCurve_mod ( obj ent o r / msg gr code data pt cAng lAng )
 
  (or *Mac$Per* (setq *Mac$Per* (/ pi 2.)))
  (or *Mac$Off* (setq *Mac$Off* 1.))

  (setq msg  (princ "\n[P]erpendicularity toggle, <Exit>"))
 
  (while
    (progn
      (setq gr (grread 't 15 0) code (car gr) data (cadr gr))
      
      (cond
        (
          (and (= 5 code) (listp data))
          (setq data (trans data 1 0))

          (setq pt   (vlax-curve-getClosestPointto ent data)
                cAng (angle pt data)
                lAng (+ cAng *Mac$Per*))
         
          (and R (setq lAng (MakeReadable lAng)))

          ;;(vla-put-InsertionPoint Obj
          ;;  (vlax-3D-point
          ;;    (polar pt cAng (* o *Mac$Off*))
          ;;  )
          ;;)
          (vla-put-Rotation Obj lAng)
 
          t
        )
        (
          (= 2 code)

          (cond
            (
              (vl-position data '(43 61))

              (setq *Mac$Off* (+ *Mac$Off* 0.1))
            )
            (
              (= 45 data)

              (setq *Mac$Off* (- *Mac$Off* 0.1))
            )
            (
              (vl-position data '(80 112))

              (setq *Mac$Per* (- (/ pi 2.) *Mac$Per*))
            )
            (
              (vl-position data '(13 32))

              nil
            )
            (
              t
            )
          )
        )
        (
          (and (= 3 code) (listp data))
          (setq data (trans data 1 0))

          (setq pt   (vlax-curve-getClosestPointto ent data)
                cAng (angle pt data)
                lAng (+ cAng *Mac$Per*))

          (and R (setq lAng (MakeReadable lAng)))

          ;;(vla-put-InsertionPoint Obj
          ;;  (vlax-3D-point
          ;;    (polar pt cAng (* o *Mac$Off*))
          ;;  )
          ;;)
          (vla-put-Rotation Obj lAng)

          nil
        )
        (
          (= 25 code)

          nil
        )
        (
          t
        )
      )
    )
  )
  data
)


(defun MakeReadable ( a )
  (cond
    (
      (and (> a (/ pi 2)) (<= a pi))

      (- a pi)
    )
    (
      (and (> a pi) (<= a (/ (* 3 pi) 2)))

      (+ a pi)
    )
    (
      a
    )
  )
)

(defun LayerUnlock ( / lock )
  (vlax-for layer
    (vla-get-Layers
      (vla-get-ActiveDocument
        (vlax-get-acad-object)
      )
    )
    (if (eq :vlax-true (vla-get-Lock layer))
      (progn
        (setq lock (cons layer lock))
        (vla-put-lock layer :vlax-false)
      )
    )
  )
  lock
)

(defun LayerRestore ( lst )
  (mapcar
    (function
      (lambda ( layer )
        (vla-put-lock layer :vlax-true)
      )
    )
    lst
  )
)
                  
(princ "\nø¤º°`°º¤ø  BAlign.lsp ~ Copyright © by Lee McDonnell  ø¤º°`°º¤ø")
(princ "\n   ~¤~      ...Type \"BLK_AlignS\" or \"BAS\" to Invoke...        ~¤~   ")
(princ)

;;;¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,;;;
;;                                                                               ;;
;;                             End of Program Code                               ;;
;;                                                                               ;;
;;;ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸¸,¤º°`°º¤;;;﻿﻿