;
;	Set Working Color
;

(defun c:detlsetlyrs ()
 (princ "\nSetting Layers for detail work...")
 (setvar "cmdecho" 0)
 (Command "cecolor" "ByLayer")
 (Command "celtype" "ByLayer")
 (Command "celweight" "-1")
 (command "cplotstyle" "ByLayer")
 (command "layer" "c" "134" "*" "") 
 (command "layer" "c" "130" "G-ANNO-DETL-DIMS" "")
 (command "layer" "c" "130" "G-ANNO-DETL-DIMS-HIDN" "")
 (command "layer" "c" "130" "G-ANNO-DETL-DIMS-CNTR" "")
 (command "layer" "c" "150" "G-ANNO-DETL-TEXT" "")
 (command "layer" "c" "121" "G-ANNO-DETL-SYMB" "")
 (command "layer" "c" "30" "G-ANNO-DETL-THCK" "")
 (command "layer" "c" "30" "G-ANNO-DETL-THCK-HIDN" "")
 (command "layer" "c" "30" "G-ANNO-DETL-THCK-CNTR" "")
 (command "layer" "c" "60" "G-ANNO-DETL-MEDM" "")
 (command "layer" "c" "60" "G-ANNO-DETL-MEDM-HIDN" "")
 (command "layer" "c" "60" "G-ANNO-DETL-MEDM-CNTR" "")
 (command "layer" "c" "100" "G-ANNO-DETL-FINE" "")
 (command "layer" "c" "100" "G-ANNO-DETL-FINE-HIDN" "")
 (command "layer" "c" "100" "G-ANNO-DETL-FINE-CNTR" "")
 (command "layer" "c" "253" "G-ANNO-DETL-PATT" "")
 (command "layer" "c" "251" "G-ANNO-DETL-PATT-GRAY" "")
 (command "layer" "c" "201" "G-ANNO-TEXT" "")
 (command "layer" "c" "200" "G-ANNO-TITL" "")
 (command "layer" "c" "220" "G-ANNO-TABL" "")
 (command "layer" "c" "223" "G-ANNO-TABL-TEXT" "")
 (command "layer" "c" "7" "G-ANNO-DETL" "")
 (command "layer" "c" "21" "G-ANNO-TTBL" "")
 (command "layer" "c" "22" "G-ANNO-TTBL-TEXT" "")
 (command "layer" "c" "240" "C-ANNO-REDL" "")
 (command "layer" "c" "191" "0" "")
 ; (command "layer" "LO" "G-NPLT" "")
 (while (> (getvar"cmdactive") 0)
  (command "")
 )
 (command "regen")
 (command "cmdecho" 1)
 (princ)
 (princ "\nAll layers are set to go.")
 (princ)
)



;;; By RonJon
;;; Found at http://www.cadtutor.net/forum/showthread.php?41822-changing-text-mtext-to-multileaders...
(defun c:detltxt2ml (/ newleader pt1 pt2 ss txt x w rjp-getbbwdth)
  (command "layer" "s" "G-ANNO-DETL-TEXT" "")
  (c:detltxt2mt)
  (vl-load-com)
  (defun rjp-getbbwdth (obj / out ll ur)
    (vla-getboundingbox obj 'll 'ur)
    (setq out (mapcar 'vlax-safearray->list (list ll ur)))
    (distance (car out) (list (caadr out) (cadar out)))
  )
  (if (setq ss (ssget '((0 . "*TEXT"))))
    (progn (setq txt (apply
		       'strcat
		       (mapcar
			 'cdr
			 (vl-sort
			   (mapcar '(lambda (x)
				      (cons (vlax-get x 'insertionpoint)
					    (strcat (vlax-get x 'textstring) " ")
				      )
				    )
				   (setq
				     ss	(mapcar
					  'vlax-ename->vla-object
					  (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
					)
				   )
			   )
			   (function (lambda (y1 y2) (< (cadr (car y2)) (cadr (car y1))))
			   )
			 )
		       )
		     )
		 w   (car (vl-sort (mapcar 'rjp-getbbwdth ss) '>))
		 txt (apply 'strcat
			    (mapcar 'chr (reverse (cdr (reverse (vl-string->list txt)))))
		     )
	   )
	   (mapcar 'vla-delete ss)
    )
  )
  (if (and (setq pt1 (getpoint "\nSpecify leader arrowhead location: "))
	   (setq pt2 (getpoint pt1 "\nSpecify landing location: "))
      )
    (progn (command "._MLEADER" pt1 pt2 "")
	   (setq newleader (vlax-ename->vla-object (entlast)))
	   (vla-put-textstring newleader txt)
	   (vla-put-textwidth newleader w)
    )
  )
  (princ)
)


;
;	Dimension 
;

(defun c:detldims () 
 (command "layer" "s" "G-ANNO-DETL-DIMS" "") 
 (command "dimrotated")
 )

;Revision Cloud


(defun c:detlrevcloud () 
 (command "layer" "M" "C-ANNO-REDL" "C" "240" "" "") 
 (command "revcloud")
 )





;
;	Dimension Angular
;

(defun c:detldimsa () 
 (command "layer" "s" "G-ANNO-DETL-DIMS" "") 
 (command "dimangular")
 )


;;
;;Dimension Overrides
;;



(defun c:detldimsovr () (c:DimTextOverride))

(defun c:DimTextOverride ( / ss textString)

(princ "\rDIMENSION TEXT OVERRIDE ")

(vl-load-com)

(if (and (setq ss (ssget '((0 . "DIMENSION"))))

(setq textString

(getstring

T

"\nEnter override text, <Enter> to remove override: ")))

(progn

(vla-startundomark

(cond (*activeDoc*)

((setq *activeDoc*

(vla-get-activedocument

(vlax-get-acad-object))))))

(vlax-for oDim

(setq ss (vla-get-activeselectionset *activeDoc*))

(vla-put-textoverride oDim textString))

(vla-delete ss)

(vla-endundomark *activeDoc*))

(prompt "\n** Nothing selected ** "))

(princ))


;
;	Hatch 
;

(defun c:detlhatch () 
 (command "layer" "s" "G-ANNO-DETL-PATT" "") 
 (command "hatch" "ANSI31" "1" "0")  
)

;
;	Hatch Line
;

(defun c:detlhatchl () 
 (command "layer" "s" "0" "") 
 (command "pline")  
)




;;---------------------=={  Text 2 MText Upgraded  }==---------------------------;;
;;                                                                               ;;
;;  Similar to the Txt2MTxt Express Tools function, but allows the user          ;;
;;  additional control over where the text is placed in the resultant MText.     ;;
;;                                                                               ;;
;;  The user can pick MText or DText, positioning such text using one of two     ;;
;;  modes: "New Line" or "Same Line". The Modes can be switched by pressing      ;;
;;  Space between picks.                                                         ;;
;;                                                                               ;;
;;  The user can also hold shift and pick text to keep the original text in      ;;
;;  place, and press "u" between picks to undo the last text pick.               ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  FUNCTION SYNTAX:  T2M                                                        ;;
;;                                                                               ;;
;;  Notes:-                                                                      ;;
;;  --------                                                                     ;;
;;  Shift-click functionality requires the user to have Express Tools installed. ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Author: Lee Mac, Copyright © September 2009 - www.lee-mac.com                ;;
;;                                                                               ;;
;;-------------------------------------------------------------------------------;;
;;                                                                               ;;
;;  Version:                                                                     ;;
;;                                                                               ;;
;;  1.0:  27/09/2009  -  First Release                                           ;;
;;-------------------------------------------------------------------------------;;
;;  1.1:  29/09/2009  -  Minor Bug Fixes                                         ;;
;;-------------------------------------------------------------------------------;;
;;  1.2:  29/09/2009  -  Fixed Alignment Bug                                     ;;
;;                    -  Added Code to match Height                              ;;
;;-------------------------------------------------------------------------------;;
;;  1.3:  01/10/2009  -  Added option to Copy Text.                              ;;
;;-------------------------------------------------------------------------------;;
;;  1.4:  01/10/2009  -  Added option to Undo Last text selection                ;;
;;-------------------------------------------------------------------------------;;
;;  1.5:  30/03/2010  -  Modified code to allow for mis-click.                   ;;
;;                    -  Updated UndoMarks.                                      ;;
;;-------------------------------------------------------------------------------;;
;;  1.6:  15/04/2010  -  MText objects now have correct width.                   ;;
;;                    -  Accounted for %%U symbol.                               ;;
;;-------------------------------------------------------------------------------;;
;;  1.7:  16/04/2010  -  Fixed %%U bug.                                          ;;
;;                    -  Trimmed Spaces when in 'Same Line' mode.                ;;
;;                    -  Fixed Width when Undo is used.                          ;;
;;                    -  Allowed Shift-Click to keep first text object selected. ;;
;;-------------------------------------------------------------------------------;;
;;  1.8:  10/05/2010  -  Allowed for UCS variations.                             ;;
;;                    -  Matched initial text rotation.                          ;;
;;-------------------------------------------------------------------------------;;
;;  1.9:  21/05/2010  -  Added ability to use SelectionSet to select text.       ;;
;;-------------------------------------------------------------------------------;;
;;  2.0:  07/06/2010  -  Fixed offset from cursor with rotated text.             ;;
;;-------------------------------------------------------------------------------;;

(defun c:detltxt2mt ( /  ;; -={ Local Functions }=-

                   *error* align_Mt Get_MTOffset_pt
                   GetTextWidth ReplaceUnderline

                  ;; -={ Local Variables }=-

                  CODE
                  DATA DOC
                  ELST ENT ET
                  FORMFLAG
                  GRDATA
                  LHGT LLST
                  MLST MSG
                  NOBJ NSTR
                  OBJ
                  SHFT SPC
                  TENT TOBJ TEXTSS
                  UFLAG UNDER
                  WLST
              
                  ;; -={ Global Variables }=-

                  ; *T2M_mode*  ~  Mode for line addition
)
  

;     --=={ Sub Functions  }==--      ;

  ;; -={ Error Handler }=-

  (defun *error* (err)
    (and uFlag (vla-EndUndoMark doc))
    (and tObj  (not (vlax-erased-p tObj)) (vla-delete tObj))
    
    (if eLst (mapcar (function entdel)
               (vl-remove-if (function null) eLst)))
    
    (or (wcmatch (strcase err) "*BREAK,*CANCEL*,*EXIT*")
        (princ (strcat "\n** Error: " err " **")))
    (princ))

  
 ;;-------------------------------------------------------------------------------;;
  

  (defun align_Mt (obj / al)
    (cond (  (eq "AcDbMText" (vla-get-ObjectName obj))
             (vla-get-AttachmentPoint obj))

          (  (eq "AcDbText" (vla-get-ObjectName obj))
             (setq al (vla-get-Alignment obj))

             (cond (  (<= 0 al 2) (1+ al))
                   (  (<= 3 al 5) 1)
                   (t (- al 5))))))
  

 ;;-------------------------------------------------------------------------------;;
  

  (defun Get_MTOffset_pt ( obj pt / miP maP al )
    (vla-getBoundingBox obj 'miP 'maP)
    (setq miP (vlax-safearray->list miP)
          maP (vlax-safearray->list maP))

    (setq al (vla-get-AttachmentPoint obj) r (vla-get-rotation obj))

    (cond (  (or (eq acAttachmentPointTopLeft   al)
                 (eq acAttachmentPointTopCenter al)
                 (eq acAttachmentPointTopRight  al))
           
             (polar pt (- r (/ pi 2.)) (vla-get-Height obj)))

          (  (or (eq acAttachmentPointMiddleLeft   al)
                 (eq acAttachmentPointMiddleCenter al)
                 (eq acAttachmentPointMiddleRight  al))
           
             (polar pt (- r (/ pi 2.)) (+ (vla-get-Height obj)
                                          (/ (- (cadr maP) (cadr miP)) 2.))))
  
          (  (or (eq acAttachmentPointBottomLeft   al)
                 (eq acAttachmentPointBottomCenter al)
                 (eq acAttachmentPointBottomRight  al))
           
             (polar pt (- r (/ pi 2.)) (+ (vla-get-Height obj)
                                          (- (cadr maP) (cadr miP)))))))
  

 ;;-------------------------------------------------------------------------------;;
  

  (defun GetTextWidth (obj / tBox eLst)
    (cond (  (eq "AcDbText" (vla-get-objectname obj))

             (setq eLst (entget  (vlax-vla-object->ename obj))
                   tBox (textbox
                          (subst
                            (cons 1 (strcat "..." (cdr (assoc 1 eLst))))
                              (assoc 1 eLst) eLst)))

             (- (caadr tBox) (caar tBox)))

          (  (vla-get-Width obj))))
  

 ;;-------------------------------------------------------------------------------;;
  

  (defun ReplaceUnderline (str / i under)
    (if (vl-string-search "%%U" (strcase Str))
      (progn
        (while (and (< i (strlen Str))
                    (setq i (vl-string-search "%%U" (strcase Str) i)))
          (if under
            (setq Str (strcat (substr Str 1 i) "\\l" (substr Str (+ i 4))) i (+ i 4) under nil)
            (setq Str (strcat (substr Str 1 i) "\\L" (substr Str (+ i 4))) i (+ i 4) under t  )))
        
        (if under (setq str (strcat str "\\l")))))
    
    str)
  

;     --=={ Main Function  }==--
 

  (setq doc (vla-get-ActiveDocument
              (vlax-get-acad-object))

        spc (if (or (eq AcModelSpace (vla-get-activespace doc))
                    (eq :vlax-true   (vla-get-MSpace doc)))

              (vla-get-modelspace doc)
              (vla-get-paperspace doc)))
  
  (setq Et
      (and (vl-position "acetutil.arx" (arx))
           (not
             (vl-catch-all-error-p
               (vl-catch-all-apply
                 (function (lambda nil (acet-sys-shift-down))))))))

  (or *T2M_Mode* (setq *T2M_Mode* 0))
  (setq mLst '("New Line " "Same Line"))

  (while
    (progn
      (setq ent (car (entsel "\nSelect Text/MText [Shift-Click keep original]: ")))
      (and et (setq shft (acet-sys-shift-down)))
      
      (cond (  (not ent)
               (princ "\n** Nothing Selected **"))
            
            (  (not (wcmatch (cdr (assoc 0 (entget ent))) "*TEXT"))
               (princ "\n** Object is not Text **")))))

  (setq uFlag (not (vla-StartUndoMark doc)))

  (setq tObj
    (vla-AddMText spc
      
      (vla-get-InsertionPoint
        (setq obj (vlax-ename->vla-object ent))) (GetTextWidth obj)
      
          (ReplaceUnderline (vla-get-TextString obj))))

  (foreach p '(InsertionPoint Layer Color StyleName Height)
    (vlax-put-property tObj p
      (vlax-get-property obj p)))

  (vla-put-rotation tObj
    (if (eq "AcDbText" (vla-get-ObjectName obj))
      (- (vla-get-rotation obj)
         (angle '(0. 0. 0.)
           (trans (getvar 'UCSXDIR) 0 (trans '(0. 0. 1.) 1 0 t))))
      (vla-get-rotation obj)))
  
  (vla-put-AttachmentPoint tObj (align_Mt obj))

  (or (and shft
           (setq eLst (cons nil eLst)))
      (and (entdel ent)
           (setq eLst (cons ent eLst))))

  (princ (eval (setq msg '(strcat "\n~¤~  Current Mode: " (nth *T2M_mode* mLst) " ~¤~   [Space to Change]"
                                  "\n~¤~ Select Text to Convert [Shift-Click keep original] [Undo] <Place MText> ~¤~"))))

  (while
    (progn
      (setq grdata (grread 't 15 2)
            code   (car grdata) data (cadr grdata))

      (cond (  (and (= 5 code) (listp data))

               (vla-put-InsertionPoint tObj
                 (vlax-3D-point
                   (Get_MTOffset_pt tObj (trans data 1 0)))) t)

            (  (and (= 3 code) (listp data))

               (if (and (setq tEnt (car (nentselp data)))
                        (wcmatch (cdr (assoc 0 (entget tEnt))) "*TEXT"))
                 
                 (AddtoMTextSelection tEnt)

                 (progn
                   (vla-put-Visible tObj :vlax-false)

                   (if (setq textss (GetSelectionSet "\nPick Corner Point: " data '((0 . "TEXT,MTEXT"))))
                     (
                       (lambda ( i )
                         (while (setq e (ssname textss (setq i (1+ i))))
                           (AddtoMTextSelection e)
                         )
                         (princ (eval msg))
                       )
                       -1
                     )
                     (princ (strcat "\n** No Text/MText Selected **" (eval msg)))
                   )
                   
                   (vla-put-Visible tObj :vlax-true) t
                 )
               )
            )

            (  (= 25 code) nil)

            (  (= 2 code)

               (cond (  (= 13 data) nil)
                     
                     (  (= 32 data)
                      
                        (setq *T2M_mode* (- 1 *T2M_mode*))
                        (princ (eval msg)))
                     
                     (  (vl-position data '(85 117))
                      
                        (if (< 1 (length eLst))
                          (progn
                            
                            (vla-put-TextString tObj
                              (substr (vla-get-TextString tObj) 1 (car lLst)))

                            (vla-put-Width tObj (car wLst))
                            
                            (if (car eLst) (entdel (car eLst)))
                            (setq eLst (cdr eLst) lLst (cdr lLst) wLst (cdr wLst)) t)
                          
                          (progn
                            (princ "\n** Nothing to Undo **")
                            (princ (eval msg)))))                           
                            
                     (t )))

            (t ))))

  (setq uFlag (vla-EndUndoMark doc))
  (princ))

(defun AddtoMTextSelection ( tEnt / nStr nObj formflag )
  (setq lLst (cons (strlen (vla-get-TextString tObj)) lLst)
        wLst (cons (vla-get-Width tObj) wLst))
  
  (setq nStr
     (vla-get-TextString
       (setq nObj
          (vlax-ename->vla-object tEnt))) formflag nil)
  
  (vla-put-Width tObj
    ((if (= *T2M_mode* 1) + max)
      (vla-get-Width tObj) (GetTextWidth nObj)))
  
  (if (not (or (eq (vla-get-Color nObj) (vla-get-Color tObj))
             (vl-position (vla-get-Color nObj) '(255 0))))
    
    (setq nStr (strcat "\\C" (itoa (vla-get-Color nObj)) ";" nStr) formflag t))
  
  (setq nStr (ReplaceUnderline nStr))
  
  (if (not (or (eq (vla-get-Height nObj) (vla-get-Height tObj))
             (and lHgt (eq (vla-get-Height nObj) lHgt))))
    
    (setq nStr (strcat "\\H" (rtos (/ (float (vla-get-Height nObj))
                                      (cond (lHgt) ((vla-get-Height tObj)))) 2 2)  "x;" nStr)
      lHgt (vla-get-Height nObj) formflag t))
  
  (if (not (eq (vla-get-StyleName nObj) (vla-get-StyleName tObj)))
    (setq nStr
       (strcat "\\F" (vla-get-fontfile
                       (vla-item
                         (vla-get-TextStyles doc)
                         (vla-get-StyleName nObj))) ";" nStr) formflag t))
  
  (if formflag (setq nStr (strcat "{" nStr "}")))
  
  (vla-put-TextString tObj
    (strcat
      (vla-get-TextString tObj)
      (if (zerop *T2M_mode*)
        (strcat "\\P" nStr)
        (strcat " "  (vl-string-left-trim (chr 32) nStr)))))
  
  (vla-update tObj)
  (or (and et (acet-sys-shift-down)
        (setq eLst (cons nil eLst)))
    (and (entdel tEnt)
      (setq eLst (cons tEnt eLst)))) t)


(defun GetSelectionSet ( str pt filter / gr data pt1 pt2 lst )
  (princ str)

  (while (and (= 5 (car (setq gr (grread t 13 0)))) (listp (setq data (cadr gr))))
    (redraw)

    (setq pt1 (list (car data) (cadr pt) (caddr data))
          pt2 (list (car pt) (cadr data) (caddr data)))

    (grvecs
      (setq lst
        (list
          (if (minusp (- (car data) (car pt))) -30 30)
          pt pt1 pt pt2 pt1 data pt2 data
        )
      )
    )
  )

  (redraw)

  (ssget (if (minusp (car lst)) "_C" "_W") pt data filter)
)

(vl-load-com)
(princ "\n:: Text2MText.lsp | Version 2.0 | © Lee Mac 2009 www.lee-mac.com ::")
(princ "\n:: Type \"T2M\" to Invoke ::")
(princ)


;;
;;
;;

(defun c:detlsetdtext ( / ss1 index newht newstyle styledata defwidth ename txtobj)
(c:detltxt2mt)
(setq ss1 (ssget (list (cons 0 "*text")))
      index 0
      newht "0.1"
      newstyle "COA"
)
(if (setq styledata (tblsearch "style" newstyle))
    (progn
       (setq defwidth (cdr (assoc 41 styledata)))
       (while (setq ename (ssname ss1 index))
              (setq txtobj (vlax-ename->vla-object ename))
              (vlax-put-property txtobj 'Height newht)
              (vlax-put-property txtobj 'StyleName newstyle)
              (if (vlax-property-available-p txtobj 'ScaleFactor)
                  (vlax-put-property txtobj 'ScaleFactor defwidth)
              )
              (setq index (1+ index))
       )
    )
    (princ "\nRequested text style does not exist!")
)
(princ)
(command "chprop" "p" "" "LA" "G-ANNO-DETL-TEXT" "")
)

(defun c:detlsetgtext ( / ss1 index newht newstyle styledata defwidth ename txtobj)
(c:detltxt2mt)
(setq ss1 (ssget (list (cons 0 "*text")))
      index 0
      newht "0.1"
      newstyle "COA"
)
(if (setq styledata (tblsearch "style" newstyle))
    (progn
       (setq defwidth (cdr (assoc 41 styledata)))
       (while (setq ename (ssname ss1 index))
              (setq txtobj (vlax-ename->vla-object ename))
              (vlax-put-property txtobj 'Height newht)
              (vlax-put-property txtobj 'StyleName newstyle)
              (if (vlax-property-available-p txtobj 'ScaleFactor)
                  (vlax-put-property txtobj 'ScaleFactor defwidth)
              )
              (setq index (1+ index))
       )
    )
    (princ "\nRequested text style does not exist!")
)
(princ)
(command "chprop" "p" "" "LA" "G-ANNO-TEXT" "")
)


(defun c:detlsettbtext ( / ss1 index newht newstyle styledata defwidth ename txtobj)
(c:detltxt2mt)
(setq ss1 (ssget (list (cons 0 "*text")))
      index 0
      newht "0.1"
      newstyle "COA"
)
(if (setq styledata (tblsearch "style" newstyle))
    (progn
       (setq defwidth (cdr (assoc 41 styledata)))
       (while (setq ename (ssname ss1 index))
              (setq txtobj (vlax-ename->vla-object ename))
              (vlax-put-property txtobj 'Height newht)
              (vlax-put-property txtobj 'StyleName newstyle)
              (if (vlax-property-available-p txtobj 'ScaleFactor)
                  (vlax-put-property txtobj 'ScaleFactor defwidth)
              )
              (setq index (1+ index))
       )
    )
    (princ "\nRequested text style does not exist!")
)
(princ)
(command "chprop" "p" "" "LA" "G-ANNO-TABL-TEXT" "")
)



(defun c:textformat (/ ss str u)
  (vl-load-com)
  (initget 1 "U L REV UN RE")
  (if (and (setq u (getkword "\nUpper Lower UNderline REmove-Underline REVerse: "))
       (setq ss (ssget ":L" '((0 . "TEXT"))))
      )
    (foreach x (mapcar 'vlax-ename->vla-object
               (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
           )
      (setq str (vla-get-textstring x))
      (cond
    ((= u "U") (vla-put-textstring x (strcase str)))
    ((= u "L") (vla-put-textstring x (strcase str t)))
    ((and (= u "UN") (not (wcmatch (strcase str) "*%%U*")))
     (vla-put-textstring x (strcat "%%U" (vla-get-textstring x)))
    )
    ((= u "RE")
     (if (wcmatch (strcase str) "*%%U*")
       (vla-put-textstring x (vl-string-subst "" "%%U" (vla-get-textstring x)))
     )
    )
    ((= u "REV")
     (vla-put-textstring
       x
       (apply 'strcat (mapcar 'chr (reverse (vl-string->list str))))
     )
    )
      )
    )
  )
  (princ)
)

(defun c:qsel ()
    (sssetfirst nil (ssget "X" (list (cons 8 (getvar "clayer")))))
    (princ)
) ;_  end defun



;;=============================================================
;;     Sel.lsp by Charles Alan Butler
;;            Copyright 2004
;;   by Precision Drafting & Design All Rights Reserved.
;; 
;;    Version 1.0 Beta  July 23,2004
;;    Version 1.1 Beta  July 13,2005
;;
;;   Creates a selection set of objects on a layer(s)
;;   User picks objects to determine the layer(s)
;;   Then User selects objects for ss or presses enter to 
;;   get all objects on the selected layer(s)
;;   You may select the selection set before starting this
;;   routine. Then select the layers to keep in the set
;;=============================================================
(defun c:sel (/ ent lay ss lay:lst lay:prompt ss:first ent:lst)
  ;;  get anything already selected
  (setq ss:first (cadr(ssgetfirst))
        ss (ssadd))

  ;;  Get user selected layers
  (if ss:first
    (setq lay:prompt "\nSelect the object to choose layers to keep.")
    (setq lay:prompt "\nSelect object for layer filter.")
  )
  (while (setq ent (entsel lay:prompt))
    (setq ent:lst (cons (car ent) ent:lst))
    (setq lay:lst
           (cons (setq lay (cdr(assoc 8 (entget (car ent))))) lay:lst))
    (prompt (strcat "\n*-* Selected Layer -> " lay))
  )
  ;;  Un HighLite the entities
  (and ent:lst (mapcar '(lambda (x) (redraw x 4)) ent:lst))
 
  (if (> (length lay:lst) 0); got layers to work with
    (progn
      (setq lay "")
      (setq lay:lst (vl-sort lay:lst '<)) ; removes douplicates
      (foreach itm  lay:lst ; combine lay names into one , del string
        (setq lay (strcat lay itm ",")))
      (setq lay (substr lay 1 (1- (strlen lay)))); remove the last ,
      (if ss:first ; ALREADY GOT SELECTION SET
        (while (setq ent (ssname ss:first 0))
          (if (member (cdr(assoc 8 (entget ent))) lay:lst)
            (ssadd (ssname ss:first 0) ss)
          )
          (ssdel (ssname ss:first 0) ss:first)
        )
        (progn ; else get a selection set to work with
          (prompt (strcat "\nOK >>--> Select objects for Selection set or "
                          "ENTER for All objects on layer(s) " lay))
          ;;  get objects using filter with user select
          (if (null (setq ss (ssget (list (cons 8 lay)))))
            ;; or get ALL objects using filter
            (setq ss (ssget "_X" (list (cons 8 lay)(cons 410 (getvar "ctab")))))
          )
        )
      )
      (if (> (sslength ss) 0)
        (progn
          (prompt (strcat "\n" (itoa (sslength ss))
                      " Object(s) selected on layer(s) " lay
                      "\nStart an ACAD command."))
          (sssetfirst nil ss)
        )
        (prompt "\n***  Nothing Selected  ***")
      )
    )
  )
  (princ)
)
(prompt "\nSelect on Layer loaded, Enter Sel to run.")
(princ)



(defun c:detlsetttext ( / ss1 index newht newstyle styledata defwidth ename txtobj)
(c:detltxt2mt)
(setq ss1 (ssget (list (cons 0 "*text")))
      index 0
      newht "0.12"
      newstyle "COA"
)
(if (setq styledata (tblsearch "style" newstyle))
    (progn
       (setq defwidth (cdr (assoc 41 styledata)))
       (while (setq ename (ssname ss1 index))
              (setq txtobj (vlax-ename->vla-object ename))
              (vlax-put-property txtobj 'Height newht)
              (vlax-put-property txtobj 'StyleName newstyle)
              (if (vlax-property-available-p txtobj 'ScaleFactor)
                  (vlax-put-property txtobj 'ScaleFactor defwidth)
              )
              (setq index (1+ index))
       )
    )
    (princ "\nRequested text style does not exist!")
)
(command "chprop" "p" "" "LA" "G-ANNO-TITL" "")
(C:mtextu)
(princ)
)





(defun c:mtextu (/ ent obj)
  (vl-load-com)

  (if (and (setq ent (car (entsel "\nSelect MText: ")))
           (eq "MTEXT" (cdr (assoc 0 (entget ent)))))

    (vlax-put-property
      (setq obj (vlax-ename->vla-object ent)) 'TextString
        (strcat "{\\L"
          (vlax-get-property obj 'TextString) "}")))

  (princ))


(defun c:detlfine ()
    (command "select" (while (= (logand (getvar "cmdactive") 1) 1)
 (command pause)
 ))
    (command "chprop" "p" "" "LA" "G-ANNO-DETL-FINE" "") 
) ;_  end defun


(defun c:detlmedm ()
    (command "select" (while (= (logand (getvar "cmdactive") 1) 1)
 (command pause)
 ))
    (command "chprop" "p" "" "LA" "G-ANNO-DETL-MEDM" "") 
) ;_  end defun

(defun c:detlthck ()
    (command "select" (while (= (logand (getvar "cmdactive") 1) 1)
 (command pause)
 ))
    (command "chprop" "p" "" "LA" "G-ANNO-DETL-THCK" "") 
) ;_  end defun

(defun c:detltabl ()
    (command "select" (while (= (logand (getvar "cmdactive") 1) 1)
 (command pause)
 ))
    (command "chprop" "p" "" "LA" "G-ANNO-TABL" "") 
) ;_  end defun

(defun c:detltablt ()
    (command "select" (while (= (logand (getvar "cmdactive") 1) 1)
 (command pause)
 ))
    (command "chprop" "p" "" "LA" "G-ANNO-TABL-TEXT" "") 
) ;_  end defun

(defun c:detlpatt ()
    (command "select" (while (= (logand (getvar "cmdactive") 1) 1)
 (command pause)
 ))
    (command "chprop" "p" "" "LA" "G-ANNO-DETL-PATT" "") 
) ;_  end defun
