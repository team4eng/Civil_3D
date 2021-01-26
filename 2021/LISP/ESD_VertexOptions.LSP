
;;;
;;; AVX : Add VerteX on Pline (LW, 2D, 3D) 
;;; DVX : Del Vertex on Pline (LW, 2D, 3D) 
;;;
;;; Special version for US/English people 
;;; 

;;
;; AVX - from (gile) Gilles Chanteau - (12/05/07)
;; Ajoute un sommet au point spécifié à une extrémité ou sur le segment sélectionné d'une polyligne
;; 

(defun c:avx1	(/    err  AcDoc     pl	  ob   pk   pa	 ap   typ  org
		 ucs  ocs  pt	sp   ep	  co   no   p1	 p2   pt   ce
		 a1   a2   bu
		)

  (vl-load-com)

  (defun err (msg)
    (if	(or
	  (= msg "Fonction annulée")
	  (= msg "quitter / sortir abandon")
	)
      (princ)
      (princ (strcat "\nErreur: " msg))
    )
    (and ucs (vla-put-activeUCS AcDoc ucs))
    (and ocs (vla-delete ocs) (setq ocs nil))
    (vla-EndUndoMark
      (vla-get-ActiveDocument (vlax-get-acad-object))
    )
    (setq *error* m:err
	  m:err	nil
    )
  )

  (setq	m:err	*error*
	*error*	err
	AcDoc	(vla-get-activeDocument (vlax-get-acad-object))
  )
  (while (and
	   (setq pl (entsel))
	   (setq ob (vlax-ename->vla-object (car pl)))
	   (setq typ (vla-get-Objectname ob))
	 )
    (if	(or (= typ "AcDbPolyline")
	    (and (member typ '("AcDb2dPolyline" "AcDb3dPolyline"))
		 (= 0 (vla-get-Type ob))
	    )
	)
      (progn
	(vla-StartUndoMark AcDoc)
	(setq pk
	       (if (= typ "AcDb3dPolyline")
		 (trans (osnap (cadr pl) "_nea") 1 0)
		 (vlax-curve-getClosestPointToProjection
		   ob
		   (trans (cadr pl) 1 0)
		   (mapcar '-
			   (trans (getvar "VIEWDIR") 1 0)
			   (trans '(0 0 0) 1 0)
		   )
		 )
	       )
	)
	(setq ap (/ (* (getvar "APERTURE")
		       (getvar "VIEWSIZE")
		    )
		    (cadr (getvar "SCREENSIZE"))
		 )
	)
	(if (= typ "AcDbPolyline")
	  (setq co (split-list (vlax-get ob 'Coordinates) 2))
	  (setq co (split-list (vlax-get ob 'Coordinates) 3))
	)
	(cond
	  ((equal pk (vlax-curve-getStartPoint ob) ap)
	   (setq pa 0)
	   (if (= (vla-get-Closed ob) :vlax-false)
	     (setq sp (vlax-curve-getStartPoint ob)
		   ep nil
	     )
	     (setq ep nil
		   sp nil
	     )
	   )
	  )
	  ((equal pk (vlax-curve-getEndPoint ob) ap)
	   (setq pa (1- (length co)))
	   (if (= (vla-get-Closed ob) :vlax-false)
	     (setq ep (vlax-curve-getEndPoint ob)
		   sp nil
	     )
	     (setq ep nil
		   sp nil
	     )
	   )
	  )
	  (T
	   (setq pa (atoi (rtos (vlax-curve-getParamAtPoint ob pk)))
		 ep nil
		 sp nil
	   )
	  )
	)
	(if (and (/= typ "AcDb3dPolyline")
		 (or
		   (not	(equal (trans '(0 0 1) 1 0 T)
			       (setq no (vlax-get ob 'Normal))
			       1e-9
			)
		   )
		   (and	(= typ "AcDbPolyline")
			(/= 0 (vla-get-Elevation ob))
		   )
		   (and (= typ "AcDb2dPolyline") (/= 0 (caddar co)))
		 )
	    )
	  (progn
	    (setq ucs (vla-add
			(vla-get-UserCoordinateSystems AcDoc)
			(vlax-3d-point (setq org (getvar "UCSORG")))
			(vlax-3d-point (mapcar '+ org (getvar "UCSXDIR")))
			(vlax-3d-point (mapcar '+ org (getvar "UCSYDIR")))
			"avxUCS"
		      )
		  ocs (vla-add
			(vla-get-UserCoordinateSystems AcDoc)
			(vlax-3d-Point
			  (setq org (vlax-curve-getStartPoint ob))
			)
			(vlax-3d-Point
			  (mapcar '+ org (trans '(1 0 0) no 0))
			)
			(vlax-3d-Point
			  (mapcar '+ org (trans '(0 1 0) no 0))
			)
			"avxOCS"
		      )
	    )
	    (vla-put-activeUCS AcDoc ocs)
	  )
	)
	(if (setq
	      pt
	       (getpoint (trans (vlax-curve-getPointAtParam ob pa) 0 1) 

;;;			 "\nSpecifiez le sommet à ajouter: " 
			 "\nSpecify the Segment/Arc for Vertex to Add : " 

	       )
	    )
	  (progn
	    (and ep (setq pa (- (length co) 2)))
	    (if	(/= typ "AcDb3dPolyline")
	      (progn
		(setq p1 (trans (vlax-curve-getPointAtParam ob pa) 0 no)
		      pt (trans pt 1 no)
		      p2 (trans	(vlax-curve-getPointAtParam ob (1+ pa))
				0
				no
			 )
		)
		(cond
		  ((and ep (/= 0 (vla-getBulge ob pa)))
		   ((lambda (a)
		      (setq
			bu
			 (list (cons (1+ (fix pa)) (/ (sin a) (cos a))))
		      )
		    )
		     (/
		       (- (angle p2 pt)
			  (+ (angle p2 p1)
			     (* 2 (atan (vla-getBulge ob pa)))
			     pi
			  )
		       )
		       2.0
		     )
		   )
		  )
		  ((and sp (/= 0 (vla-getBulge ob pa)))
		   ((lambda (a)
		      (setq
			bu (list (cons 0 (/ (sin a) (cos a))))
		      )
		    )
		     (/
		       (- (+ (angle p1 p2)
			     (* -2 (atan (vla-getBulge ob pa)))
			     pi
			  )
			  (angle p1 pt)
		       )
		       2.0
		     )
		   )
		  )
		  (T
		   (setq
		     ce	((lambda (mid1 mid2)
			   (inters mid1
				   (polar mid1
					  (+ (angle p1 pt) (/ pi 2))
					  1.0
				   )
				   mid2
				   (polar mid2
					  (+ (angle pt p2) (/ pi 2))
					  1.0
				   )
				   nil
			   )
			 )
			  (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0))
				  p1
				  pt
			  )
			  (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.0))
				  pt
				  p2
			  )
			)
		   )
		   (if (or (= 0 (vla-getBulge ob pa)) (null ce))
		     (setq a1 0.0
			   a2 0.0
		     )
		     (if (< pi
			    (ang<2pi (- (angle pt p2) (angle p1 pt)))
			    (* 2 pi)
			 )
		       (setq a1	(- (ang<2pi (- (angle ce p1) (angle ce pt)))
				)
			     a2	(- (ang<2pi (- (angle ce pt) (angle ce p2)))
				)
		       )
		       (setq a1	(ang<2pi (- (angle ce pt) (angle ce p1)))
			     a2	(ang<2pi (- (angle ce p2) (angle ce pt)))
		       )
		     )
		   )
		   (setq bu
			  (list	(cons pa (/ (sin (/ a1 4.0)) (cos (/ a1 4.0))))
				(cons (1+ (fix pa))
				      (/ (sin (/ a2 4.0)) (cos (/ a2 4.0)))
				)
			  )
		   )
		  )
		)
	      )
	    )
	    (cond
	      ((= typ "AcDbPolyline")
	       (setq pt (list (car pt) (cadr pt)))
	      )
	      ((= typ "AcDb3dPolyline") (setq pt (trans pt 1 0)))
	    )
	    (or sp (setq pa (1+ pa)))
	    (cond
	      (sp (setq co (cons pt co)))
	      (ep (setq co (append co (list pt))))
	      (T
	       (setq co	(append	(sublst co 1 pa)
				(cons pt (sublst co (1+ pa) nil))
			)
	       )
	      )
	    )
	    (or
	      (= typ "AcDb3dPolyline")
	      (while (<= (setq pa (1+ pa)) (vlax-curve-getEndParam ob))
		(setq bu (cons (cons pa (vla-getBulge ob (1- pa))) bu))
	      )
	    )
	    (vlax-put ob 'Coordinates (apply 'append co))
	    (or	(= typ "AcDb3dPolyline")
		(mapcar	'(lambda (x) (vla-setBulge ob (car x) (cdr x)))
			bu
		)
	    )
	    (and ucs (vla-put-activeUCS AcDoc ucs))
	    (vla-EndUndoMark AcDoc)
	  )
	)
      )
      (progn 

;;	(alert "Entité non valide.") 
	(alert "Entity Not Valid.") 

	(exit)
      )
    )
  )
  (and ocs (vla-delete ocs) (setq ocs nil))
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)

;; 
;; DVX - from (gile) Gilles Chanteau - (12/05/07) 
;; Supprime le sommet sélectionné d'une polyligne (LW, 2D ou 3D) 
;; 

(defun c:dvx1	(/ err os pt ent typ plst par blst n)
  (vl-load-com)

  (defun err (msg)
    (if	(or
	  (= msg "Fonction annulée") 
	  (= msg "quitter / sortir abandon") 
	)
      (princ) 

      (princ (strcat "\nErreur: " msg))
    )
    (vla-EndUndoMark
      (vla-get-ActiveDocument (vlax-get-acad-object))
    )
    (setvar "OSMODE" os)
    (setq *error* m:err
	  m:err	nil
    )
  )

  (setq	m:err	*error*
	*error*	err
	os	(getvar "OSMODE")
  )
  (setvar "OSMODE" 1)
  (while (setq pt
		(getpoint 

;;		  "\nSélectionnez le sommet à supprimer: " 
		  "\nSelect the Vertex to Del/Erase: " 

		)
	 )
    (if	(and
	  (setq	ent (ssget pt
			   '((-4 . "<OR")
			     (0 . "LWPOLYLINE")
			     (-4 . "<AND")
			     (0 . "POLYLINE")
			     (-4 . "<NOT")
			     (-4 . "&")
			     (70 . 118)
			     (-4 . "NOT>")
			     (-4 . "AND>")
			     (-4 . "OR>")
			    )
		    )
	  )
	  (setq ent (vlax-ename->vla-object (ssname ent 0)))
	  (setq typ (vla-get-ObjectName ent))
	)
      (if
	(and
	  (setq	plst (if (= typ "AcDbPolyline")
		       (split-list (vlax-get ent 'Coordinates) 2)
		       (split-list (vlax-get ent 'Coordinates) 3)
		     )
	  )
	  (< 2 (length plst))
	)
	 (progn
	   (vla-StartUndoMark
	     (vla-get-ActiveDocument (vlax-get-acad-object))
	   )
	   (setq pt   (trans pt 1 0)
		 par  (cond
			((equal pt (vlax-curve-getStartPoint ent) 1e-9)
			 0
			)
			((equal pt (vlax-curve-getEndPoint ent) 1e-9)
			 (1- (length plst))
			)
			(T
			 (atoi (rtos (vlax-curve-getParamAtPoint ent pt))
			 )
			)
		      )
		 blst nil
		 n    0
	   )
	   (or (= typ "AcDb3dPolyline")
	       (repeat (length plst)
		 (if (/= n par)
		   (setq
		     blst
		      (cons (cons (length blst) (vla-getBulge ent n))
			    blst
		      )
		   )
		 )
		 (setq n (1+ n))
	       )
	   )
	   (vlax-put ent
		     'Coordinates
		     (apply 'append (vl-remove (nth par plst) plst))
	   )
	   (or (= typ "AcDb3dPolyline")
	       (mapcar '(lambda (x) (vla-setBulge ent (car x) (cdr x)))
		       blst
	       )
	   )
	   (vla-EndUndoMark
	     (vla-get-ActiveDocument (vlax-get-acad-object))
	   )
	 )
	(progn 

;; 	  (alert "\nLa polyligne n'a que deux sommets. ") 
	  (alert "\nThe Pline has ONLY 2 Vertex. ") 

	  (exit)
	)
      )
      (progn 

;;	(alert "Entité non valide.") 
	(alert "Entity not Valid.") 

	(exit)
      )
    )
  )
  (setvar "OSMODE" os)
  (setq	*error*	m:err
	m:err nil
  )
  (princ)
)

;;; SUBLST Retourne une sous-liste
;;; Premier élément : 1
;;; (sublst '(1 2 3 4 5 6) 3 2) -> (3 4)
;;; (sublst '(1 2 3 4 5 6) 3 nil) -> (3 4 5 6)

(defun sublst (lst start leng / rslt)
  (if (not (<= 1 leng (- (length lst) start)))
    (setq leng (- (length lst) (1- start)))
  )
  (repeat leng
    (setq rslt	(cons (nth (1- start) lst) rslt)
	  start	(1+ start)
    )
  )
  (reverse rslt)
)

;; SPLIT-LIST Retourne une liste de sous-listes
;; Arguments
;; - lst : la lste à fractionner
;; - num : un entier, le nombre d'éléments des sous listes
;; Exemples :
;; (split-list '(1 2 3 4 5 6 7 8) 2) -> ((1 2) (3 4) (5 6) (7 8))
;; (split-list '(1 2 3 4 5 6 7 8) 3) -> ((1 2 3) (4 5 6) (7 8))

(defun split-list (lst n)
  (if lst
    (cons (sublst lst 1 n)
	  (split-list (sublst lst (1+ n) nil) n)
    )
  )
)

;;; Ang<2pi Retourne l'angle, à 2*k*pi près, compris entre 0 et 2*pi

(defun ang<2pi (ang)
  (if (and (<= 0 ang) (< ang (* 2 pi)))
    ang
    (ang<2pi (rem (+ ang (* 2 pi)) (* 2 pi)))
  )
) 

