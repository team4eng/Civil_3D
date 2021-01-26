;;  Block Import Lisp  08/12/2008
;;  CAB at TheSwamp.org

  ;;  Get user selection of folder
  ;;  Get all DWG files in folder
  ;;  INSERT dwg as block @ 0,0
  ;;  get Bounding Box of block
  ;;  Move Insert to right w/ gap between blocks
  ;;  Next Insert

(defun c:BLK_INS (/ path LastDist gap space err newblk bname obj ll lr ur
               InsPt dist GetFolder)
  (vl-load-com)
  (defun GetFolder ( / DirPat msg)
   (setq msg "Open a folder and click on SAVE")
   (and
    (setq DirPat (getfiled "Browse for folder" msg " " 1))
    (setq DirPat (substr DirPat 1 (- (strlen DirPat) (strlen msg))))
   )
   DirPat
  )
  

  (defun activespace (doc)
    (if (or (= acmodelspace (vla-get-activespace doc))
            (= :vlax-true (vla-get-mspace doc)))
        (vla-get-modelspace doc)
        (vla-get-paperspace doc)
    )
  )

  (setq gap 2) ; this is the gap between blocks
  (setq LastDist 0.0) ; this is the cumulative distance
  
  (if (setq Path (GetFolder))
    (progn
      (setq space (activespace (vla-get-activeDocument (vlax-get-acad-object))))
      (prompt "\n***  Working, Please wait ......\n")
      (foreach bname (vl-directory-files Path "*.dwg" 1)
        ;;  OK, try & insert the Block
        (if (vl-catch-all-error-p
              (setq err (vl-catch-all-apply
                '(lambda () (setq newblk (vla-insertBlock space 
                                (vlax-3d-point '(0.0 0.0 0.0)) (strcat path bname) 1.0 1.0 1.0 0.0))
                   ))))
          ;;  Display the error message and block/file name
          (prompt (strcat "\n" bname " " (vl-catch-all-error-message err)))
          ;;  ELSE
          (progn ; INSERT was sucessful, move the block
            ;;  get bounding box
            (if (vl-catch-all-error-p
                  (setq err (vl-catch-all-apply 'vla-getboundingbox (list newblk 'll 'ur))))
               (prompt (strcat "\nBB Error - could not move " bname "\n  " (vl-catch-all-error-message err)))
               (progn
                 (setq ll (vlax-safearray->list ll)
                       ur (vlax-safearray->list ur)
                       lr (list (car ur) (cadr ll))
                       dist (distance ll lr)
                       )
                 ;;  MOVE the block
                 (setq ;InsPt  (vla-get-insertionpoint Newblk)
                       NewPt (polar '(0. 0. 0.) 0.0 (+ LastDist Gap (* dist 0.5)))
                       LastDist (+ LastDist Gap dist)
                       )
                 (vlax-put Newblk 'insertionpoint NewPt)
               )
            )
          )
         )
      )
    )
  )
  (princ)      
)
(princ)
(prompt "\nBlock Import Loadd, Enter BlkImport to run.")