(defun C:XRDA (/ *error*
 
mip:layer-status-restore mip:layer-status-save
 
delete-xref-img-underlay delete-all-dict
 
)
 
(vl-load-com)
 
(defun *error* (msg)
 
(mip:layer-status-restore)
 
(princ msg)
 
(princ)
 
) ;_ end of defun
 
(defun mip:layer-status-restore ()
 
(foreach item *PD_LAYER_LST*
 
(if (not (vlax-erased-p (car item)))
 
(vl-catch-all-apply
 
'(lambda ()
 
(vla-put-lock (car item) (cdr (assoc "lock" (cdr item))))
 
(vla-put-freeze
 
(car item)
 
(cdr (assoc "freeze" (cdr item)))
 
) ;_ end of vla-put-freeze
 
) ;_ end of lambda
 
) ;_ end of vl-catch-all-apply
 
) ;_ end of if
 
) ;_ end of foreach
 
(setq *PD_LAYER_LST* nil)
 
) ;_ end of defun
 
(defun mip:layer-status-save ()
 
(setq *PD_LAYER_LST* nil)
 
(vlax-for item (vla-get-layers
 
(vla-get-activedocument (vlax-get-acad-object))
 
) ;_ end of vla-get-layers
 
(setq *PD_LAYER_LST*
 
(cons (list item
 
(cons "freeze" (vla-get-freeze item))
 
(cons "lock" (vla-get-lock item))
 
) ;_ end of cons
 
*PD_LAYER_LST*
 
) ;_ end of cons
 
) ;_ end of setq
 
(vla-put-lock item :vlax-false)
 
(if (= (vla-get-freeze item) :vlax-true)
 
(vl-catch-all-apply
 
'(lambda () (vla-put-freeze item :vlax-false))
 
) ;_ end of vl-catch-all-apply
 
) ;_ end of if
 
) ;_ end of vlax-for
 
) ;_ end of defun
 
(defun delete-xref-img-underlay (/ count txt)
 
(mip:layer-status-save)
 
(vlax-for Blk (vla-get-Blocks
 
(vla-get-activedocument (vlax-get-acad-object))
 
) ;_ end of vla-get-Blocks
 
(if (and (= (vla-get-IsXref Blk) :vlax-false)
 
(not (wcmatch (vla-get-name Blk) "*|*"))
 
) ;_ end of and
 
(progn
 
(setq count 0
 
txt (strcat " Erase Xref and Underlay in "
 
(vla-get-name Blk)
 
) ;_ end of strcat
 
) ;_ end of setq
 
(grtext -1 txt)
 
(vlax-for Obj Blk
 
(setq count (1+ count))
 
(if (zerop (rem count 10))
 
(grtext -1 (strcat txt " : " (itoa count)))
 
) ;_ end of if
 
(if
 
(and (vlax-write-enabled-p Obj)
 
(or
 
(and ;_ XREF
 
(= (vla-get-ObjectName obj) "AcDbBlockReference")
 
(vlax-property-available-p Obj "Path")
 
) ;_ end of and
 
(and ;_ UNDERLAY
 
(wcmatch (vla-get-ObjectName obj) "*Reference")
 
(vlax-property-available-p Obj "UnderlayName")
 
) ;_ end of and
 
(= (vla-get-ObjectName obj) "AcDbRasterImage") ;_ IMAGE
 
) ;_ end of or
 
) ;_ end of and
 
(VL-CATCH-ALL-APPLY 'vla-Delete (list Obj))
 
) ;_ end of if
 
) ;_ end of vlax-for
 
) ;_ end of progn
 
) ;_ end of if
 
) ;_ end of vlax-for
 
(mip:layer-status-restore)
 
) ;_ end of defun
 
(defun delete-all-dict (dict)
 
;;; dict - dict name (like "ACAD_IMAGE_DICT", "ACAD_PDFDEFINITIONS" ... )
 
(vl-catch-all-apply
 
'(lambda ()
 
(vlax-map-Collection
 
(vla-item
 
(vla-get-dictionaries
 
(vla-get-activedocument (vlax-get-acad-object))
 
) ;_ end of vla-get-dictionaries
 
dict ;_ "ACAD_IMAGE_DICT"
 
) ;_ end of vla-Item
 
'vla-delete
 
) ;_ end of vlax-map-Collection
 
) ;_ end of lambda
 
) ;_ end of vl-catch-all-apply
 
) ;_ end of defun
 
(vl-load-com)
 
(delete-xref-img-underlay)
 
(command "_-xref" "_d" "*")
 
(while (> (getvar "CMDACTIVE") 0) (command))
 
(mapcar 'delete-all-dict
 
(list "ACAD_IMAGE_DICT"
 
"ACAD_PDFDEFINITIONS"
 
"ACAD_DWFDEFINITIONS"
 
"ACAD_DGNDEFINITIONS"
 
) ;_ end of list
 
) ;_ end of mapcar
 
(command "_.regenall")
 
(command "_.externalreferences")
 
(princ)
 
) ;_ end of defun
Open the External Reference Dialog/palette by entering XREF or 