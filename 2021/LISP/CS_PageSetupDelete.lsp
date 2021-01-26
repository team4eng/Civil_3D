
; Deletes Page Setup(s)
; --- www.cadforum.cz ---
; (wildcards allowed)
;
(vl-load-com)

(defun DelPageSetup ( Name / Cfgs x)
(setq Cfgs
 (vla-get-plotconfigurations
 (vla-get-activedocument
 (vlax-get-acad-object))))

(cond
 ((vl-string-search "*" Name)
  (vlax-for x Cfgs
   (if (wcmatch (vla-get-name x) Name)
    (vla-delete x)
 )))

 (T
  (vl-catch-all-apply
    '(lambda () (vla-delete (vla-item Cfgs Name)))
 ))
)
(princ)
)

(defun C:DelPageSetup ( / psname)
 (setq psname (getstring "Page setup name(s) to delete (*=all): "))
 (DelPageSetup psname)
 (prin1)
)