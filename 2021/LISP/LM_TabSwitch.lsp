(defun c:la+ ( / l n )
    (setq l (getorderedlayouts)
          n (length l)
    )
    (setvar 'ctab (nth (rem (1+ (vl-position (getvar 'ctab) l)) n) l))
    (princ)
)

(defun c:la- ( / l n )
    (setq l (getorderedlayouts)
          n (length l)
    )
    (setvar 'ctab (nth (rem (+ n -1 (vl-position (getvar 'ctab) l)) n) l))
    (princ)
)

(defun getorderedlayouts ( / l )
    (vlax-for x (aclayouts)
        (setq l (cons (list (vla-get-taborder x) (vla-get-name x)) l))
    )
    (mapcar 'cadr (vl-sort l '(lambda ( a b ) (< (car a) (car b)))))
)

(defun aclayouts nil
    (eval
        (list 'defun 'aclayouts 'nil
            (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
        )
    )
    (aclayouts)
)

(vl-load-com) (princ)