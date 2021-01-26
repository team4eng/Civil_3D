(defun c:BLK_SelAnon nil
    (sssetfirst nil
        (ssget "_X"
            (list
               '(0 . "INSERT")
               '(2 . "`*U#*")
                (if (= 1 (getvar 'cvport))
                    (cons 410 (getvar 'ctab))
                   '(410 . "Model")
                )
            )
        )
    )
    (princ)
)