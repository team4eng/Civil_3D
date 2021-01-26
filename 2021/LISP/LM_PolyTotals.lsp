;;---------------------=={ Total Area }==---------------------;;
;;                                                            ;;
;;  Displays the total area of selected objects at the        ;;
;;  command line. The precision of the printed result is      ;;
;;  dependent on the setting of the LUPREC system variable.   ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:tarea ( / a i s )
    (if (setq s
            (ssget
               '(   (0 . "CIRCLE,ELLIPSE,*POLYLINE,SPLINE")
                    (-4 . "<NOT")
                        (-4 . "<AND")
                            (0 . "POLYLINE") (-4 . "&") (70 . 80)
                        (-4 . "AND>")
                    (-4 . "NOT>")
                )
            )
        )
        (progn
            (setq a 0.0)
            (repeat (setq i (sslength s))
                (setq a (+ a (vlax-curve-getarea (ssname s (setq i (1- i))))))
            )
            (alert (strcat "Total Area: " (rtos a 2) " SF"))
                    )
    )
    (princ)
)
(vl-load-com) (princ)


;;--------------------=={ Total Length }==--------------------;;
;;                                                            ;;
;;  Displays the total length of selected objects at the      ;;
;;  command line. The units and precision format of the       ;;
;;  printed result is dependent upon the settings of the      ;;
;;  LUNITS & LUPREC system variables respectively.            ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2013 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;

(defun c:tlen ( / e i l s )
    (if (setq s
            (ssget
               '(   (0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,SPLINE")
                    (-4 . "<NOT")
                        (-4 . "<AND")
                            (0 . "POLYLINE") (-4 . "&") (70 . 80)
                        (-4 . "AND>")
                    (-4 . "NOT>")
                )
            )
        )
        (progn
            (setq l 0.0)
            (repeat (setq i (sslength s))
                (setq e (ssname s (setq i (1- i)))
                      l (+ l (vlax-curve-getdistatparam e (vlax-curve-getendparam e)))
                )
            )
            (alert (strcat "Total Length: " (rtos l) " LF"))
                    )
    )
    (princ)
)
(vl-load-com) (princ)

