;;;						;
;;;	Utility Subs				;
;;;						;


;;;	Version Number				;
(defun c:BlockManagerVer( / )
   (prompt "\nBlock Manager ver 3.0 (read only)")
   (princ)
   )

;;;	Return File Systime			;
  
    (defun bm:ReturnFileTime (filename / time hour ampm minutes filetimeStr)
      (setq time (vl-file-systime filename))
      (if time
	(progn (if (> (nth 4 time) 12)
		 (setq hour (itoa (- (nth 4 time) 12))
		       ampm "PM"
		       )
		 (setq hour (itoa (nth 4 time))
		       ampm "AM"
		       )
		 )
	       (if (< (nth 5 time) 10)
		 (setq minutes (strcat "0" (itoa (nth 5 time))))
		 (setq minutes (itoa (nth 5 time)))
		 )
	       (setq filetimeStr
		      (strcat (itoa (nth 3 time))
			      "/"
			      (itoa (nth 1 time))
			      "/"
			      (itoa (nth 0 time))
			      " "
			      hour
			      ":"
			      minutes
			      " "
			      ampm
			      )
		     )
	       )
	)
      filetimeStr
      )


;;;						;
;;;	Return Root Directory Path		;
;;;	(bm:ReturnDir)				;
(defun bm:ReturnDir( / ret)
(setq ret(vl-registry-read
 "HKEY_CURRENT_USER\\Software\\BlockManager\\Dir" 
 "Path"))
  ret
  )

;;;						;
;;;	Return Root Directory Path		;
;;;	(bm:SaveDir)				;
(defun bm:SaveDir( / path)
(setq path(dcl_SelectFolder "Select Block Library Location"))
(if path
  (progn
     (if  (>(vl-string-position 92 path nil t)1)
     (setq path(strcat path "\\")))
(vl-registry-write
      "HKEY_CURRENT_USER\\Software\\BlockManager\\Dir"
      "Path"
      path
      ))
  )
  (princ path)
  (princ)
  )
