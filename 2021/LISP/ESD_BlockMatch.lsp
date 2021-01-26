;;  Match Block
;;  Matches properties from doner block to selected blocks
(defun c:BLK_Match (/ srcblk ensel srcobj prop blkobj blk color linetype
               rotation x-scale y-scale z-scale layer linetypescale lineweight plotstylename
                    )

  (if (setq srcblk (car (entsel "\nSelect source block: ")))
    (setq srcobj   (vlax-ename->vla-object srcblk)
          x-scale  (vla-get-xscalefactor srcobj)
          y-scale  (vla-get-yscalefactor srcobj)
          z-scale  (vla-get-zscalefactor srcobj)
          rotation (vla-get-rotation srcobj)
          color    (vla-get-color srcobj)
          linetype (vla-get-linetype srcobj)
          layer    (vla-get-layer srcobj)
          linetypescale (vla-get-linetypescale srcobj)
          lineweight (vla-get-lineweight srcobj)
          plotstylename (vla-get-plotstylename srcobj)

    )
  )

  (while (progn
           (prompt "\nSelect Block to update: ")
           (setq blk (ssget "+.:E:S"))
         )
    (setq blkobj (vlax-ename->vla-object (ssname blk 0)))

    (prompt "\***  Got one.  ***")
    (vla-put-xscalefactor blkobj x-scale)
    (vla-put-yscalefactor blkobj y-scale)
    (vla-put-zscalefactor blkobj z-scale)
    (vla-put-rotation blkobj rotation)
    (vla-put-Color blkobj color)
    (vla-put-Linetype blkobj linetype)
    (vla-put-layer blkobj layer)
    (vla-put-linetypescale blkobj linetypescale)
    (vla-put-lineweight blkobj lineweight)
    (vla-put-plotstylename blkobj plotstylname)

  )
  (princ)
)
(prompt "\***  Match Block Loaded,  Enter MB to run.  ***")
(princ)


(defun c:BLK_MatchR (/ srcblk ensel srcobj prop blkobj blk color linetype
               rotation x-scale y-scale z-scale layer linetypescale lineweight plotstylename
                    )

  (if (setq srcblk (car (entsel "\nSelect source block: ")))
    (setq srcobj   (vlax-ename->vla-object srcblk)
          x-scale  (vla-get-xscalefactor srcobj)
          y-scale  (vla-get-yscalefactor srcobj)
          z-scale  (vla-get-zscalefactor srcobj)
          rotation (vla-get-rotation srcobj)
              )
  )

  (while (progn
           (prompt "\nSelect Block to update: ")
           (setq blk (ssget "+.:E:S"))
         )
    (setq blkobj (vlax-ename->vla-object (ssname blk 0)))

    (prompt "\***  Got one.  ***")
    (vla-put-xscalefactor blkobj x-scale)
    (vla-put-yscalefactor blkobj y-scale)
    (vla-put-zscalefactor blkobj z-scale)
    (vla-put-rotation blkobj rotation)
     )
  (princ)
)
(prompt "\***  Match Block Loaded,  Enter BLK_MatchR to run.  ***")
(princ)