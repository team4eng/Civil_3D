  
;;;	Save ListView settings			;
  
  (defun bm:SaveListViewSettings (/ col0 col1 col2)
    (setq col0 (dcl_ListView_GetColWidth BlockManager_PaletteManager_BlocksListView 0)
          col1 (dcl_ListView_GetColWidth BlockManager_PaletteManager_BlocksListView 1)
          col2 (dcl_ListView_GetColWidth BlockManager_PaletteManager_BlocksListView 2)
          )
    (vl-registry-write
      "HKEY_CURRENT_USER\\Software\\BlockManager\\BlocksListView"
      "col0"
      col0
      )
    (vl-registry-write
      "HKEY_CURRENT_USER\\Software\\BlockManager\\BlocksListView"
      "col1"
      col1
      )
    (vl-registry-write
      "HKEY_CURRENT_USER\\Software\\BlockManager\\BlocksListView"
      "col2"
      col2
      )
    
    )
;;;	Return ListView settings		;

  (defun bm:GetListViewSettings	(/ col0 col1 col2)
  (setq col0(vl-registry-read
 "HKEY_CURRENT_USER\\Software\\BlockManager\\BlocksListView" 
 "col0")
	col1(vl-registry-read
 "HKEY_CURRENT_USER\\Software\\BlockManager\\BlocksListView" 
 "col1")
	col2(vl-registry-read
 "HKEY_CURRENT_USER\\Software\\BlockManager\\BlocksListView" 
 "col2")
	)
    (if (not col0)(setq col0 200))
    (if (not col1)(setq col1 100))
    (if (not col2)(setq col2 100))

    (list
      (list "Name" 1 col0)
      (list "Size" 1 col1)
      (list "Date Modified" 1 col2))
    )
;;;	ListView Add Columns			;
  
  (defun bm:ListViewAddColumns( / )
(dcl_ListView_AddColumns
    BlockManager_PaletteManager_BlocksListView
    (bm:GetListViewSettings))
    )
  
;;;	Populate ListView			;
;;;	(bm:PopulateListView "C:/Test")		;
;;;	(setq dir "C:/Test")			;
  
  (defun bm:PopulateListView(dir / dwgFiles filesizeStr systimeStr)
    (dcl_ListView_Clear BlockManager_PaletteManager_BlocksListView)
  (setq dwgFiles (vl-directory-files dir "*.dwg" 1))

      (if dwgFiles
	(progn
	  (foreach
		 x
		  dwgfiles
	    (setq filesizeStr (strcat (itoa (/ (vl-file-size (strcat dir "\\" x)) 1000)) " KB")
		  systimeStr  (bm:ReturnFileTime (strcat dir "\\" x))
		  )
	    (if	(and filesizeStr systimeStr)
	      (dcl_ListView_AddItem BlockManager_PaletteManager_BlocksListView (list x filesizeStr systimeStr))
	      )
	    )
	  )
	)
    )


;(setq folder nil Child nil) 
;(setq folder(bm:ReturnDir))
;(bm:PopulateTree (bm:ReturnDir))
;this populates the first 3 levels of the tree
(defun bm:PopulateTree (folder / Child)
   (dcl_tree_clear BlockManager_PaletteManager_NavTreeControl)
   (dcl_tree_addparent BlockManager_PaletteManager_NavTreeControl
                       "Block Library"            folder
                       0                          0
                       1
                      )
   

   (foreach i (vl-directory-files folder "*" -1)
      (if (and (not (= i ".")) (not (= i "..")))
         (dcl_tree_addchild BlockManager_PaletteManager_NavTreeControl
                            folder
                            i
                            (strcat folder i)
                            0
                            0
                            1
         )
      )
   )

   (setq Child
           (dcl_tree_getfirstchilditem BlockManager_PaletteManager_NavTreeControl folder)
   )
   (while Child
      (foreach i (vl-directory-files Child "*" -1)
         (if (and (not (= i ".")) (not (= i "..")))
            (dcl_tree_addchild BlockManager_PaletteManager_NavTreeControl
                               Child
                               i
                               (strcat Child "\\" i)
                               0
                               0
                               1
            )
         )
      )
      (setq
         Child (dcl_tree_getnextsiblingitem BlockManager_PaletteManager_NavTreeControl
                                            Child
               )
      )
   )
)
|;

(defun bm:PopulateExpandingItem(folder / )
   (foreach i (vl-directory-files folder "*" -1)
      (if (and (not (= i ".")) (not (= i "..")))
         (dcl_tree_addchild BlockManager_PaletteManager_NavTreeControl
                            folder
                            i
                            (strcat folder "\\" i)
                            0
                            0
                            1
         )
      )
   )
   (setq Child
           (dcl_tree_getfirstchilditem BlockManager_PaletteManager_NavTreeControl folder)
   )
   (while Child
      
      (foreach i (vl-directory-files Child "*" -1)
         (if (and (not (= i "."))
                  (not (= i ".."))
                  (not(dcl_Tree_GetItemLabel BlockManager_PaletteManager_NavTreeControl Item))
                  )
            (dcl_tree_addchild BlockManager_PaletteManager_NavTreeControl
                               Child
                               i
                               (strcat Child "\\" i)
                               0
                               0
                               1
            )
         )
      )
      (setq
         Child (dcl_tree_getnextsiblingitem BlockManager_PaletteManager_NavTreeControl
                                            Child
               )
      )
   )
   )