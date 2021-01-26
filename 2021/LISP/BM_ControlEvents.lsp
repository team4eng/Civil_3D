;;;						;
;;;	Control Events				;
;;;						;

;;;	ListView				;
;;;						;

;;;	OnClicked Event				;
(defun c:BlockManager_PaletteManager_BlocksListView_OnClicked (Row Column /)
   (setq dwgname (dcl_listview_getitemtext BlockManager_PaletteManager_BlocksListView Row)
         dir     (dcl_tree_getselecteditem BlockManager_PaletteManager_NavTreeControl)
   )
   (dcl_dwgpreview_loaddwg BlockManager_PaletteManager_PreviewBlockView
                           (findfile (strcat dir "\\" dwgname))
   )
   (princ)
   (princ)
)


;;;	OnDblClicked Event			;
(defun c:BlockManager_PaletteManager_BlocksListView_OnDblClicked (Row Column /)
   (c:BlockManager_PaletteManager_InsertButton_OnClicked)
   (princ)
   (princ)
)

;;;	TreeView				;
;;;						;

;;;	OnSelChanged Event			;
(defun c:BlockManager_PaletteManager_NavTreeControl_OnSelChanged (Label Key /)
   (dcl_control_setcaption BlockManager_PaletteManager_PathLabel Key)
   (bm:PopulateListView (strcat Key "\\"))
   (princ)
   (princ)
)

;;;	OnItemExpanded Event			;
(defun c:BlockManager_PaletteManager_NavTreeControl_OnItemExpanded (Label Key /)
   (if (not (= (dcl_tree_getrootitem BlockManager_PaletteManager_NavTreeControl) Key))
      (progn (bm:PopulateExpandingItem Key)
             (dcl_control_setcaption BlockManager_PaletteManager_PathLabel Key)
      )
   )
   (princ)
   (princ)
)

;;;	BlockView				;
;;;						;

;;;	OnDblClicked Event			;
(defun c:BlockManager_PaletteManager_PreviewBlockView_OnDblClicked (/)
   (c:BlockManager_PaletteManager_InsertButton_OnClicked)
   (princ)
   (princ)
)

;;;	Close Button				;
;;;						;

;;;	OnClicked Event				;

(defun c:BlockManager_PaletteManager_CloseButton_OnClicked (/)
   (bm:SaveListViewSettings)
   (dcl_form_close BlockManager_PaletteManager)
   (princ)
   (princ)
)

;;;	Insert Button				;
;;;						;

;;; 	Revised 09.11.2010			;
;;;	Users should customize:			;
;;;	(command "insert" . . .)		;
;;;	to suit individual needs		;
;;;	OnClicked Event				;
(defun c:BlockManager_PaletteManager_InsertButton_OnClicked (/ *Error* path blockname)
   (defun *Error* (Msg)
    (cond ((or (not Msg)
	   (member Msg
		   '("console break"
		     "Function cancelled"
		     "quit / exit abort"))))
      ((princ (strcat "\nError: " Msg))))
    (princ))
   (setq blockName (dcl_listview_getitemtext
                      BlockManager_PaletteManager_BlocksListView
                      (dcl_listview_getcursel BlockManager_PaletteManager_BlocksListView)
                   )
         path      (dcl_tree_getselecteditem BlockManager_PaletteManager_NavTreeControl)
   )
   (if (and path blockName)
      (command "_.insert" (findfile (strcat path "\\" blockName))); "_scale" 1 pause "0")
   )
   (princ)
   (princ)
)

;;;	Directory Button			;
;;;						;

;;;	OnClicked Event				;
(defun c:BlockManager_PaletteManager_DirectoryButton_OnClicked (/)
   (bm:SaveDir)
   (c:BlockManager_PaletteManager_OnInitialize)
   (princ)
   (princ)
)


;;;	PreviewPop Button			;
;;;						;

;;;	OnClicked Event				;
(defun c:BlockManager_PaletteManager_PreviewPopButton_OnClicked (/)
   (setq blockName (dcl_listview_getitemtext
                      BlockManager_PaletteManager_BlocksListView
                      (dcl_listview_getcursel BlockManager_PaletteManager_BlocksListView)
                   )
         path      (dcl_tree_getselecteditem BlockManager_PaletteManager_NavTreeControl)
   )
   (if (and path blockName)
      (BMP_ShowPreviewForm (strcat path "\\" blockname))
   )
   (princ)
   (princ)
)


;|«Visual LISP© Format Options»
(90 3 70 2 nil "end of " 90 70 1 0 2 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
