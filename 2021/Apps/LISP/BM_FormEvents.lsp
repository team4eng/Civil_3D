;;;						;
;;;	Form Events				;
;;;						;

;;;	OnInitialize				;
(defun c:BlockManager_PaletteManager_OnInitialize (/ path)
  ;(dcl_Control_SetVisible BlockManager_PaletteManager_ToggleButton nil)
  ;(dcl_Tree_SelectItem BlockManager_PaletteManager_NavTreeControl nil)
  (dcl_Tree_Clear BlockManager_PaletteManager_NavTreeControl)
  (dcl_ListView_Clear BlockManager_PaletteManager_BlocksListView)
  (dcl_DWGPreview_Clear BlockManager_PaletteManager_PreviewBlockView)
  (setq path(bm:ReturnDir))
  (if path
    (progn
      
  (bm:ListViewAddColumns)
  
  (bm:PopulateTree (bm:ReturnDir))
  (dcl_Tree_ExpandItem BlockManager_PaletteManager_NavTreeControl (dcl_Tree_GetRootItem BlockManager_PaletteManager_NavTreeControl) 1)
  (dcl_Control_SetCaption BlockManager_PaletteManager_PathLabel (strcat "  " path))
  )
    )
)


;;;	OnClose					;
(defun c:BlockManager_PaletteManager_OnClose (UpperLeftX UpperLeftY /)
  (princ "\nSee ya!")
  (princ)
)

;;;	OnEnteringNoDocState			;
(defun c:BlockManager_PaletteManager_OnEnteringNoDocState (/)
  (dcl_Form_Close BlockManager_PaletteManager)
)

;;;	Show Form				;
(defun c:BlockManager( / )
  (vl-load-com)
  ;(command "openDCL")
  (if dcl_form_show
    (progn(if (not (member "BlockManager" (dcl_GetProjects)))
    (dcl_Project_load "BlockManager")
    )
  (if (=(dcl_form_isActive BlockManager_PaletteManager)nil)
     (dcl_Form_Show BlockManager_PaletteManager)
      )
);progn
    (alert "The OpenDCL arx module did not load!")
    );if
  (princ)
  (princ)
  )
