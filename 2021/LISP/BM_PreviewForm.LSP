;;;						;
;;;	Preview Form				;
;;;						;

;;;						;
;;;	Form Event Functions			;
;;;						;

(defun BMP_ShowPreviewForm(Block / )
;;;	OnInitialize				;

(defun c:BlockManager_BlockPreview_OnInitialize (/)
  (dcl_BlockView_DisplayDwg BlockManager_BlockPreview_BlockView Block 0)
   (dcl_Control_SetCaption BlockManager_BlockPreview_BlockNameLabel Block)
)


;;;	OnDocActivated				;
(defun c:BlockManager_BlockPreview_OnDocActivated (/)
  (dcl_Form_Close BlockManager_BlockPreview)
)


;;;	OnEnteringNoDocState			;
(defun c:BlockManager_BlockPreview_OnEnteringNoDocState (/)
  (dcl_Form_Close BlockManager_BlockPreview)
)

(defun c:BlockManager_BlockPreview_CloseButton_OnClicked (/)
  (dcl_Form_Close BlockManager_BlockPreview)
)

   (if (=(dcl_form_isActive BlockManager_BlockPreview)nil)
     (dcl_Form_Show BlockManager_BlockPreview)
      )
   (princ)
   (princ)
   )