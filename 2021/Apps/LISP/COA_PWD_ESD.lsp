(defun mystartup ()

 ; (command "-style" "Standard" "simplex.shx" "2" "1" "0" "N" "N" "N")
 (command "-style" "T4" "Arial" "0" "1" "0" "_N" "_N")
 (command "._ucs" "World") 	 
 (command "_.layer" "_Set" "0" "")
 (command "-scalelistedit" "R" "Y" "E")  
 (Command "cecolor" "ByLayer")
 (Command "celtype" "ByLayer")
 (Command "celweight" "-1")
; (command "cplotstyle" "ByLayer") 
 (Command "pickbox" "5")
 (Command "dynmode" "3")
 (Command "dynprompt" "1")
 
)

(defun C:explore ()
	(startapp "explorer /e,/select," (strcat (getvar "dwgprefix")(getvar "dwgname")))
	(princ)
)

(defun C:MED () (command "._mtexted" "."))
(defun C:NED () (command "._mtexted" "NOTEPAD"))
(defun C:R1 () (command "REGENAUTO" "ON"))
(defun C:R2 () (command "REGENAUTO" "OFF"))
(defun C:A0 () (command "SNAPANG" "0"))
(defun C:FIXCOPYPASTE () (dictremove (namedobjdict) "ACAD_DATALINK"))

(load "Blk_Lib.lsp")  (setq *Blk_Lib* t)
(if (findfile "C:\\Program Files (x86)\\Common Files\\OpenDCL")
(progn (autoload "BlockManager" '("BlockManager"))(load (findfile "BlockManager.fas")))
)

(autoload "C:/Autodesk/Civil_3D/2021/Apps/SSM/SSMPropEditor.lsp" '("SSMPropEditor"))

(autoload "CK_FileOpen.vlx" '("OPEN1"))

(autoload "CS_AddDelVertex.vlx" '("ADDV" "DELV"))
(autoload "CS_ATrim.vlx" '("ATRIM"))
(autoload "CS_PageSet.vlx" '("PAGESET"))
(autoload "CS_PageSetupDelete" '("DelPageSetup"))
(autoload "CS_PaintSelect.vlx" '("PaintSelect"))
(autoload "CS_WBlockAll.vlx" '("WBlockAll"))
(autoload "CS_xattachp.vlx" '("xattachp"))

(autoload "ESD_BlockFlip" '("BLK_FLIP"))
(autoload "ESD_BlockImport" '("BLK_INS"))
(autoload "ESD_BlockMatch" '("BLK_MATCH" "BLK_MATCHR"))
(autoload "ESD_BlockRename" '("RB"))
(autoload "ESD_BlockSwap.vlx" '("SwapBlock"))
(autoload "ESD_DimFlip" '("DIM_FLIP"))
(autoload "ESD_DisplayChange" '("CHD"))
(autoload "ESD_Layer_OrderBack" '("LYR_OrderBack"))
(autoload "ESD_MatchTextProps" '("TXT_MATCH" "TXT_MAS" "TXT_MAR"))
(autoload "ESD_ObjectBreak" '("BRK@"))
(autoload "ESD_ObjectCopyXref" '("XCOPY"))
(autoload "ESD_PipeFlip" '("Pipe_Flip"))
(autoload "ESD_PolylineEdit_Net" '("AVX" "DVX"))
(autoload "ESD_QuickSave" '("QAS" "QASC" "QWS" "QWSC" "QASA" "QWSA"))
(autoload "ESD_RedefineSurveyBlocks" '("ReDefVBLK"))
(autoload "ESD_RotateNorth" '("RN"))
(autoload "ESD_SaveContinue" '("QSC"))
(autoload "ESD_SelectAllByType" '("SX_BLK" "SX_BLKA" "SX_FELN" "SX_SVFG" "SX_3DPL" "SX_POLY" "SX_COGO" "SX_LINE" "SX_ARCS" "SX_ELPS" "SX_SPLN" "SX_TEXT" "SX_MTXT" "SX_MLDR" "SX_DIMM"))
(autoload "ESD_SelectSpecial" '("SSO" "SSL" "SST" "SSD" "SSB"))
(autoload "ESD_StripMText" '("SMT"))
(autoload "ESD_TextMLeader" '("T2ML"))
(autoload "ESD_TextTableCell" '("TTC"))
(autoload "ESD_VertexOptions" '("AVX1" "DVX1"))
(autoload "ESD_ViewportDraw" '("VG"))
(autoload "ESD_ViewportLock" '("VPLO" "VPLA" "VPULO" "VPULA" "VPT" "VPL" "VPLC" "VPULC"))
(autoload "ESD_ViewTwist" '("AVL" "AVP"))
(autoload "ESD_XREF_Detach" '("XRD"))
(autoload "ESD_XREF_DetachAll" '("XRDA"))
(autoload "ESD_XREF_DrawOrder" '("XREF_DrawOrder"))
(autoload "ESD_ZoomCommands" '("ZP" "ZE" "ZO"))

(autoload "LM_AlignTextToCurve" '("ATC"))
(autoload "LM_BlockAlign" '("BLK_Align" "BA"))
(autoload "LM_BlockAlignStatic" '("BLK_AlignS" "BAS"))
(autoload "LM_BlockSelAnon" '("BLK_SelAnon"))
(autoload "LM_BurstUpgraded" '("BLK_BURST"))
(autoload "LM_ChangeBlockBasePoint" '("CBP" "CBPR"))
(autoload "LM_CopyRenameBlock" '("CBK" "RBK"))
(autoload "LM_CopySwapText" '("CTX" "STX"))
(autoload "LM_Count" '("Count"))
(autoload "LM_DeleteBlocks" '("delblocks"))
(autoload "LM_DoubleOffset" '("Doff" "DoubleOffset"))
(autoload "LM_DTCurve" '("DTCURVE" "DTREMOVE"))
(autoload "LM_DynamicTextAlign" '("TXALIGN"))
(autoload "LM_Ellipse2Arc" '("E2A"))
(autoload "LM_MatchTextProps" '("MTP" "MatchTextProps"))
(autoload "LM_ObjectAlign" '("OA"))
(autoload "LM_ObjectBreak" '("BRK" "BRKO"))
(autoload "LM_ObjectOutline" '("OO"))
(autoload "LM_PolyTools" '("PC" "PJ" "PW"))
(autoload "LM_PolyTotals" '("TAREA" "TLEN"))
(autoload "LM_Steal" '("Steal" "StealAll" "StealTemplates" "StealLast"))
(autoload "LM_TabSort" '("TABSORT"))
(autoload "LM_Tabswitch" '("LA+" "LA-"))
(autoload "LM_Text2Mtext" '("T2M"))
(autoload "LM_TextFlip" '("TXT_FLIP" "TXT_SPIN"))
(autoload "LM_ViewportOutline" '("VPO"))

(autoload "SJ_WindowSize" '("WINDOWSIZE"))
