;-------------------------------------------------------------------------------
; Program Name: Blk_Lib.lsp [Block Library R17]
; Created By:   Terry Miller (Email: terrycadd@yahoo.com)
;               (URL: http://web2.airmail.net/terrycad)
; Date Created: 1-1-00
; Function:     Loads Block Library functions to insert blocks and adds blocks
;               to Block Libraries.
; Note:         Blk_Lib requires functions inside of GetIcon.lsp.
;-------------------------------------------------------------------------------
; Quick Block Library Installation Steps:
; 1. Copy Blk_Lib.lsp and Blk_Lib.dcl to a folder in your AutoCAD's search path.
; 2. Start AutoCAD and at the Command line type (load "Blk_Lib.lsp") to load
;    Block Library.
; 3. Then on the command line type (setq *Blk_Lib* t) so as to bypass the Block
;    Library Instructions.
; 4. You can run Block Library by typing SBL for "Select Block Library" or
;    type BLMENU for the "Block Library Menu".
; Note: You can automate steps (2) and (3) by adding them to your AcadDoc.lsp
; file to load Block Library.
;-------------------------------------------------------------------------------
; Revision History
; Rev  By     Date    Description
;-------------------------------------------------------------------------------
; 1    TM   1-1-00    Initial version.
; 2    TM   2-1-00    Added c:Sel_Lib, Select Block Library.
; 3    TM   3-1-00    Revised c:InBlocks and c:All_Blk_Lib functions.
; 4    TM   4-1-00    Created and added GetIcon.lsp Get functions.
; 5    TM   5-1-00    Revised code for AutoCAD 2000 compatibility.
; 6    TM   12-1-00   Revised GetIcon.lsp to allow up to 4 lines and to allow
;                     choosing different icons.
; 7    TM   1-1-01    Included Blk_Lib as the main command function and added
;                     more icons to GetIcon.lsp.
; 8    TM   10-1-03   Added c:LIB, shortcut for c:Library, for a user version
;                     of Select Block Library. Included insertion dot as the
;                     default for slides. Allow user to control block rotation.
; 9    TM   10-20-03  Added Slide_Script function to be used with Select Block
;                     Library to add folders of drawings to Block Libraries.
;                     Added c:Mat, shortcut for c:Match, the Match Slides Game.
; 10   TM   5-20-04   Added GetDwgsList function to check if drawing environment
;                     is a Single Document Interface before running scripts.
; 11   TM   12-20-04  Revised code for AutoCAD 2005 compatibility. Redesigned
;                     the dialogs with slide images, and increased the width
;                     for block names to allow more room for longer block names.
; 12   TM   3-20-05   Detached GetIcon.lsp functions into a separate file.
; 13   TM   9-20-05   Revised some of the dialog control functions and reworded
;                     some of the dialog messages.
; 14   TM   7-20-06   Revised code for AutoCAD 2007 compatibility.
; 15   TM   11-30-07  Added runapp function for DOS applications.
; 16   TM   2-1-08    Revised INBL, shortcut for c:InBlocks function, to insert
;                     blocks from a folder of drawings into a blank drawing to
;                     manage and add blocks to Block Libraries.
; 17   TM   2-20-08   Added ADL, shortcut for c:Add_Dwgs function, to add a
;                     folder of drawings to a library without inserting them
;                     into a drawing.
;-------------------------------------------------------------------------------
;   Note: Block Library saves a backup of your drawing as C:\Autodesk\Civil_3D\BLK\_Config\Blk_Lib.dwg
;   before it creates slides and wblocks.  If the program is interrupted or
;   canceled open C:\Autodesk\Civil_3D\BLK\_Config\Blk_Lib.dwg as a backup.
;-------------------------------------------------------------------------------
; Overview of Main Functions
;-------------------------------------------------------------------------------
; c:ALLBL and c:All_Blk_Lib [All Blocks to Library] - Select Library to add all
;   blocks in drawing.
;
; All_Blk_Lib [All Blocks to Library] - Adds all blocks in drawing to Library.
;   Example: (All_Blk_Lib "Notes" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Notes\\Notes.def" nil)
;   Returns: Creates or appends a Notes Library DefFile definition file in the
;   C:\Autodesk\Civil_3D\BLK\_Config\Notes folder for displaying slides and inserts blocks from the
;   same folder by default.
;
; c:BLIB - Call the function Blk_Lib with default arguments.
;
; c:Blk_Lib [Block Library] - Main command function that calls c:Sel_Lib
;  [Select Block Library]
;
; Blk_Lib [Block Library] - Inserts blocks from Library or calls the function
;   Blk_Mgr.
;   Example: (Blk_Lib "Electronic Symbols" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Elec_Sym\\Elec_Sym.def"
;   "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Elec_Blk\\Elec_Blk.def")
;   Returns: Creates or reads an Electronics Symbols Library DefFile definition
;   file in the C:\Autodesk\Civil_3D\BLK\_Config\Elec_Sym folder for displaying slides and inserts
;   blocks from the C:\Autodesk\Civil_3D\BLK\_Config\Elec_Blk folder.
;
; c:BLM and c:Blk_Mgr - Call the function Blk_Mgr with default arguments.
;
; Blk_Mgr [Block Library Manager] - Adds blocks to the Library or calls the
;   function Edit_Lib if no blocks found in drawing to add.
;   Example: (Blk_Mgr "Notes" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Notes\\Notes.def"
;   "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Notes\\Notes.def")
;   Returns: Adds a block to the C:\Autodesk\Civil_3D\BLK\_Config\Notes folder or calls the function
;   Edit_Lib if no blocks found in drawing to add.
;
; c:BLMENU and c:Blk_Menu [Block Library Menu] - Select a Block Library Command
;   from the Block Library Menu.
;
; c:DB and c:Dwg_Blks [Drawing Blocks] - View and select drawing blocks.
;
; c:EDBL and c:Edit_Lib - Call the function Edit_Lib with default arguments.
;
; Edit_Lib [Edit Block Library] - Rearranges or deletes lines of block insertion
;   data in the DefFile.
;   Example: (Edit_Lib "Notes" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Notes\\Notes.def")
;   Returns: Rearrange or delete lines of block insertion data for the Notes
;   Library in the C:\Autodesk\Civil_3D\BLK\_Config\Notes\Notes.def DefFile.
;
; c:INBL and c:InBlocks [Insert Blocks] - Insert blocks from selected folder.
;
; c:LIB and c:Library. [Select Block Library for Users]
;
; c:SBL and c:Sel_Lib [Select Block Library] - Selects a Block Library to use
;   or adds or edits library definitions.
;
; c:UL and c:User_Lib - Call the function User_Lib with default arguments.
;
; User_Lib [User Library] - Inserts blocks from Library with only user options.
;   Example: (User_Lib "Weld Symbols" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Weld_Sym\\Weld_Sym.def" nil)
;   Returns: Reads a Weld Symbols Library DefFile definition file in the
;   C:\Autodesk\Civil_3D\BLK\_Config\Weld_Sym folder for displaying slides and inserts blocks from
;   the same folder by default.
;
; c:ADL and c:Add_Dwgs [Add Drawings to Library] - Adds a folder of drawings
;   to a library without inserting them into a drawing.
;-------------------------------------------------------------------------------
; Shortcuts to Commands and Functions
;-------------------------------------------------------------------------------
; c:ALLBL - Shortcut for c:All_Blk_Lib. [All Blocks to Library]
;-------------------------------------------------------------------------------
;(defun c:ALLBL () (c:All_Blk_Lib))
;-------------------------------------------------------------------------------
; c:BLIB - Shortcut that calls function Blk_Lib with default arguments.
;-------------------------------------------------------------------------------
;(defun c:BLIB () (Blk_Lib nil nil nil))
;-------------------------------------------------------------------------------
; c:BLK_LIB - Shortcut for c:Sel_Lib. [Select Block Library]
;-------------------------------------------------------------------------------
(defun c:BLK_LIB () (c:Sel_Lib))
;-------------------------------------------------------------------------------
; c:BLM - Shortcut for c:Blk_Mgr. [Block Library Manager]
;-------------------------------------------------------------------------------
;(defun c:BLM () (Blk_Mgr nil nil nil))
;-------------------------------------------------------------------------------
; c:BLMENU - Shortcut for c:Blk_Menu. [Block Library Menu]
;-------------------------------------------------------------------------------
;(defun c:BLMENU () (c:Blk_Menu) (princ))
;-------------------------------------------------------------------------------
; c:DB - Shortcut for c:Dwg_Blks. [Drawing Blocks]
;-------------------------------------------------------------------------------
;(defun c:DB () (c:Dwg_Blks))
;-------------------------------------------------------------------------------
; c:EDBL - Shortcut for c:Edit_Lib. [Edit Block Library]
;-------------------------------------------------------------------------------
;(defun c:EDBL () (Edit_Lib nil nil))
;-------------------------------------------------------------------------------
; c:INBL - Shortcut for c:InBlocks. [Insert Blocks]
;-------------------------------------------------------------------------------
;(defun c:INBL () (c:InBlocks))
;-------------------------------------------------------------------------------
; c:LIB - Shortcut for c:Library. [Select Block Library for Users]
;-------------------------------------------------------------------------------
(defun c:LIB () (c:Library))
;-------------------------------------------------------------------------------
; c:SBL - Shortcut for c:Sel_Lib. [Select Block Library]
;-------------------------------------------------------------------------------
(defun c:SBL () (c:Sel_Lib))
;-------------------------------------------------------------------------------
; c:UL - Shortcut for c:User_Lib. [User Library]
;-------------------------------------------------------------------------------
;(defun c:UL () (User_Lib nil nil nil))
;-------------------------------------------------------------------------------
; c:ADL - Shortcut for c:Add_Dwgs. [Add Drawings to Library]
;-------------------------------------------------------------------------------
(defun c:ADL () (c:Add_Dwgs))
;-------------------------------------------------------------------------------
; Start of Block Library Commands and Functions
;-------------------------------------------------------------------------------
; c:All_Blk_Lib [All Blocks to Library] - Select Library to add all blocks in
; drawing.
;-------------------------------------------------------------------------------
(defun c:All_Blk_Lib (/ BlockLib@ BlkList@ Option# Cnt# DatFile$ Dcl_Id% EOF
    Field# FileName% intLib# Item$ LibrarySub: LibTitle$ Mid$ nthLibrary$
    Old_error PathDat$ Pick# Q$ Rep# SlideLib@ StartNo# TblList@ Text$ TitleLib@
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "ATTREQ" 0) (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  ;-----------------------------------------------------------------------------
  ; c:All_Blk_Lib subfunctions = LibrarySub:
  ;-----------------------------------------------------------------------------
  ; LibrarySub:
  ;-----------------------------------------------------------------------------
  (defun LibrarySub: ()
    (setq nthLibrary$ (get_tile "library")
          intLib# (atoi nthLibrary$)
          *Blk_Lib* (nth intLib# TitleLib@)
    );setq
    (set_tile "library" nthLibrary$)
  );defun LibrarySub:
  ;-----------------------------------------------------------------------------
  ; c:All_Blk_Lib - Start Main Function
  ;-----------------------------------------------------------------------------
  (setq LibTitle$ "All Blocks to")
  (setq PathDat$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Sel_Lib.dat")
  (setq DatFile$ (cadr (GetPathFile PathDat$)))
  (princ (strcat "\n" LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDat$ nil)
  (if (= (GetYesNo "All Blocks to Library"
    "Do you need to add a new\nLibrary folder for Blocks?" "Folder") "Yes")
    (progn
      (GetOK "All Blocks to Library"
        (strcat "Add a new Library Definition and folder,\n"
          "and then re-run All Blocks to Library."
        );strcat
        "filefolder"
      );GetOK
      (c:Sel_Lib)
      (princ "\nAll Blocks to Library ")
      (exit)
    );progn
  );if
  ;-----------------------------------------------------------------------------
  ; Get BlkList@
  ;-----------------------------------------------------------------------------
  (setq TblList@ (tblnext "block" t))
  (while TblList@
    (if (/= (substr (cdr (assoc 2 TblList@)) 1 1) "*")
      (setq BlkList@ (cons (cdr (assoc 2 TblList@)) BlkList@))
    );if
    (setq TblList@ (tblnext "block"))
  );while
  (if BlkList@
    (progn
      (if (equal BlkList@ *BlkList@)
        (setq BlkList@ *NewBlkList@)
        (progn
          (setq *BlkList@ BlkList@);Store global *BlkList@ variable
          (setq BlkList@ (acad_strlsort BlkList@))
          (setq BlkList@ (BlkSlides BlkList@))
          (setq *NewBlkList@ BlkList@);Store global *NewBlkList@ variable
        );progn
      );if
      (if (not BlkList@)
        (progn
          (princ "\nNo valid blocks found in drawing.")(princ)
          (GetOK "Block Library"
            (strcat "No valid blocks found in drawing for "
            LibTitle$ " Library.\n"
            "Create or insert blocks or open a drawing with blocks.")
            "AlertX"
          );GetOK
          (exit)
        );progn
      );if
    );progn
    (progn
      (princ "\nNo blocks found in drawing.")(princ)
      (GetOK "Block Library"
        (strcat "No valid blocks found in drawing for " LibTitle$ " Library.\n"
        "Create or insert blocks or open a drawing with blocks.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  ;-----------------------------------------------------------------------------
  ; Get Lists from PathDat$ file.
  ;-----------------------------------------------------------------------------
  (if (findfile PathDat$)
    (setq FileName% (open PathDat$ "r"))
    (progn
      (GetOK (strcat LibTitle$ " Library")
        (strcat DatFile$ " file not found\n"
        "for " LibTitle$ " Library.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (setq Q$ "\"") (setq Rep# 0)
  (setq EOF nil) (setq Item$ "")
  (while (null EOF)
    (setq Text$ (read-line FileName%))
    (if Text$
      (if (= (substr Text$ 1 1) Q$)
        (progn
          (setq StartNo# 2)
          (setq Cnt# 1)
          (setq Field# 1)
          (while (and (<= Cnt# (strlen Text$)) (< Field# 3))
            (setq Mid$ (substr Text$ Cnt# 3))
            (if (= Mid$ "\",\"")
              (progn
                (setq Item$
                  (substr Text$ StartNo# (- Cnt# StartNo#))
                );setq
                (cond
                  ((= Field# 1)
                    (if TitleLib@
                      (setq TitleLib@ (append TitleLib@ (list Item$)))
                      (setq TitleLib@ (list Item$))
                    );if
                  );Field# 1
                  ((= Field# 2)
                    (if SlideLib@
                      (setq SlideLib@ (append SlideLib@ (list Item$)))
                      (setq SlideLib@ (list Item$))
                    );if
                  );Field# 2
                  (t (exit))
                );cond
                (setq Field# (1+ Field#))
                (setq StartNo# (+ Cnt# 3))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (setq Item$
            (substr Text$ StartNo# (- (strlen Text$) StartNo#))
          );setq
          (if BlockLib@
            (setq BlockLib@ (append BlockLib@ (list Item$)))
            (setq BlockLib@ (list Item$))
          );if
        );progn
      );if
      (setq EOF t)
    );if
  );while
  (close FileName%)
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Library in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (if (and (= (type *Blk_Lib*) 'STR) (member *Blk_Lib* TitleLib@))
    (setq intLib# (- (length TitleLib@) (length (member *Blk_Lib* TitleLib@)))
          nthLibrary$ (itoa intLib#)
    );setq
    (setq *Blk_Lib* "Block"
          nthLibrary$ "0"
          intLib# 0
    );setq
  );if
  (princ "\nSelect an option: ")(princ)
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (while (/= Option# 1)
    (new_dialog "Library" Dcl_Id%)
    (set_tile "title" " All Blocks to Library")
    (start_list "library")
    (mapcar 'add_list TitleLib@)
    (end_list)
    (set_tile "library" nthLibrary$)
    (action_tile "library" "(LibrarySub:)")
    (action_tile "select"  "(done_dialog 3)");select
    (action_tile "cancel"  "(done_dialog 2)");cancel
    (setq Option# (start_dialog))
    (if (= Option# 3);select
      (setq Pick# 3
            Option# 1
      );setq
    );if
    (if (= Option# 2);cancel
      (setq Option# 1)
    );if
  );while
  (unload_dialog Dcl_Id%)
  (if (= Pick# 3);select
    (All_Blk_Lib
      (nth intLib# TitleLib@) (nth intLib# SlideLib@) (nth intLib# BlockLib@)
    );All_Blk_Lib
  );if
  (setq *error* Old_error)
  (princ)
);defun c:All_Blk_Lib
;-------------------------------------------------------------------------------
; All_Blk_Lib {All Blocks to Library] - Add all blocks in drawing to Library.
; Arguments: 3
;   LibTitle$ = Library title
;   PathDef$ = Pathname and DefFile name for slides
;   PathBlk$ = Pathname and DefFile name for blocks
; Returns: Adds all blocks in drawing to Library.
;-------------------------------------------------------------------------------
(defun All_Blk_Lib (LibTitle$ PathDef$ PathBlk$ / Attribs$ BlkList@ BlkPath$
    BlockData$ Color0# DefFile$ DefPath$ FileName% PathBlock$ Pt1@ Pt2@ Q$
    RealBlk& Rep# Replace$ RevSS& SSDims& TblList@ UcsIcon#
  );variables
  ;-----------------------------------------------------------------------------
  ; Evaluate Arguments and get DefPath$ and DefFile$ variables.
  ;-----------------------------------------------------------------------------
  (if (null LibTitle$)
    (setq LibTitle$ "Block")
  );if
  (if (null PathDef$)
    (setq PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
  );if
  (setq DefPath$ (car (GetPathFile PathDef$)))
  (setq DefFile$ (cadr (GetPathFile PathDef$)))
  (if (null PathBlk$)
    (setq PathBlk$ PathDef$)
  );if
  (setq BlkPath$ (car (GetPathFile PathBlk$)))
  (princ (strcat "\nAll Blocks to " LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDef$ PathBlk$)
  ;-----------------------------------------------------------------------------
  ; Get BlkList@
  ;-----------------------------------------------------------------------------
  (setq TblList@ (tblnext "block" t))
  (while TblList@
    (if (/= (substr (cdr (assoc 2 TblList@)) 1 1) "*")
      (setq BlkList@ (cons (cdr (assoc 2 TblList@)) BlkList@))
    );if
    (setq TblList@ (tblnext "block"))
  );while
  (if BlkList@
    (progn
      (if (equal BlkList@ *BlkList@)
        (setq BlkList@ *NewBlkList@)
        (progn
          (setq *BlkList@ BlkList@);Store global *BlkList@ variable
          (setq BlkList@ (acad_strlsort BlkList@))
          (setq BlkList@ (BlkSlides BlkList@))
          (setq *NewBlkList@ BlkList@);Store global *NewBlkList@ variable
        );progn
      );if
      (if (not BlkList@)
        (progn
          (princ "\nNo valid blocks found in drawing.")(princ)
          (GetOK "All Blocks to Library"
            (strcat "No valid blocks found in drawing for "
            LibTitle$ " Library.\n"
            "Create or insert blocks or open a drawing with blocks.") "AlertX"
          );GetOK
          (exit)
        );progn
      );if
    );progn
    (progn
      (princ "\nNo blocks found in drawing.")(princ)
      (GetOK "All Blocks to Library"
        (strcat "No blocks found in drawing for " LibTitle$ " Library.\n"
        "Create or insert blocks or open a drawing with blocks.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (SaveDwgName);Save Drawing*.dwg as a different name
  (princ "\nAdding slides and blocks ")(princ)
  (setq Pt1@ (polar (getvar "VIEWCTR") (* pi 0.5)(/ (getvar "VIEWSIZE") 2.0)))
  (setq Pt2@ (polar Pt1@ (* pi 1.5) (getvar "VIEWSIZE")))
  (command "UNDO" "BEGIN")
  (if (ssget "X")
    (command ".ERASE" (ssget "X") "")
  );if
  (command ".LAYER" "T" "*" "U" "*" "ON" "*" "")
  (command ".LAYER" "T" "0" "U" "0" "ON" "0" "S" "0" "")
  (setq Color0# (abs (cdr (assoc 62 (tblnext "layer" t)))))
  (command ".LAYER" "M" "0" "C" "7" "" "")
  (setq UcsIcon# (getvar "UCSICON"))
  (setvar "UCSICON" 0)
  (setq FileName% (open PathDef$ "a"))
  (foreach Block$ BlkList@
    (command ".INSERT" Block$ "0,0" "" "" "")
    (command ".ZOOM" "E")
    (setq RealBlk& (ssget "L"))
    (if RealBlk&
      (progn
        (command ".EXPLODE" "L")
        (setq SSDims& (ssget "X" (list (cons 0 "DIMENSION"))))
        (if SSDims&
          (progn
            (command ".ERASE" (ssget "X") "")
            (command ".INSERT" Block$ "0,0" "" "" "")
          );progn
        );if
        (command ".DONUT" "0" "0.00000001" "0,0" "")
        (setq Ent1^ (entlast))
        (command ".ZOOM" "E")
        (command ".DONUT" "0" (/ (getvar "VIEWSIZE") 25.0) "0,0" "")
        (command ".CHPROP" "L" "" "C" "6" "");Color choice optional
        (setq Ent2^ (entlast))
        (command ".ZOOM" "0.95x")
        (command ".REGEN")
        (setq PathBlock$ (strcat BlkPath$ Block$ ".dwg"))
        (setq Replace$ "Yes")
        (if (findfile PathBlock$)
          (setq Replace$ (GetYesNoCancel "All Blocks to Library"
            (strcat PathBlock$ "\nexists.  Do you want to replace it?") "Block")
          );setq
        );if
        (if (= Replace$ "Yes")
          (progn
            (command ".MSLIDE" (strcat DefPath$ Block$))
            (entdel Ent1^)(entdel Ent2^)
            (if SSDims&
              (progn
                (if (findfile PathBlock$)
                  (command ".WBLOCK" PathBlock$ "Y" Block$)
                  (command ".WBLOCK" PathBlock$ Block$)
                );if
                (command ".ERASE" (ssget "X") "")
              );progn
              (progn
                (setq RevSS& (ReverseSS (ssget "X")))
                (if (findfile PathBlock$)
                  (command ".WBLOCK" PathBlock$ "Y" "" "0,0" RevSS& "")
                  (command ".WBLOCK" PathBlock$ "" "0,0" RevSS& "")
                );if
              );progn
            );if
            (setq Q$ "\"")
            (if (= (cdr (assoc 70 (tblsearch "BLOCK" Block$))) 2)
              (setq Attribs$ "1")
              (setq Attribs$ "0")
            );if
            (setq BlockData$ (strcat
              Q$ Block$ Q$ ","          ;Block
              Q$ "Current layer" Q$ "," ;Layer
              Q$ "User selects" Q$ ","  ;Point
              Q$ "User selects" Q$ ","  ;Scale
              Q$ "0" Q$ ","             ;Explode
              Q$ Attribs$ Q$)           ;Attribs
            );setq
            (write-line BlockData$ FileName%)
          );progn
          (command ".ERASE" (ssget "X") "")
        );if
      );progn
      (command ".ERASE" (ssget "X") "")
    );if
    (Whirl)
    (if (= Replace$ "Cancel")
      (progn
        (command ".ZOOM" "W" Pt1@ Pt2@)
        (command ".LAYER" "M" "0" "C" Color0# "" "")
        (close FileName%)
        (command "UNDO" "END")
        (command "U")
        (princ " ")
        (exit)
      );progn
    );if
  );foreach
  (command ".ZOOM" "W" Pt1@ Pt2@)
  (command ".LAYER" "M" "0" "C" Color0# "" "")
  (close FileName%)
  (command "UNDO" "END")
  (command "U")
  (setvar "UCSICON" UcsIcon#)
  (princ " ")
  (princ)
);defun All_Blk_Lib
;-------------------------------------------------------------------------------
; *BL_error* [Block Library error handler]
;-------------------------------------------------------------------------------
(defun *BL_error* (msg)
  (if (not (member msg
      (list "console break" "Function cancelled" "quit / exit abort")))
    (alert msg)
  );if
  (setq *error* Old_error Old_error nil)
  (princ)
);defun *BL_error*
;-------------------------------------------------------------------------------
; Blk_Lib [Block Library] - Inserts blocks from Library.
; Arguments: 3
;   LibTitle$ = Library title
;   PathDef$ = Pathname and DefFile name for slides
;   PathBlk$ = Pathname and DefFile name for blocks
; Returns: Inserts a block or calls function Blk_Mgr.
;-------------------------------------------------------------------------------
(defun Blk_Lib (LibTitle$ PathDef$ PathBlk$ / Attribs$ AttribsList@ BlkList@
    BlkPath$ Block$ BlockLen# Option# Cnt# DefFile$ DefPath$ Dcl_Id% EOF
    Explode$ ExplodeList@ Field# FileName% ImageName$ Item$ Layer$ LayerList@
    Manager$ Mid$ No_Blks# No_Pages# Old_error Osmode PathBlock$ Pg_No# Pick
    Point@ PointList@ Q$ Ref# Rep# ScaleList@ Scale~ SlideRef$ StartNo# Text$
    UserPoint@ UserScale~
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "ATTREQ" 0) (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  ;-----------------------------------------------------------------------------
  ; Evaluate Arguments and get DefPath$ and DefFile$ variables.
  ;-----------------------------------------------------------------------------
  (if (null LibTitle$)
    (setq LibTitle$ "Block")
  );if
  (if (null PathDef$)
    (setq PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
  );if
  (setq DefPath$ (car (GetPathFile PathDef$)))
  (setq DefFile$ (cadr (GetPathFile PathDef$)))
  (if (null PathBlk$)
    (setq PathBlk$ PathDef$)
  );if
  (setq BlkPath$ (car (GetPathFile PathBlk$)))
  (princ (strcat "\n" LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDef$ PathBlk$)
  ;-----------------------------------------------------------------------------
  ; Get Lists from PathDef$ file.
  ;-----------------------------------------------------------------------------
  (if (findfile PathDef$)
    (setq FileName% (open PathDef$ "r"))
    (progn
      (GetOK (strcat LibTitle$ " Library")
        (strcat DefFile$ " file not found\n"
        "for " LibTitle$ " Library.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (setq Q$ "\"") (setq Rep# 0)
  (setq EOF nil) (setq Item$ "")
  (while (null EOF)
    (setq Rep# (1+ Rep#))
    (if (= Rep# 20)
      (progn
        (setq Rep# 0)
        (Whirl)
      );progn
    );if
    (setq Text$ (read-line FileName%))
    (if Text$
      (if (= (substr Text$ 1 1) Q$)
        (progn
          (setq StartNo# 2)
          (setq Cnt# 1)
          (setq Field# 1)
          (while (and (<= Cnt# (strlen Text$)) (< Field# 6))
            (setq Mid$ (substr Text$ Cnt# 3))
            (if (= Mid$ "\",\"")
              (progn
                (setq Item$
                  (substr Text$ StartNo# (- Cnt# StartNo#))
                );setq
                (cond
                  ((= Field# 1)
                    (if BlkList@
                      (setq BlkList@ (append BlkList@ (list Item$)))
                      (setq BlkList@ (list Item$))
                    );if
                  );Field# 1
                  ((= Field# 2)
                    (if LayerList@
                      (setq LayerList@ (append LayerList@ (list Item$)))
                      (setq LayerList@ (list Item$))
                    );if
                  );Field# 2
                  ((= Field# 3)
                    (if PointList@
                      (setq PointList@ (append PointList@ (list Item$)))
                      (setq PointList@ (list Item$))
                    );if
                  );Field# 3
                  ((= Field# 4)
                    (if ScaleList@
                      (setq ScaleList@ (append ScaleList@ (list Item$)))
                      (setq ScaleList@ (list Item$))
                    );if
                  );Field# 4
                  ((= Field# 5)
                    (if ExplodeList@
                      (setq ExplodeList@ (append ExplodeList@ (list Item$)))
                      (setq ExplodeList@ (list Item$))
                    );if
                  );Field# 5
                  (t (exit))
                );cond
                (setq Field# (1+ Field#))
                (setq StartNo# (+ Cnt# 3))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (setq Item$
            (substr Text$ StartNo# (- (strlen Text$) StartNo#))
          );setq
          (if AttribsList@
            (setq AttribsList@ (append AttribsList@ (list Item$)))
            (setq AttribsList@ (list Item$))
          );if
        );progn
      );if
      (setq EOF t)
    );if
  );while
  (princ " ")
  (close FileName%)
  (setq No_Pages# (fix (/ (1- (length BlkList@)) 20.0))
        Pg_No# 0
        BlockLen# (length BlkList@)
        Option# 99
  );setq
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Blk_Lib in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (princ "\nSelect a block or option: ")(princ)
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (while (/= Option# 1)
    (new_dialog "Blk_Lib" Dcl_Id%)
    (set_tile "title" (strcat " " LibTitle$ " Library "
      (itoa (+ Pg_No# 1)) " of " (itoa (+ No_Pages# 1)))
    );set_tile
    (if (= Pg_No# No_Pages#)
      (mode_tile "next" 1)
      (mode_tile "next" 0)
    );if
    (if (= Pg_No# 0)
      (mode_tile "previous" 1)
      (mode_tile "previous" 0)
    );if
    (if (> BlockLen# 19)
      (setq No_Blks# 20)
      (setq No_Blks# BlockLen#)
    );if
    (action_tile "manager" "(done_dialog 5)")
    (action_tile "next" "(done_dialog 4)")
    (action_tile "previous" "(done_dialog 3)")
    (action_tile "cancel" "(done_dialog 2)")
    (setq Rep# 1)
    (repeat (fix No_Blks#)
      (action_tile (strcat "sld" (itoa Rep#)) "(setq SlideRef$ $key Pick t)")
      (setq ImageName$ (nth (+ (* Pg_No# 20) (- Rep# 1)) BlkList@))
      (start_image (strcat "sld" (itoa Rep#)))
      (slide_image 0 0
        (dimx_tile (strcat "sld" (itoa Rep#)))
        (dimy_tile (strcat "sld" (itoa Rep#)))
        (strcat DefPath$ ImageName$ ".sld")
      );slide_image
      (end_image)
      (set_tile
        (strcat "sld" (itoa Rep#) "text") ImageName$
      );set_tile
      (setq Rep# (1+ Rep#))
    );repeat
    (setq Option# (start_dialog))
    (if (= Option# 5);manager
      (setq Option# 1
            SlideRef$ nil
            Manager$ "Yes"
      );setq
    );if
    (if (= Option# 4);next
      (setq Pg_No# (1+ Pg_No#)
            BlockLen# (- BlockLen# 20)
            SlideRef$ nil
      );setq
    );if
    (if (= Option# 3);previous
      (setq Pg_No# (- Pg_No# 1)
            BlockLen# (+ BlockLen# 20)
            SlideRef$ nil
      );setq
    );if
    (if (= Option# 2);cancel
      (setq Option# 1
            SlideRef$ nil
      );setq
    );if
  );while
  (unload_dialog Dcl_Id%)
  (if (and SlideRef$ Pick)
    (progn
      (setq Ref# (- (+ (* Pg_No# 20) (atoi (substr SlideRef$ 4))) 1))
      (setq Block$   (nth Ref# BlkList@)
            Layer$   (nth Ref# LayerList@)
            Point@   (nth Ref# PointList@)
            Scale~   (nth Ref# ScaleList@)
            Explode$ (nth Ref# ExplodeList@)
            Attribs$ (nth Ref# AttribsList@)
      );setq
      (setq PathBlock$ (strcat BlkPath$ Block$ ".dwg"))
      (if (or (= Block$ " ") (not (findfile PathBlock$)))
        (progn
          (if (/= Block$ " ")
            (GetOK (strcat LibTitle$ " Library")
              (strcat PathBlock$ "\nblock not found.") "AlertX"
            );GetOK
          );if
          (exit)
        );progn
      );if
      (if (= Point@ "Lower left limits")
        (setq Point@ (getvar "LIMMIN"))
      );if
      (if (= Point@ "Lower right limits")
        (setq Point@ (list (car (getvar "LIMMAX")) (cadr (getvar "LIMMIN"))))
      );if
      (if (= Point@ "Upper left limits")
        (setq Point@ (list (car (getvar "LIMMIN")) (cadr (getvar "LIMMAX"))))
      );if
      (if (= Point@ "Upper right limits")
        (setq Point@ (getvar "LIMMAX"))
      );if
      (if (and (= Point@ "User selects") (/= Scale~ "User selects"))
        (progn
          (while (null UserPoint@)
            (setq UserPoint@ (getpoint
              (strcat "\nInsertion point for " Block$ " block: "))
            );setq
          );while
          (setq Point@ UserPoint@)
        );progn
      );if
      (if (/= Point@ "User selects")
        (progn
          (if (= (type Point@) 'STR)
            (setq Point@ (RealList Point@))
          );if
          (if (/= (type Point@) 'LIST)
            (progn
              (GetOK (strcat LibTitle$ " Library")
                (strcat "Invalid insertion point for " Block$
                "\nblock in " DefFile$ " file.") "AlertX"
              );GetOK
              (exit)
            );progn
          );if
        );progn
      );if
      (if (= Scale~ "Dim scale")
        (if (= (getvar "DIMSCALE") 0)
          (progn
            (GetOK (strcat LibTitle$ " Library")
              "DIMSCALE value must be greater than 0." "AlertX"
            );GetOK
            (exit)
          );progn
          (setq Scale~ (getvar "DIMSCALE"))
        );if
      );if
      (if (and (= Scale~ "User selects") (/= Point@ "User selects"))
        (progn
          (while (or (null UserScale~) (< UserScale~ 0))
            (setq UserScale~
              (getreal
                (strcat "\nScale factor for " Block$ " block <1>: ")
              );getreal
            );setq
            (if (null UserScale~)
              (setq UserScale~ 1.0)
            );if
            (if (<= UserScale~ 0)
              (princ "\nScale factor must be greater than 0.")(princ)
            );if
          );while
          (setq Scale~ (rtosr UserScale~))
        );progn
      );if
      (if (/= Scale~ "User selects")
        (progn
          (if (= (type Scale~) 'STR)
            (setq Scale~ (atof Scale~))
          );if
          (if (<= Scale~ 0)
            (progn
              (GetOK (strcat LibTitle$ " Library")
                (strcat "Invalid scale factor for " Block$
                "\nblock in " DefFile$ " file.") "AlertX"
              );GetOK
              (exit)
            );progn
          );if
        );progn
      );if
      (setq Osmode (getvar "OSMODE"))
      (setvar "OSMODE" 0);None
      (if (= Explode$ "1");Yes
        (progn
          (setq PathBlock$ (strcat "*" BlkPath$ Block$))
          (if (and (= Point@ "User selects") (= Scale~ "User selects"))
            (command ".INSERT" PathBlock$)
            (command ".INSERT" PathBlock$ Point@ Scale~)
          );if
        );progn
        (progn
          (setq PathBlock$ (strcat BlkPath$ Block$))
          (if (/= Layer$ "Current layer")
            (if (tblsearch "LAYER" Layer$)
              (command ".LAYER" "T" Layer$ "U" Layer$ "ON" Layer$ "S" Layer$ "")
              (command ".LAYER" "M" Layer$ "")
            );if
          );if
          (if (and (= Point@ "User selects") (= Scale~ "User selects"))
            (command ".INSERT" PathBlock$)
            (command ".INSERT" PathBlock$ Point@ Scale~ "")
          );if
        );progn
      );if
      (setvar "OSMODE" Osmode)
    );progn
  );if
  (if (= Manager$ "Yes")
    (Blk_Mgr LibTitle$ PathDef$ PathBlk$)
  );if
  (setq *error* Old_error)
  (princ)
);defun Blk_Lib
;-------------------------------------------------------------------------------
; Blk_Lib_Msg [Block Library Message] - Installation Instructions
;-------------------------------------------------------------------------------
(defun Blk_Lib_Msg (/ Dcl_Id%)
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Blk_Lib_Msg in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (new_dialog "Blk_Lib_Msg" Dcl_Id%)
  (set_tile "msg1"
    "Note: Add the following line to the AutoLISP file that will be loading \"Blk_Lib.lsp\".")
  (set_tile "msg2"
    "(load \"Blk_Lib.lsp\")  (setq *Blk_Lib* t)")
  (set_tile "msg3"
    "Block Library saves a backup of your drawing as C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.dwg before")
  (set_tile "msg4"
    "it creates slides and wblocks.  If the program is interrupted or canceled, you may")
  (set_tile "msg5"
    "open C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.dwg as a backup.")
  (action_tile "accept" "(done_dialog)")
  (start_dialog)
  (unload_dialog Dcl_Id%)
  (exit)
);defun Blk_Lib_Msg
;-------------------------------------------------------------------------------
; c:Blk_Mgr - Calls function Blk_Mgr with default arguments.
;-------------------------------------------------------------------------------
(defun c:Blk_Mgr () (Blk_Mgr nil nil nil) (princ))
;-------------------------------------------------------------------------------
; Blk_Mgr [Block Library Manager] - Manages blocks for Library.
; Arguments: 3
;   LibTitle$ = Library title
;   PathDef$ = Pathname and DefFile name for slides
;   PathBlk$ = Pathname and DefFile name for blocks
; Returns: Adds a block to Library or calls function Edit_Lib if no blocks found
; in drawing to add.
;-------------------------------------------------------------------------------
(defun Blk_Mgr (LibTitle$ PathDef$ PathBlk$ / Attribs$ BlkList@ BlkPath$
    Blk_Lib_def: Block$ BlockMgr: DefFile$ DefPath$ Dot$ EditLib$ Explode$
    Layer$ MgrPoint~ MgrScale~ MslideList@ Mslide_Wblock: Old_error Point$
    Replace$ Scale$ TblList@
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "ATTREQ" 0) (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  ;-----------------------------------------------------------------------------
  ; Evaluate Arguments and get DefPath$ and DefFile$ variables.
  ;-----------------------------------------------------------------------------
  (if (null LibTitle$)
    (setq LibTitle$ "Block")
  );if
  (if (null PathDef$)
    (setq PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
  );if
  (setq DefPath$ (car (GetPathFile PathDef$)))
  (setq DefFile$ (cadr (GetPathFile PathDef$)))
  (if (null PathBlk$)
    (setq PathBlk$ PathDef$)
  );if
  (setq BlkPath$ (car (GetPathFile PathBlk$)))
  (princ (strcat "\n" LibTitle$ " Library Manager "))(princ)
  (Custom_Dirs LibTitle$ PathDef$ PathBlk$)
  ;-----------------------------------------------------------------------------
  ; Blk_Mgr subfunctions = BlockMgr:, Mslide_Wblock:, Blk_Lib_def:
  ;-----------------------------------------------------------------------------
  ; BlockMgr: - Controls dialog Blk_Mgr to Manage blocks for Library.
  ;-----------------------------------------------------------------------------
  (defun BlockMgr: (/ AddLibSub: Attribs$ BlkList@ Block$ BlockSub: Option# Cnt#
      Dcl_Id% Dot$ DotSub: Explode$ ExplodeSub: LayList@ Layer$ LayerSub:
      MslideData@ MslideList@ NewList@ PntList@ Point$ PointSub: ScaList@ Scale$
      ScaleSub: TblList@ ViewBlkSub: View_Blks: nthBlkList$ nthLayList$
      nthPntList$ nthScaList$
    );variables
    ;---------------------------------------------------------------------------
    ; BlockMgr: subfunctions = BlockSub:, PointSub:, LayerSub:, ScaleSub:,
    ; ExplodeSub:, ViewBlkSub:, DotSub:, AddLibSub:, View_Blks:
    ;---------------------------------------------------------------------------
    ; BlockSub:
    ;---------------------------------------------------------------------------
    (defun BlockSub: ()
      (setq nthBlkList$ (get_tile "block"))
      (set_tile "block" nthBlkList$)
      (setq Block$ (nth (atoi nthBlkList$) BlkList@))
      (start_image "blocksld")
      (fill_image 0 0 (dimx_tile "blocksld") (dimy_tile "blocksld") 0)
      (end_image)
      (start_image "blocksld")
      (slide_image 0 0 (dimx_tile "blocksld") (dimy_tile "blocksld")
        (strcat "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\" Block$ ".sld")
      );slide_image
      (end_image)
      (if (= (cdr (assoc 70 (tblsearch "BLOCK" Block$))) 2)
        (setq Attribs$ "1")
        (setq Attribs$ "0")
      );if
    );defun BlockSub:
    ;---------------------------------------------------------------------------
    ; PointSub:
    ;---------------------------------------------------------------------------
    (defun PointSub: ()
      (setq nthPntList$ (get_tile "point"))
      (set_tile "point" nthPntList$)
      (setq Point$ (nth (atoi nthPntList$) PntList@))
    );defun PointSub:
    ;---------------------------------------------------------------------------
    ; LayerSub:
    ;---------------------------------------------------------------------------
    (defun LayerSub: ()
      (setq nthLayList$ (get_tile "layer"))
      (set_tile "layer" nthLayList$)
      (setq Layer$ (nth (atoi nthLayList$) LayList@))
    );defun LayerSub:
    ;---------------------------------------------------------------------------
    ; ScaleSub:
    ;---------------------------------------------------------------------------
    (defun ScaleSub: ()
      (setq nthScaList$ (get_tile "scale"))
      (set_tile "scale" nthScaList$)
      (setq Scale$ (nth (atoi nthScaList$) ScaList@))
    );defun ScaleSub:
    ;---------------------------------------------------------------------------
    ; ExplodeSub:
    ;---------------------------------------------------------------------------
    (defun ExplodeSub: ()
      (if (= Explode$ "0")
        (setq Explode$ "1");yes
        (setq Explode$ "0");no
      );if
    );defun ExplodeSub:
    ;---------------------------------------------------------------------------
    ; ViewBlkSub:
    ;---------------------------------------------------------------------------
    (defun ViewBlkSub: (/ Ref#)
      (setq Ref# (View_Blks: BlkList@))
      (princ (strcat "\n" LibTitle$ " Library Manager "))(princ)
      (princ "\nSelect an option: ")(princ)
      (if Ref#
        (setq Block$ (nth Ref# BlkList@)
              nthBlkList$ (itoa Ref#)
        );setq
      );if
      (if (= (cdr (assoc 70 (tblsearch "BLOCK" Block$))) 2)
        (setq Attribs$ "1")
        (setq Attribs$ "0")
      );if
    );defun ViewBlkSub:
    ;---------------------------------------------------------------------------
    ; DotSub:
    ;---------------------------------------------------------------------------
    (defun DotSub: ()
      (if (= Dot$ "0")
        (setq Dot$ "1");yes
        (setq Dot$ "0");no
      );if
    );defun DotSub:
    ;---------------------------------------------------------------------------
    ; AddLibSub:
    ;---------------------------------------------------------------------------
    (defun AddLibSub: ()
      (setq MslideData@
       '(Block$ Layer$ Point$ Scale$ Explode$ Attribs$ Dot$)
      );setq
      (foreach Item MslideData@
        (if MslideList@
          (setq MslideList@ (append MslideList@ (list (eval Item))))
          (setq MslideList@ (list (eval Item)))
        );if
      );foreach
    );defun AddLibSub:
    ;---------------------------------------------------------------------------
    ; View_Blks: - Controls dialog Dwg_Blks to view blocks in drawing.
    ; Arguments: 1
    ;   BlkList@ = List of valid blocks in block table
    ; Returns: Nth Number of the block selected from BlkList@ or nil.
    ;---------------------------------------------------------------------------
    (defun View_Blks: (BlkList@ / BlockLen# Option# Dcl_Id% ImageName$ No_Blks#
        No_Pages# Pg_No# Rep# SlideRef$
      );variables
      (setq No_Pages# (fix (/ (1- (length BlkList@)) 20.0))
            Pg_No# 0
            BlockLen# (length BlkList@)
            Option# 99
      );setq
      ;-------------------------------------------------------------------------
      ; Load DCL dialog: Dwg_Blks in Blk_Lib.dcl
      ;-------------------------------------------------------------------------
      (princ "\nView Blocks ")
      (princ "\nSelect a block or option: ")(princ)
      (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
      (while (/= Option# 1)
        (new_dialog "Dwg_Blks" Dcl_Id%)
        (if (= Pg_No# No_Pages#)
          (mode_tile "next" 1)
          (mode_tile "next" 0)
        );if
        (if (= Pg_No# 0)
          (mode_tile "previous" 1)
          (mode_tile "previous" 0)
        );if
        (if (> BlockLen# 19)
          (setq No_Blks# 20)
          (setq No_Blks# BlockLen#)
        );if
        (set_tile "title" (strcat " View Blocks "
          (itoa (+ Pg_No# 1)) " of " (itoa (+ No_Pages# 1)))
        );set_tile
        (action_tile "next" "(done_dialog 4)")
        (action_tile "previous" "(done_dialog 3)")
        (action_tile "cancel" "(done_dialog 2)")
        (setq Rep# 1)
        (repeat (fix No_Blks#)
          (action_tile (strcat "sld" (itoa Rep#))
            "(setq SlideRef$ $key Pick t)"
          );action_tile
          (setq ImageName$ (nth (+ (* Pg_No# 20) (- Rep# 1)) BlkList@))
          (start_image (strcat "sld" (itoa Rep#)))
          (slide_image 0 0
            (dimx_tile (strcat "sld" (itoa Rep#)))
            (dimy_tile (strcat "sld" (itoa Rep#)))
            (strcat "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\" ImageName$ ".sld")
          );slide_image
          (end_image)
          (set_tile
            (strcat "sld" (itoa Rep#) "text") ImageName$
          );set_tile
          (setq Rep# (1+ Rep#))
        );repeat
        (setq Option# (start_dialog))
        (if (= Option# 4);next
          (setq Pg_No# (1+ Pg_No#)
                BlockLen# (- BlockLen# 20)
                SlideRef$ nil
          );setq
        );if
        (if (= Option# 3);previous
          (setq Pg_No# (- Pg_No# 1)
                BlockLen# (+ BlockLen# 20)
                SlideRef$ nil
          );setq
        );if
        (if (= Option# 2);cancel
          (setq Option# 1
                SlideRef$ nil
          );setq
        );if
      );while
      (unload_dialog Dcl_Id%)
      (if SlideRef$
        (setq Ref# (- (+ (* Pg_No# 20) (atoi (substr SlideRef$ 4))) 1))
      );if
      Ref#
    );defun View_Blks:
    ;---------------------------------------------------------------------------
    ; BlockMgr: - Start Main Function
    ;---------------------------------------------------------------------------
    ; Get BlkList@
    ;---------------------------------------------------------------------------
    (setq BlkList@ *NewBlkList@)
    ;---------------------------------------------------------------------------
    ; Get LayList@
    ;---------------------------------------------------------------------------
    (setq TblList@ (tblnext "layer" t))
    (while TblList@
      (setq
        LayList@ (cons (cdr (assoc 2 TblList@)) LayList@)
        TblList@ (tblnext "layer")
      );setq
    );while
    (setq LayList@ (acad_strlsort LayList@))
    (setq NewList@ (list "Current layer"))
    (setq Cnt# 0)
    (while (< Cnt# 100)
      (if (member (itoa Cnt#) LayList@)
        (if NewList@
          (setq NewList@ (append NewList@ (list (itoa Cnt#))))
          (setq NewList@ (list (itoa Cnt#)))
        );if
      );if
      (setq Cnt# (1+ Cnt#))
    );while
    (foreach Item LayList@
      (if (not (member Item NewList@))
        (setq NewList@ (append NewList@ (list (Capitals Item))))
      );if
    );foreach
    (setq LayList@ NewList@)
    ;---------------------------------------------------------------------------
    ; Get PntList@
    ;---------------------------------------------------------------------------
    (setq PntList@
     '( "User selects"
        "Specify point"
        "0,0"
        "Lower left limits"
        "Lower right limits"
        "Upper left limits"
        "Upper right limits"
      )
    );setq
    ;---------------------------------------------------------------------------
    ; Get ScaList@
    ;---------------------------------------------------------------------------
    (setq ScaList@
     '( "User selects"
        "Specify scale"
        "Dim scale"
        "Full scale"
      )
    );setq
    ;---------------------------------------------------------------------------
    ; Initialize local variables.
    ;---------------------------------------------------------------------------
    (if *Blk_Mgr@
      (if (and (member (nth 0 *Blk_Mgr@) BlkList@)(member (nth 1 *Blk_Mgr@) LayList@))
        (setq Block$ (nth 0 *Blk_Mgr@)
              nthBlkList$ (itoa (- (length BlkList@)(length (member Block$ BlkList@))))
              Layer$ (nth 1 *Blk_Mgr@)
              nthLayList$ (itoa (- (length LayList@)(length (member Layer$ LayList@))))
              Point$ (nth 2 *Blk_Mgr@)
              nthPntList$ (itoa (- (length PntList@)(length (member Point$ PntList@))))
              Scale$ (nth 3 *Blk_Mgr@)
              nthScaList$ (itoa (- (length ScaList@)(length (member Scale$ ScaList@))))
              Explode$ (nth 4 *Blk_Mgr@)
              Dot$ (nth 5 *Blk_Mgr@)
        );setq
      );if
    );if
    (if (not Block$)
      (setq Block$ (nth 0 BlkList@)
            nthBlkList$ "0"
            Layer$ (nth 0 LayList@)
            nthLayList$ "0"
            Point$ (nth 0 PntList@)
            nthPntList$ "0"
            Scale$ (nth 0 ScaList@)
            nthScaList$ "0"
            Explode$ "0"
            Dot$ "1"
      );setq
    );if
    (if (= (cdr (assoc 70 (tblsearch "BLOCK" Block$))) 2)
      (setq Attribs$ "1")
      (setq Attribs$ "0")
    );if
    ;---------------------------------------------------------------------------
    ; Load DCL dialog: Blk_Mgr in Blk_Lib.dcl
    ;---------------------------------------------------------------------------
    (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
    (setq Option# 99)
    (while (/= Option# 1)
      (new_dialog "Blk_Mgr" Dcl_Id%)
      (set_tile "title" (strcat " " LibTitle$ " Library Manager"))
      (start_list "block")(mapcar 'add_list BlkList@)(end_list)
      (set_tile "block" nthBlkList$)
      (start_list "point")(mapcar 'add_list PntList@)(end_list)
      (set_tile "point" nthPntList$)
      (start_list "layer")(mapcar 'add_list LayList@)(end_list)
      (set_tile "layer" nthLayList$)
      (start_list "scale")(mapcar 'add_list ScaList@)(end_list)
      (set_tile "scale" nthScaList$)
      (set_tile "explode" Explode$)
      (set_tile "dot" Dot$)
      (start_image "blocksld")
      (slide_image 0 0
        (dimx_tile "blocksld")
        (dimy_tile "blocksld")
        (strcat "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\" Block$ ".sld")
      );slide_image
      (end_image)
      (action_tile "block" "(BlockSub:)")
      (action_tile "point" "(PointSub:)")
      (action_tile "layer" "(LayerSub:)")
      (action_tile "scale" "(ScaleSub:)")
      (action_tile "explode" "(ExplodeSub:)")
      (action_tile "viewblk" "(done_dialog 5)")
      (action_tile "dot" "(DotSub:)")
      (action_tile "addlib" "(done_dialog 4)")
      (action_tile "editlib" "(done_dialog 3)")
      (action_tile "cancel" "(done_dialog 2)")
      (setq Option# (start_dialog))
      (setq *Blk_Mgr@ (list Block$ Layer$ Point$ Scale$ Explode$ Dot$))
      (if (= Option# 5);viewblk
        (progn
          (ViewBlkSub:)
          (setq Option# 99)
        );progn
      );if
      (if (= Option# 4);addlib
        (progn
          (AddLibSub:)
          (setq Option# 1)
        );progn
      );if
      (if (= Option# 3);editlib
        (setq Option# 1
              MslideList@ nil
              EditLib$ "Yes"
        );setq
      );if
      (if (= Option# 2);cancel
        (setq Option# 1
              MslideList@ nil
        );setq
      );if
    );while
    (unload_dialog Dcl_Id%)
    MslideList@
  );defun BlockMgr:
  ;-----------------------------------------------------------------------------
  ; Mslide_Wblock: - Create slide and wblock.
  ; Arguments: 2
  ;   Block$ = Block name to wblock
  ;   Dot$ = Flag to include insertion Dot with slide, "0" or "1".
  ; Returns: Creates slide and wblock.
  ;-----------------------------------------------------------------------------
  (defun Mslide_Wblock: (Block$ Dot$ / Color0# Ent1^ Ent2^ PathBlock$ Pt1@ Pt2@
      RealBlk& Rep# RevSS& SSDims& UcsIcon#
    );variables
    (SaveDwgName);Save Drawing*.dwg as a different name
    (setq Pt1@ (polar (getvar "VIEWCTR") (* pi 0.5)(/ (getvar "VIEWSIZE") 2.0)))
    (setq Pt2@ (polar Pt1@ (* pi 1.5) (getvar "VIEWSIZE")))
    (command "UNDO" "BEGIN")
    (if (ssget "X")
      (command ".ERASE" (ssget "X") "")
    );if
    (command ".LAYER" "T" "*" "U" "*" "ON" "*" "")
    (command ".LAYER" "T" "0" "U" "0" "ON" "0" "S" "0" "")
    (setq Color0# (abs (cdr (assoc 62 (tblnext "layer" t)))))
    (command ".LAYER" "M" "0" "C" "7" "" "")
    (command ".INSERT" Block$ "0,0" "" "" "")
    (command ".ZOOM" "E")
    (setq UcsIcon# (getvar "UCSICON"))
    (setvar "UCSICON" 0)
    (setq RealBlk& (ssget "L"))
    (if RealBlk&
      (progn
        (command ".EXPLODE" "L")
        (setq SSDims& (ssget "X" (list (cons 0 "DIMENSION"))))
        (if SSDims&
          (progn
            (command ".ERASE" (ssget "X") "")
            (command ".INSERT" Block$ "0,0" "" "" "")
          );progn
        );if
        (if (= Dot$ "1")
          (progn
            (command ".DONUT" "0" "0.00000001" "0,0" "")
            (setq Ent1^ (entlast))
          );progn
        );if
        (command ".ZOOM" "E")
        (if (= Dot$ "1")
          (progn
            (command ".DONUT" "0" (/ (getvar "VIEWSIZE") 25.0) "0,0" "")
            (command ".CHPROP" "L" "" "C" "6" "");Color choice optional
            (setq Ent2^ (entlast))
          );progn
        );if
        (command ".ZOOM" "0.95x")
        (command ".REGEN")
        (command ".MSLIDE" (strcat DefPath$ Block$))
        (if (= Dot$ "1")
          (progn
            (entdel Ent1^)(entdel Ent2^)
          );progn
        );if
        (setq PathBlock$ (strcat BlkPath$ Block$ ".dwg"))
        (if SSDims&
          (progn
            (if (findfile PathBlock$)
              (command ".WBLOCK" PathBlock$ "Y" Block$)
              (command ".WBLOCK" PathBlock$ Block$)
            );if
            (command ".ERASE" (ssget "X") "")
          );progn
          (progn
            (setq RevSS& (ReverseSS (ssget "X")))
            (if (findfile PathBlock$)
              (command ".WBLOCK" PathBlock$ "Y" "" "0,0" RevSS& "")
              (command ".WBLOCK" PathBlock$ "" "0,0" RevSS& "")
            );if
          );progn
        );if
      );progn
      (command ".ERASE" (ssget "X") "")
    );if
    (command "UNDO" "END")
    (command "U")
    (command ".ZOOM" "W" Pt1@ Pt2@)
    (command ".LAYER" "M" "0" "C" Color0# "" "")
    (setvar "UCSICON" UcsIcon#)
  );defun Mslide_Wblock:
  ;-----------------------------------------------------------------------------
  ; Blk_Lib_def: - Append PathDef$ file with block insertion data.
  ; Arguments: 1
  ;   MslideList@ = List of Block$, Layer$, Point$, Scale$, Explode$, Attribs$
  ; Returns: Appends PathDef$ file with new block insertion data.
  ;-----------------------------------------------------------------------------
  (defun Blk_Lib_def: (MslideList@ / BlkList$ FileName% Q$)
    (setq Q$ "\"")
    (setq BlkList$ (strcat
      Q$(nth 0 MslideList@)Q$ "," ;Block$
      Q$(nth 1 MslideList@)Q$ "," ;Layer$
      Q$(nth 2 MslideList@)Q$ "," ;Point$
      Q$(nth 3 MslideList@)Q$ "," ;Scale$
      Q$(nth 4 MslideList@)Q$ "," ;Explode$
      Q$(nth 5 MslideList@)Q$)    ;Attribs$
    );setq
    (setq FileName% (open PathDef$ "a"))
    (write-line BlkList$ FileName%)
    (close FileName%)
  );defun Blk_Lib_def:
  ;-----------------------------------------------------------------------------
  ; Blk_Mgr - Start Main Function
  ;-----------------------------------------------------------------------------
  ; Get BlkList@
  ;-----------------------------------------------------------------------------
  (setq TblList@ (tblnext "block" t))
  (while TblList@
    (if (/= (substr (cdr (assoc 2 TblList@)) 1 1) "*")
      (setq BlkList@ (cons (cdr (assoc 2 TblList@)) BlkList@))
    );if
    (setq TblList@ (tblnext "block"))
  );while
  (if BlkList@
    (progn
      (if (equal BlkList@ *BlkList@)
        (setq BlkList@ *NewBlkList@)
        (progn
          (setq *BlkList@ BlkList@);Store global *BlkList@ variable
          (setq BlkList@ (acad_strlsort BlkList@))
          (setq BlkList@ (BlkSlides BlkList@))
          (setq *NewBlkList@ BlkList@);Store global *NewBlkList@ variable
        );progn
      );if
      (if BlkList@
        (progn
          (princ "\nSelect an option: ")(princ)
          (setq MslideList@ (BlockMgr:))
        );progn
        (setq EditLib$ (GetYesNo (strcat LibTitle$ " Library Manager")
          (strcat "No valid blocks found in drawing for Library\n"
          "Manager.  Do you want to edit the Library?") "Quest")
        );setq
      );if
    );progn
    (setq EditLib$ (GetYesNo (strcat LibTitle$ " Library Manager")
      (strcat "No blocks found in drawing for Library\n"
      "Manager.  Do you want to edit the Library?") "Quest")
    );setq
  );if
  (if (and MslideList@ BlkList@)
    (progn
      (setq Block$   (nth 0 MslideList@)
            Layer$   (nth 1 MslideList@)
            Point$   (nth 2 MslideList@)
            Scale$   (nth 3 MslideList@)
            Explode$ (nth 4 MslideList@)
            Attribs$ (nth 5 MslideList@)
            Dot$     (nth 6 MslideList@)
      );setq
      (if (= Point$ "Specify point")
        (progn
          (while (null MgrPoint~)
            (setq MgrPoint~ (getpoint
              (strcat "\nInsertion point for " Block$ " block: "))
            );setq
          );while
          (setq Point$
            (strcat (rtosr (car MgrPoint~)) " " (rtosr (cadr MgrPoint~)))
          );setq
        );progn
      );if
      (if (= Point$ "0,0")
        (setq Point$ "0 0")
      );if
      (if (= Scale$ "Specify scale")
        (progn
          (while (or (null MgrScale~) (< MgrScale~ 0))
            (setq MgrScale~
              (getreal
                (strcat "\nScale factor for " Block$ " block <1>: ")
              );getreal
            );setq
            (if (null MgrScale~)
              (setq MgrScale~ 1.0)
            );if
            (if (<= MgrScale~ 0)
              (princ "\nScale factor must be greater than 0.")(princ)
            );if
          );while
          (setq Scale$ (rtosr MgrScale~))
        );progn
      );if
      (if (= Scale$ "Full scale")
        (setq Scale$ "1.0")
      );if
      (setq MslideList@ (list
        (eval Block$) (eval Layer$)   (eval Point$)
        (eval Scale$) (eval Explode$) (eval Attribs$))
      );setq
      (setq PathBlock$ (strcat DefPath$ Block$ ".dwg"))
      (if (findfile PathBlock$)
        (setq Replace$ (GetYesNo (strcat LibTitle$ " Library Manager")
          (strcat PathBlock$ "\nexists.  Do you want to replace it?") "Block")
        );setq
      );if
      (if (/= Replace$ "No")
        (progn
          (Mslide_Wblock: Block$ Dot$)
          (Blk_Lib_def: MslideList@)
          (GetOK "Block Library Manager" (strcat "The " Block$ " block was added\n"
            "to the " LibTitle$ " Library.\nContinue or press Cancel to exit.") ""
          );GetOK
          (Blk_Mgr LibTitle$ PathDef$ PathBlk$)
        );progn
      );if
    );progn
  );if
  (if (= EditLib$ "Yes")
    (Edit_Lib LibTitle$ PathDef$)
  );if
  (setq *error* Old_error)
  (princ)
);defun Blk_Mgr
;-------------------------------------------------------------------------------
; c:Blk_Menu [Block Library Menu]
;-------------------------------------------------------------------------------
(defun c:Blk_Menu (/ Cmd$ Dcl_Id% GetShortCut: Old_error ShortCutCmd$)
  ;-----------------------------------------------------------------------------
  ; c:Blk_Menu subfunction = GetShortCut:
  ;-----------------------------------------------------------------------------
  ; GetShortCut:
  ;-----------------------------------------------------------------------------
  (defun GetShortCut: (/ Char$ Cnt# TabFlag)
    (setq ShortCutCmd$ (nth (atoi (get_tile "CommandList")) CommandList@))
    (if (/= (substr ShortCutCmd$ 1 1) " ")
      (progn
        (setq Cnt# 1 Cmd$ "")
        (while (<= Cnt# (strlen ShortCutCmd$))
          (setq Char$ (substr ShortCutCmd$ Cnt# 1))
          (if (= Char$ "\t")(setq TabFlag t))
          (if (not TabFlag)
            (setq Cmd$ (strcat Cmd$ Char$))
          );if
          (setq Cnt# (1+ Cnt#))
        );while
        (if (/= Cmd$ "")
          (setq ShortCutCmd$ (strcat "(c:" Cmd$ ")"))
          (setq ShortCutCmd$ nil)
        );if
      );progn
      (setq ShortCutCmd$ nil)
    );if
  );defun GetShortCut:
  ;-----------------------------------------------------------------------------
  ; c:Blk_Menu - Start Main Function
  ;-----------------------------------------------------------------------------
  (setq Old_error *error* *error* *BL_error*)
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Blk_Menu in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (princ "\nBlock Library Menu")
  (princ "\nSelect a Block Library Menu Command or option: ")(princ)
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (new_dialog "Blk_Menu" Dcl_Id%)
  (setq CommandList@ (list
    "ALLBL\tAll_Blk_Lib\tAll Blocks to Library"
    "BLM\tBlk_Mgr\tBlock Library Manager"
    "BLMENU\tBlk_Menu\tBlock Library Menu"
    "DB\tDwg_Blks\tDrawing Blocks"
    "EDBL\tEdit_Lib\tEdit Block Library"
    "INBL\tInBlocks\tInsert Blocks from folder"
    "LIB\tLibrary\tUser Block Library"
    "SBL\tSel_Lib\tSelect Block Library"
    "ADL\tAdd_Dwgs\tAdd Drawings to Library"
    );list
  );setq
  (start_list "CommandList")
  (mapcar 'add_list CommandList@)
  (end_list)
  (action_tile "CommandList" "(GetShortCut:)(done_dialog)")
  (action_tile "cancel" "(done_dialog)")
  (start_dialog)
  (unload_dialog Dcl_Id%)
  (if ShortCutCmd$
    (progn
      (princ "\nCommand: ")(princ Cmd$)
      (eval (read ShortCutCmd$))
    );progn
  );if
  (setq *error* Old_error)
  (princ)
);defun c:Blk_Menu
;-------------------------------------------------------------------------------
; BlkSlides - Creates slides of all blocks in drawing.
;-------------------------------------------------------------------------------
(defun BlkSlides (BlkList@ / Block$ Color0# NewBlkList@ PathSld$ Pt1@ Pt2@ RealBlk& UcsIcon#
  );variables
  (SaveDwgName);Save Drawing*.dwg as a different name
  (princ "\nUpdating slides ")(princ)
  (runapp "DEL C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\*.sld" nil);Delete old temp slides
  (setq Pt1@ (polar (getvar "VIEWCTR") (* pi 0.5)(/ (getvar "VIEWSIZE") 2.0)))
  (setq Pt2@ (polar Pt1@ (* pi 1.5) (getvar "VIEWSIZE")))
  (command "UNDO" "BEGIN")
  (if (ssget "X")
    (command ".ERASE" (ssget "X") "")
  );if
  (command ".LAYER" "T" "*" "U" "*" "ON" "*" "")
  (command ".LAYER" "T" "0" "U" "0" "ON" "0" "S" "0" "")
  (setq Color0# (abs (cdr (assoc 62 (tblnext "layer" t)))))
  (command ".LAYER" "M" "0" "C" "7" "" "")
  (setq UcsIcon# (getvar "UCSICON"))
  (setvar "UCSICON" 0)
  (foreach Block$ BlkList@
    (setq PathSld$ (strcat "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\" Block$))
    (command ".INSERT" Block$ "0,0" "" "" "")
    (command ".ZOOM" "E")
    (setq RealBlk& (ssget "L"))
    (if RealBlk&
      (progn
        (command ".EXPLODE" "L")
        (command ".DONUT" "0" "0.00000001" "0,0" "")
        (command ".ZOOM" "E")
        (command ".DONUT" "0" (/ (getvar "VIEWSIZE") 25.0) "0,0" "")
        (command ".CHPROP" "L" "" "C" "6" "");Color choice optional
        (command ".ZOOM" "0.95x")
        (command ".REGEN")
        (command ".MSLIDE" PathSld$)
        (command ".ERASE" (ssget "X") "")
        (if NewBlkList@
          (setq NewBlkList@ (append NewBlkList@ (list Block$)))
          (setq NewBlkList@ (list Block$))
        );if
      );progn
      (command ".ERASE" (ssget "X") "")
    );if
    (Whirl)
  );foreach
  (command ".ZOOM" "W" Pt1@ Pt2@)
  (command ".LAYER" "M" "0" "C" Color0# "" "")
  (command "UNDO" "END")
  (command "U")
  (setvar "UCSICON" UcsIcon#)
  (princ " ")
  NewBlkList@
);defun BlkSlides
;-------------------------------------------------------------------------------
; Custom_Dirs - Creates PathDef$, PathBlk$ and C:\Autodesk\Civil_3D\BLK\_Config\Blk_Sld folders for
; storing slides and blocks.
; Arguments: 3
;   LibTitle$ = Library title
;   PathDef$ = Pathname and DefFile name for slides
;   PathBlk$ = Pathname and DefFile name for blocks
; Returns: Creates folders for DefFiles, slides and blocks for Library.
;-------------------------------------------------------------------------------
(defun Custom_Dirs (LibTitle$ PathDef$ PathBlk$ / BlkFile$ Cnt# DefFile$ Echo1$
    Echo2$ Echo3$ Echo4$ FileName% MadeIt NewDir$ Path1$ Path2$ Path3$ Path4$
    Path5$ PathFile$ Q$ Text$ WriteText$
  );variables
  (if (not (findfile "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Temp.bat"))
    (vl-mkdir "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config")
  );if
  (if (not *Blk_Lib*);Check for global *Blk_Lib* variable
    (Blk_Lib_Msg);Installation Instructions
  );if
  ;-----------------------------------------------------------------------------
  ; Evaluate Arguments and get DefFile$ variable.
  ;-----------------------------------------------------------------------------
  (if (null LibTitle$)
    (setq LibTitle$ "Block")
  );if
  (if (null PathDef$)
    (setq PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
  );if
  (if (null PathBlk$)
    (setq PathBlk$ PathDef$)
  );if
  (setq Q$ "\""
        Text$ (strcat ";" (String$ 79 "-"))
  );setq
  ;-----------------------------------------------------------------------------
  ; Create C:\Autodesk\Civil_3D\BLK\_Config\Blk_Temp.bat file to create new folders and files.
  ;-----------------------------------------------------------------------------
  (setq FileName% (open "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Temp.bat" "w"))
  (write-line ":---- Blk_Temp.bat ----" FileName%)
  (write-line "@ECHO OFF" FileName%)
  (write-line "C:" FileName%)
  (if (and
      (/= PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
      (/= PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Sel_Lib.dat")
      (/= PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\Blk_Sld.def")
    );and
    (progn
      (if (not (findfile PathDef$))
        (progn
          (setq Path1$ (strcat PathDef$ "\n"))
          (setq DefFile$ (cadr (GetPathFile PathDef$)))
          (setq Echo1$ (strcat "ECHO " Text$ " > " Q$ PathDef$ Q$))
          (setq Echo2$ (strcat "ECHO ; " DefFile$ " >> " Q$ PathDef$ Q$))
          (setq Echo3$ (strcat "ECHO " Text$ ">> " Q$ PathDef$ Q$))
          (setq Cnt# 1)
          (while (< Cnt# (strlen PathDef$))
            (if (and (= (substr PathDef$ Cnt# 1) (chr 92)) (/= Cnt# 3))
              (progn
                (setq NewDir$ (strcat Q$ (substr PathDef$ 1 (- Cnt# 1)) Q$))
                (vl-mkdir (strcat Q$ (substr PathDef$ 1 (- Cnt# 1)) Q$))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (write-line Echo1$ FileName%)
          (write-line Echo2$ FileName%)
          (write-line Echo3$ FileName%)
          (setq MadeIt t)
        );progn
      );if
      (if (/= PathBlk$ PathDef$)
        (if (not (findfile PathBlk$))
          (progn
            (setq Path2$ (strcat PathBlk$ "\n"))
            (setq BlkFile$ (cadr (GetPathFile PathBlk$)))
            (setq Echo1$ (strcat "ECHO " Text$ " > " Q$ PathBlk$ Q$))
            (setq Echo2$ (strcat "ECHO ; " BlkFile$ " >> " Q$ PathBlk$ Q$))
            (setq Echo3$ (strcat "ECHO " Text$ ">> " Q$ PathBlk$ Q$))
            (setq Cnt# 1)
            (while (< Cnt# (strlen PathBlk$))
              (if (and (= (substr PathBlk$ Cnt# 1) (chr 92)) (/= Cnt# 3))
                (progn
                  (setq NewDir$ (strcat Q$ (substr PathBlk$ 1 (- Cnt# 1)) Q$))
                  (vl-mkdir (strcat Q$ (substr PathBlk$ 1 (- Cnt# 1)) Q$))
                );progn
              );if
              (setq Cnt# (1+ Cnt#))
            );while
            (write-line Echo1$ FileName%)
            (write-line Echo2$ FileName%)
            (write-line Echo3$ FileName%)
            (setq MadeIt t)
          );progn
        );if
      );if
    );progn
  );if
  (setq PathFile$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
  (if (not (findfile PathFile$))
    (progn
      (vl-mkdir "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib")
      (setq Path4$ (strcat PathFile$ "\n"))
      (setq Echo1$ (strcat "ECHO " Text$ " > " PathFile$))
      (setq Echo2$ (strcat "ECHO ; Blk_Lib.def >> " PathFile$))
      (setq Echo3$ (strcat "ECHO " Text$ ">> " PathFile$))
      (write-line Echo1$ FileName%)
      (write-line Echo2$ FileName%)
      (write-line Echo3$ FileName%)
      (setq MadeIt t)
    );progn
  );if
  (setq PathFile$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Sel_Lib.dat")
  (if (not (findfile PathFile$))
    (progn
      (vl-mkdir "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib")
      (setq Path5$ (strcat PathFile$ "\n"))
      (setq Echo1$ (strcat "ECHO " Text$ " > " PathFile$))
      (setq Echo2$ (strcat "ECHO ; Sel_Lib.dat >> " PathFile$))
      (setq Echo3$ (strcat "ECHO " Text$ ">> " PathFile$))
      (setq WriteText$ (strcat
        Q$ "Block" Q$ "," Q$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def" Q$ ","
        Q$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def" Q$)
      );setq
      (setq Echo4$ (strcat "ECHO " WriteText$ ">> " PathFile$))
      (write-line Echo1$ FileName%)
      (write-line Echo2$ FileName%)
      (write-line Echo3$ FileName%)
      (write-line Echo4$ FileName%)
      (setq MadeIt t)
    );progn
  );if
  (setq PathFile$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\Blk_Sld.def")
  (if (not (findfile PathFile$))
    (progn
      (vl-mkdir "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld")
      (setq Path3$ (strcat PathFile$ "\n"))
      (setq Echo1$ (strcat "ECHO " Text$ " > " PathFile$))
      (setq Echo2$ (strcat "ECHO ; Blk_Sld.def >> " PathFile$))
      (setq Echo3$ (strcat "ECHO " Text$ ">> " PathFile$))
      (write-line Echo1$ FileName%)
      (write-line Echo2$ FileName%)
      (write-line Echo3$ FileName%)
      (setq MadeIt t)
    );progn
  );if
  (setq PathFile$ "C:\\Temp\\Temp.dcl")
  (if (not (findfile PathFile$))
    (progn
      (vl-mkdir "C:\\Temp")
      (setq Echo1$ (strcat "ECHO // > " PathFile$))
      (write-line Echo1$ FileName%)
      (setq MadeIt t)
    );progn
  );if
  (close FileName%)
  (if MadeIt
    (progn
      (runapp "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Temp.bat" nil)
      (setq LibTitle$ nil PathDef$ nil PathBlk$ nil)
    );progn
  );if
);defun Custom_Dirs
;-------------------------------------------------------------------------------
; c:Dwg_Blks [Drawing Blocks] - View and select drawing blocks.
;-------------------------------------------------------------------------------
(defun c:Dwg_Blks (/ Blk_Sld_def: BlkList@ Dwg_Blks: Old_error TblList@
  );variables
  (setvar "ATTREQ" 0) (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  ;-----------------------------------------------------------------------------
  ; c:Dwg_Blks subfunctions = Blk_Sld_def:, Dwg_Blks:
  ;-----------------------------------------------------------------------------
  ; Blk_Sld_def: - Create Blk_Sld.def file.
  ;-----------------------------------------------------------------------------
  (defun Blk_Sld_def: (BlkList@ / Attribs$ BlockData$ FileName% PathFile$ Q$ Text$
    );variables
    (setq Text$ (strcat ";" (String$ 79 "-")))
    (setq PathFile$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\Blk_Sld.def")
    (setq FileName% (open PathFile$ "w"))
    (write-line Text$ FileName%)
    (write-line "; Blk_Sld.def" FileName%)
    (write-line Text$ FileName%)
    (setq Q$ "\"")
    (foreach Block$ BlkList@
      (if (= (cdr (assoc 70 (tblsearch "BLOCK" Block$))) 2)
        (setq Attribs$ "1")
        (setq Attribs$ "0")
      );if
      (setq BlockData$ (strcat
        Q$ Block$ Q$ ","          ;Block
        Q$ "Current layer" Q$ "," ;Layer
        Q$ "User selects" Q$ ","  ;Point
        Q$ "User selects" Q$ ","  ;Scale
        Q$ "0" Q$ ","             ;Explode
        Q$ Attribs$ Q$)           ;Attribs
      );setq
      (write-line BlockData$ FileName%)
    );foreach
    (close FileName%)
  );defun Blk_Sld_def:
  ;-----------------------------------------------------------------------------
  ; Dwg_Blks: - Controls dialog Dwg_Blks.dcl to view select drawing blocks.
  ;-----------------------------------------------------------------------------
  (defun Dwg_Blks: (LibTitle$ DefFile$ / Attribs$ AttribsList@ BlkList@ Block$
      BlockLen# Option# Cnt# Dcl_Id% EOF Explode$ ExplodeList@ Field# FileName%
      ImageName$ Item$ Layer$ LayerList@ Mid$ No_Blks# No_Pages# PathBlock$
      PathFile$ Pg_No# Pick Point@ PointList@ Q$ Ref# Rep# ScaleList@ Scale~
      SlideRef$ StartNo# Text$ UserPoint@ UserScale~
    );variables
    ;---------------------------------------------------------------------------
    ; Get Lists from DefFile$.
    ;---------------------------------------------------------------------------
    (if (setq PathFile$ (findfile (strcat "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\" DefFile$)))
      (setq FileName% (open PathFile$ "r"))
      (progn
        (GetOK (strcat LibTitle$ " Library")
          (strcat DefFile$ " file not found.") "AlertX"
        );GetOK
        (exit)
      );progn
    );if
    (setq Q$ "\"")
    (setq EOF nil)(setq Item$ "")
    (while (null EOF)
      (setq Text$ (read-line FileName%))
      (if Text$
        (if (= (substr Text$ 1 1) Q$)
          (progn
            (setq StartNo# 2)
            (setq Cnt# 1)
            (setq Field# 1)
            (while (and (<= Cnt# (strlen Text$)) (< Field# 6))
              (setq Mid$ (substr Text$ Cnt# 3))
              (if (= Mid$ "\",\"")
                (progn
                  (setq Item$
                    (substr Text$ StartNo# (- Cnt# StartNo#))
                  );setq
                  (cond
                    ((= Field# 1)
                      (if BlkList@
                        (setq BlkList@ (append BlkList@ (list Item$)))
                        (setq BlkList@ (list Item$))
                      );if
                    );Field# 1
                    ((= Field# 2)
                      (if LayerList@
                        (setq LayerList@ (append LayerList@ (list Item$)))
                        (setq LayerList@ (list Item$))
                      );if
                    );Field# 2
                    ((= Field# 3)
                      (if PointList@
                        (setq PointList@ (append PointList@ (list Item$)))
                        (setq PointList@ (list Item$))
                      );if
                    );Field# 3
                    ((= Field# 4)
                      (if ScaleList@
                        (setq ScaleList@ (append ScaleList@ (list Item$)))
                        (setq ScaleList@ (list Item$))
                      );if
                    );Field# 4
                    ((= Field# 5)
                      (if ExplodeList@
                        (setq ExplodeList@ (append ExplodeList@ (list Item$)))
                        (setq ExplodeList@ (list Item$))
                      );if
                    );Field# 5
                    (t (exit))
                  );cond
                  (setq Field# (1+ Field#))
                  (setq StartNo# (+ Cnt# 3))
                );progn
              );if
              (setq Cnt# (1+ Cnt#))
            );while
            (setq Item$
              (substr Text$ StartNo# (- (strlen Text$) StartNo#))
            );setq
            (if AttribsList@
              (setq AttribsList@ (append AttribsList@ (list Item$)))
              (setq AttribsList@ (list Item$))
            );if
          );progn
        );if
        (setq EOF t)
      );if
    );while
    (close FileName%)
    (setq No_Pages# (fix (/ (1- (length BlkList@)) 20.0))
          Pg_No# 0
          BlockLen# (length BlkList@)
          Option# 99
    );setq
    ;---------------------------------------------------------------------------
    ; Load DCL dialog: Dwg_Blks in Blk_Lib.dcl
    ;---------------------------------------------------------------------------
    (princ "\nSelect a block or option: ")(princ)
    (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
    (while (/= Option# 1)
      (new_dialog "Dwg_Blks" Dcl_Id%)
      (if (= Pg_No# No_Pages#)
        (mode_tile "next" 1)
        (mode_tile "next" 0)
      );if
      (if (= Pg_No# 0)
        (mode_tile "previous" 1)
        (mode_tile "previous" 0)
      );if
      (if (> BlockLen# 19)
        (setq No_Blks# 20)
        (setq No_Blks# BlockLen#)
      );if
      (set_tile "title" (strcat " " LibTitle$ " "
        (itoa (+ Pg_No# 1)) " of " (itoa (+ No_Pages# 1)))
      );set_tile
      (action_tile "next" "(done_dialog 4)")
      (action_tile "previous" "(done_dialog 3)")
      (action_tile "cancel" "(done_dialog 2)")
      (setq Rep# 1)
      (repeat (fix No_Blks#)
        (action_tile (strcat "sld" (itoa Rep#)) "(setq SlideRef$ $key Pick t)")
        (setq ImageName$ (nth (+ (* Pg_No# 20) (- Rep# 1)) BlkList@))
        (start_image (strcat "sld" (itoa Rep#)))
        (slide_image 0 0
          (dimx_tile (strcat "sld" (itoa Rep#)))
          (dimy_tile (strcat "sld" (itoa Rep#)))
          (strcat "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\" ImageName$ ".sld")
        );slide_image
        (end_image)
        (set_tile
          (strcat "sld" (itoa Rep#) "text") ImageName$
        );set_tile
        (setq Rep# (1+ Rep#))
      );repeat
      (setq Option# (start_dialog))
      (if (= Option# 4);next
        (setq Pg_No# (1+ Pg_No#)
              BlockLen# (- BlockLen# 20)
              SlideRef$ nil
        );setq
      );if
      (if (= Option# 3);previous
        (setq Pg_No# (- Pg_No# 1)
              BlockLen# (+ BlockLen# 20)
              SlideRef$ nil
        );setq
      );if
      (if (= Option# 2);cancel
        (setq Option# 1
              SlideRef$ nil
        );setq
      );if
    );while
    (unload_dialog Dcl_Id%)
    (if (and SlideRef$ Pick)
      (progn
        (setq Ref# (- (+ (* Pg_No# 20) (atoi (substr SlideRef$ 4))) 1))
        (setq Block$   (nth Ref# BlkList@)
              Layer$   (nth Ref# LayerList@)
              Point@   (nth Ref# PointList@)
              Scale~   (nth Ref# ScaleList@)
              Explode$ (nth Ref# ExplodeList@)
              Attribs$ (nth Ref# AttribsList@)
        );setq
        (if (= Block$ " ")
          (exit)
        );if
        (if (= Point@ "Lower left limits")
          (setq Point@ (getvar "LIMMIN"))
        );if
        (if (= Point@ "Lower right limits")
          (setq Point@ (list (car (getvar "LIMMAX")) (cadr (getvar "LIMMIN"))))
        );if
        (if (= Point@ "Upper left limits")
          (setq Point@ (list (car (getvar "LIMMIN")) (cadr (getvar "LIMMAX"))))
        );if
        (if (= Point@ "Upper right limits")
          (setq Point@ (getvar "LIMMAX"))
        );if
        (if (and (= Point@ "User selects") (/= Scale~ "User selects"))
          (progn
            (while (null UserPoint@)
              (setq UserPoint@ (getpoint
                (strcat "\nInsertion point for " Block$ " block: "))
              );setq
            );while
            (setq Point@ UserPoint@)
          );progn
        );if
        (if (/= Point@ "User selects")
          (progn
            (if (= (type Point@) 'STR)
              (setq Point@ (RealList Point@))
            );if
            (if (/= (type Point@) 'LIST)
              (progn
                (GetOK (strcat LibTitle$ " Library")
                  (strcat "Invalid insertion point for " Block$
                  "\nblock in " DefFile$ " file.") "AlertX"
                );GetOK
                (exit)
              );progn
            );if
          );progn
        );if
        (if (= Scale~ "Dim scale")
          (if (= (getvar "DIMSCALE") 0)
            (progn
              (GetOK (strcat LibTitle$ " Library")
                "DIMSCALE value must be greater than 0." "AlertX"
              );GetOK
              (exit)
            );progn
            (setq Scale~ (getvar "DIMSCALE"))
          );if
        );if
        (if (and (= Scale~ "User selects") (/= Point@ "User selects"))
          (progn
            (while (or (null UserScale~) (< UserScale~ 0))
              (setq UserScale~
                (getreal
                  (strcat "\nScale factor for " Block$ " block <1>: ")
                );getreal
              );setq
              (if (null UserScale~)
                (setq UserScale~ 1.0)
              );if
              (if (<= UserScale~ 0)
                (princ "\nScale factor must be greater than 0.")(princ)
              );if
            );while
            (setq Scale~ (rtosr UserScale~))
          );progn
        );if
        (if (/= Scale~ "User selects")
          (progn
            (if (= (type Scale~) 'STR)
              (setq Scale~ (atof Scale~))
            );if
            (if (<= Scale~ 0)
              (progn
                (GetOK (strcat LibTitle$ " Library")
                  (strcat "Invalid scale factor for " Block$
                  "\nblock in " DefFile$ " file.") "AlertX"
                );GetOK
                (exit)
              );progn
            );if
          );progn
        );if
        (setq Osmode (getvar "OSMODE"))
        (setvar "OSMODE" 0);None
        (if (= Explode$ "1");Yes
          (progn
            (setq PathBlock$ (strcat "*" Block$))
            (if (and (= Point@ "User selects") (= Scale~ "User selects"))
              (command ".INSERT" PathBlock$)
              (command ".INSERT" PathBlock$ Point@ Scale~)
            );if
          );progn
          (progn
            (setq PathBlock$ Block$)
            (if (/= Layer$ "Current layer")
              (if (tblsearch "LAYER" Layer$)
                (command ".LAYER" "T" Layer$ "U" Layer$
                  "ON" Layer$ "S" Layer$ ""
                );command
                (command ".LAYER" "M" Layer$ "")
              );if
            );if
            (if (and (= Point@ "User selects") (= Scale~ "User selects"))
              (command ".INSERT" PathBlock$)
              (command ".INSERT" PathBlock$ Point@ Scale~ "")
            );if
          );progn
        );if
        (setvar "OSMODE" Osmode)
      );progn
    );if
    (princ)
  );defun Dwg_Blks:
  ;-----------------------------------------------------------------------------
  ; c:Dwg_Blks - Start Main Function
  ;-----------------------------------------------------------------------------
  (setq Old_error *error* *error* *BL_error*)
  (SaveDwgName);Save Drawing*.dwg as a different name
  (princ "\nDrawing Blocks ")(princ)
  (Custom_Dirs "Drawing Blocks" nil nil)
  ;-----------------------------------------------------------------------------
  ; Get BlkList@
  ;-----------------------------------------------------------------------------
  (setq TblList@ (tblnext "block" t))
  (while TblList@
    (if (/= (substr (cdr (assoc 2 TblList@)) 1 1) "*")
      (setq BlkList@ (cons (cdr (assoc 2 TblList@)) BlkList@))
    );if
    (setq TblList@ (tblnext "block"))
  );while
  (if BlkList@
    (progn
      (if (equal BlkList@ *BlkList@)
        (setq BlkList@ *NewBlkList@)
        (progn
          (setq *BlkList@ BlkList@);Store global *BlkList@ variable
          (setq BlkList@ (acad_strlsort BlkList@))
          (setq BlkList@ (BlkSlides BlkList@))
          (setq *NewBlkList@ BlkList@);Store global *NewBlkList@ variable
        );progn
      );if
      (if BlkList@
        (progn
          (Blk_Sld_def: BlkList@)
          (Dwg_Blks: "Drawing Blocks" "Blk_Sld.def")
        );progn
        (GetOK "Drawing Blocks" "No valid blocks found in drawing." "AlertX")
      );if
    );progn
    (GetOK "Drawing Blocks" "No blocks found in drawing." "AlertX")
  );if
  (setq *error* Old_error)
  (princ)
);defun c:Dwg_Blks
;-------------------------------------------------------------------------------
; c:Edit_Lib - Calls function Edit_Lib with default arguments.
;-------------------------------------------------------------------------------
(defun c:Edit_Lib () (Edit_Lib nil nil))
;-------------------------------------------------------------------------------
; Edit_Lib [Edit Block Library] - Rearrange or delete block insertion data.
; Arguments: 2
;   LibTitle$ = Library title
;   PathDef$ = Pathname and DefFile name for slides
; Returns: Rearrange or delete lines of block insertion data in PathDef$ file.
;-------------------------------------------------------------------------------
(defun Edit_Lib (LibTitle$ PathDef$ / AttribsList@ BlkList@ BlockLen# Option#
    Cnt# DefFile$ DefPath$ Dcl_Id% EOF ExplodeList@ Field# FileName% ImageName$
    Item$ LayerList@ Mid$ MoveRef# No_Blks# No_Pages# Old_error Pg_No# PointList@
    Q$ Ref# Rep# ScaleList@ SlideRef$ StartNo# Text$ Verify: WriteData@
  );variables
  ;-----------------------------------------------------------------------------
  ; Verify: - Verifies options
  ;-----------------------------------------------------------------------------
  (defun Verify: (Option#)
    (if (= Option# 6);move
      (if SlideRef$
        (progn
          (setq MoveRef# (- (+ (* Pg_No# 20) (atoi (substr SlideRef$ 4))) 1))
          (setq SlideRef$ nil)
        );progn
        (GetOK (strcat "Edit " LibTitle$ " Library")
          (strcat "Select an image to move then select Move. Then"
          "\nselect a new image location and select Paste.") "Inform"
        );GetOK
      );if
    );if
    (if (= Option# 5);paste
      (if (and SlideRef$ MoveRef#)
        (progn
          (setq Ref# (- (+ (* Pg_No# 20) (atoi (substr SlideRef$ 4))) 1))
          (if (/= Ref# MoveRef#)
            (progn
              (setq BlkList@     (Move_nth MoveRef# Ref# BlkList@)
                    LayerList@   (Move_nth MoveRef# Ref# LayerList@)
                    PointList@   (Move_nth MoveRef# Ref# PointList@)
                    ScaleList@   (Move_nth MoveRef# Ref# ScaleList@)
                    ExplodeList@ (Move_nth MoveRef# Ref# ExplodeList@)
                    AttribsList@ (Move_nth MoveRef# Ref# AttribsList@)
              );setq
              (setq SlideRef$ nil
                    MoveRef# nil
              );setq
              (done_dialog 5)
            );progn
            (progn
              (GetOK (strcat "Edit " LibTitle$ " Library")
                (strcat "The move and paste image"
                "\nlocations cannot be the same.") "AlertX"
              );GetOK
              (setq SlideRef$ nil
                    MoveRef# nil
              );setq
            );progn
          );if
        );progn
        (progn
          (GetOK (strcat "Edit " LibTitle$ " Library")
            (strcat "Select an image to move then select Move. Then"
            "\nselect a new image location and select Paste.") "Inform"
          );GetOK
          (setq SlideRef$ nil
                MoveRef# nil
          );setq
        );progn
      );if
    );if
    (if (= Option# 4);delete
      (if SlideRef$
        (if (= (GetYesNo (strcat "Edit " LibTitle$ " Library")
            (strcat "Are you sure you want to delete this"
            "\nimage definition in the " LibTitle$ " Library?") "Delete")
            "Yes")
          (progn
            (setq Ref# (- (+ (* Pg_No# 20) (atoi (substr SlideRef$ 4))) 1))
            (setq BlkList@     (Delete_nth Ref# BlkList@)
                  LayerList@   (Delete_nth Ref# LayerList@)
                  PointList@   (Delete_nth Ref# PointList@)
                  ScaleList@   (Delete_nth Ref# ScaleList@)
                  ExplodeList@ (Delete_nth Ref# ExplodeList@)
                  AttribsList@ (Delete_nth Ref# AttribsList@)
                  No_Pages# (fix (/ (1- (length BlkList@)) 20.0))
                  BlockLen# (1- BlockLen#)
                  SlideRef$ nil
                  MoveRef# nil
            );setq
            (if (= BlockLen# 0)
              (setq Pg_No# No_Pages#
                    BlockLen# (+ BlockLen# 20)
              );setq
            );if
            (done_dialog 4)
          );progn
        );if
        (GetOK (strcat "Edit " LibTitle$ " Library")
          "Select an image to delete then select Delete." "Inform"
        );GetOK
      );if
    );if
  );defun Verify:
  ;-----------------------------------------------------------------------------
  (setq Old_error *error* *error* *BL_error*)
  (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  ;-----------------------------------------------------------------------------
  ; Evaluate Arguments and get DefPath$ and DefFile$ variables.
  ;-----------------------------------------------------------------------------
  (if (null LibTitle$)
    (setq LibTitle$ "Block")
  );if
  (if (null PathDef$)
    (setq PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
  );if
  (setq DefPath$ (car (GetPathFile PathDef$)))
  (setq DefFile$ (cadr (GetPathFile PathDef$)))
  (princ (strcat "\nEdit " LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDef$ nil)
  ;-----------------------------------------------------------------------------
  ; Get Lists from PathDef$ file.
  ;-----------------------------------------------------------------------------
  (if (findfile PathDef$)
    (setq FileName% (open PathDef$ "r"))
    (progn
      (GetOK (strcat LibTitle$ " Library")
        (strcat DefFile$ " file not found\nfor " LibTitle$ " Library.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (setq Q$ "\"") (setq Rep# 0)
  (setq EOF nil) (setq Item$ "")
  (while (null EOF)
    (setq Rep# (1+ Rep#))
    (if (= Rep# 20)
      (progn
        (setq Rep# 0)
        (Whirl)
      );progn
    );if
    (setq Text$ (read-line FileName%))
    (if Text$
      (if (= (substr Text$ 1 1) Q$)
        (progn
          (setq StartNo# 2)
          (setq Cnt# 1)
          (setq Field# 1)
          (while (and (<= Cnt# (strlen Text$)) (< Field# 6))
            (setq Mid$ (substr Text$ Cnt# 3))
            (if (= Mid$ "\",\"")
              (progn
                (setq Item$
                  (substr Text$ StartNo# (- Cnt# StartNo#))
                );setq
                (cond
                  ((= Field# 1)
                    (if BlkList@
                      (setq BlkList@ (append BlkList@ (list Item$)))
                      (setq BlkList@ (list Item$))
                    );if
                  );Field# 1
                  ((= Field# 2)
                    (if LayerList@
                      (setq LayerList@ (append LayerList@ (list Item$)))
                      (setq LayerList@ (list Item$))
                    );if
                  );Field# 2
                  ((= Field# 3)
                    (if PointList@
                      (setq PointList@ (append PointList@ (list Item$)))
                      (setq PointList@ (list Item$))
                    );if
                  );Field# 3
                  ((= Field# 4)
                    (if ScaleList@
                      (setq ScaleList@ (append ScaleList@ (list Item$)))
                      (setq ScaleList@ (list Item$))
                    );if
                  );Field# 4
                  ((= Field# 5)
                    (if ExplodeList@
                      (setq ExplodeList@ (append ExplodeList@ (list Item$)))
                      (setq ExplodeList@ (list Item$))
                    );if
                  );Field# 5
                  (t (exit))
                );cond
                (setq Field# (1+ Field#))
                (setq StartNo# (+ Cnt# 3))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (setq Item$
            (substr Text$ StartNo# (- (strlen Text$) StartNo#))
          );setq
          (if AttribsList@
            (setq AttribsList@ (append AttribsList@ (list Item$)))
            (setq AttribsList@ (list Item$))
          );if
        );progn
      );if
      (setq EOF t)
    );if
  );while
  (princ " ")
  (close FileName%)
  (setq No_Pages# (fix (/ (1- (length BlkList@)) 20.0))
        Pg_No# 0
        BlockLen# (length BlkList@)
        Option# 99
  );setq
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Edit_Lib in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (princ "\nSelect a block or option: ")(princ)
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (while (> Option# 2)
    (new_dialog "Edit_Lib" Dcl_Id%)
    (if (= Pg_No# No_Pages#)
      (mode_tile "next" 1)
      (mode_tile "next" 0)
    );if
    (if (= Pg_No# 0)
      (mode_tile "previous" 1)
      (mode_tile "previous" 0)
    );if
    (if (> BlockLen# 19)
      (setq No_Blks# 20)
      (setq No_Blks# BlockLen#)
    );if
    (set_tile "title" (strcat " Edit " LibTitle$ " Library "
      (itoa (+ Pg_No# 1)) " of " (itoa (+ No_Pages# 1)))
    );set_tile
    (action_tile "previous" "(done_dialog 8)")
    (action_tile "next" "(done_dialog 7)")
    (action_tile "move" "(Verify: 6)")
    (action_tile "paste" "(Verify: 5)")
    (action_tile "delete" "(Verify: 4)")
    (action_tile "accept" "(done_dialog 3)")
    (action_tile "cancel" "(done_dialog 2)")
    (setq Rep# 1)
    (repeat (fix No_Blks#)
      (action_tile (strcat "sld" (itoa Rep#)) "(setq SlideRef$ $key)")
      (setq ImageName$ (nth (+ (* Pg_No# 20) (- Rep# 1)) BlkList@))
      (start_image (strcat "sld" (itoa Rep#)))
      (slide_image 0 0
        (dimx_tile (strcat "sld" (itoa Rep#)))
        (dimy_tile (strcat "sld" (itoa Rep#)))
        (strcat DefPath$ ImageName$ ".sld")
      );slide_image
      (end_image)
      (set_tile
        (strcat "sld" (itoa Rep#) "text") ImageName$
      );set_tile
      (setq Rep# (1+ Rep#))
    );repeat
    (setq Option# (start_dialog))
    (if (= Option# 8);previous
      (setq Pg_No# (- Pg_No# 1)
            BlockLen# (+ BlockLen# 20)
            SlideRef$ nil
      );setq
    );if
    (if (= Option# 7);next
      (setq Pg_No# (1+ Pg_No#)
            BlockLen# (- BlockLen# 20)
            SlideRef$ nil
      );setq
    );if
    (if (= Option# 3);accept
      (progn
        (setq Text$ (strcat ";" (String$ 79 "-")))
        (setq FileName% (open PathDef$ "w"))
        (write-line Text$ FileName%)
        (write-line (strcat "; " DefFile$) FileName%)
        (write-line Text$ FileName%)
        (setq Q$ "\"")(setq Rep# 0)
        (foreach Item BlkList@
          (setq WriteData@ (strcat
            Q$(nth Rep# BlkList@)Q$ ","     ;Block$
            Q$(nth Rep# LayerList@)Q$ ","   ;Layer$
            Q$(nth Rep# PointList@)Q$ ","   ;Point$
            Q$(nth Rep# ScaleList@)Q$ ","   ;Scale$
            Q$(nth Rep# ExplodeList@)Q$ "," ;Explode$
            Q$(nth Rep# AttribsList@)Q$)    ;Attribs$
          );setq
          (write-line WriteData@ FileName%)
          (setq Rep# (1+ Rep#))
        );foreach
        (close FileName%)
        (setq Option# 1)
      );progn
    );if
    (if (= Option# 2);cancel
      (setq Option# 1)
    );if
  );while
  (unload_dialog Dcl_Id%)
  (setq *error* Old_error)
  (princ)
);defun Edit_Lib
;-------------------------------------------------------------------------------
; c:InBlocks [Insert Blocks] - Insert blocks from selected folder.
; May be used with c:All_Blk_Lib to quickly add blocks to Libraries.
;-------------------------------------------------------------------------------
(defun c:InBlocks (/ BlkName$ Block$ FolderDwgs@ InsertAll$ Old_error
    PathFilename$ Replace$
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "ATTREQ" 0) (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  (princ "\nSelect a drawing in a folder to insert blocks for Library: ")(princ)
  (if (not *LastPath$)
    (setq *LastPath$ "")
  );if
  (if (setq PathFilename$ (getfiled " Select a drawing in a folder to insert blocks for Library" *LastPath$ "dwg" 2))
    (progn
      (setq *LastPath$ (car (GetPathFile PathFilename$)))
      (setq InsertAll$ (GetYesNo "Insert Blocks"
        (strcat "Do you want to insert all blocks"
        "\nfrom the " (substr *LastPath$ 1 (- (strlen *LastPath$) 1)) " folder?") "Quest")
      );setq
      (if (= InsertAll$ "Yes")
        (progn
          (princ "\nInserting blocks ")(princ)
          (if (>= (atoi (getvar "ACADVER")) 15)
            (command "LAYOUT" "S" "Model")
          );if
          (setq FolderDwgs@ (vl-directory-files *LastPath$ "*.dwg" 1))
          (foreach Block$ FolderDwgs@
            (setq BlkName$ (substr Block$ 1 (- (strlen Block$) 4)))
            (if (not (member (strcase Block$) (list (strcase (getvar "DWGNAME")) "BLK_LIB.DWG")))
              (progn
                (if (tblsearch "BLOCK" BlkName$)
                  (setq Replace$ (GetYesNoCancel "Insert Blocks"
                    (strcat "A Block named " BlkName$ " already exists"
                    "\nin the drawing.  Do you want to redefine it?") "Block")
                  );setq
                  (setq Replace$ "Yes")
                );if
                (if (= Replace$ "Yes")
                  (command ".INSERT" (strcat *LastPath$ BlkName$) "0,0" "" "" "")
                );if
                (if (= Replace$ "Cancel")
                  (progn
                    (command "ZOOM" "E")
                    (princ " ")
                    (exit)
                  );progn
                );if
              );progn
            );if
            (whirl)
          );foreach
          (command ".ZOOM" "E")
          (princ " ")
        );progn
      );if
    );progn
  );if
  (setq *error* Old_error)
  (princ)
);defun c:InBlocks
;-------------------------------------------------------------------------------
; SaveDwgName - Saves drawing before backing it up as C:\Autodesk\Civil_3D\BLK\_Config\Blk_Lib.dwg.
;-------------------------------------------------------------------------------
(defun SaveDwgName (/ DwgName$)
  (if (>= (atoi (getvar "ACADVER")) 15)
    (if (/= (getvar "CTAB") "Model")
      (command "PSPACE")
    );if
  );if
  (if (= (getvar "DWGTITLED") 0)
    (progn
      (setq DwgName$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.dwg")
      (if (findfile DwgName$)
        (command ".SAVEAS" "" DwgName$ "Y")
        (command ".SAVEAS" "" DwgName$)
      );if
    );progn
    (if (/= (getvar "DBMOD") 0)
      (command ".QSAVE")
    );if
  );if
  (setq DwgName$ (strcat (getvar "DWGPREFIX")(getvar "DWGNAME")))
  (if (/= DwgName$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.dwg")
    (if (findfile "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.dwg");Backup copy of drawing
      (command ".SAVE" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.dwg" "Y")
      (command ".SAVE" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.dwg")
    );if
  );if
  (princ)
);defun SaveDwgName
;-------------------------------------------------------------------------------
; c:Sel_Lib [Select Block Library] - Selects Block Library to use or adds or
; edits library definitions.
;-------------------------------------------------------------------------------
(defun c:Sel_Lib (/ BlockLib@ Cnt# DatFile$ EOF Field# FileName% Item$
    LibTitle$ Mid$ Old_error PathDat$ Q$ Rep# SlideLib@ StartNo# Text$ TitleLib@
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  (setq LibTitle$ "Select Block")
  (setq PathDat$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Sel_Lib.dat")
  (setq DatFile$ (cadr (GetPathFile PathDat$)))
  (princ (strcat "\n" LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDat$ nil)
  ;-----------------------------------------------------------------------------
  ; Get Lists from PathDat$ file.
  ;-----------------------------------------------------------------------------
  (if (findfile PathDat$)
    (setq FileName% (open PathDat$ "r"))
    (progn
      (GetOK (strcat LibTitle$ " Library")
        (strcat DatFile$ " file not found\nfor " LibTitle$ " Library.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (setq Q$ "\"") (setq Rep# 0)
  (setq EOF nil) (setq Item$ "")
  (while (null EOF)
    (setq Text$ (read-line FileName%))
    (if Text$
      (if (= (substr Text$ 1 1) Q$)
        (progn
          (setq StartNo# 2)
          (setq Cnt# 1)
          (setq Field# 1)
          (while (and (<= Cnt# (strlen Text$)) (< Field# 3))
            (setq Mid$ (substr Text$ Cnt# 3))
            (if (= Mid$ "\",\"")
              (progn
                (setq Item$
                  (substr Text$ StartNo# (- Cnt# StartNo#))
                );setq
                (cond
                  ((= Field# 1)
                    (if TitleLib@
                      (setq TitleLib@ (append TitleLib@ (list Item$)))
                      (setq TitleLib@ (list Item$))
                    );if
                  );Field# 1
                  ((= Field# 2)
                    (if SlideLib@
                      (setq SlideLib@ (append SlideLib@ (list Item$)))
                      (setq SlideLib@ (list Item$))
                    );if
                  );Field# 2
                  (t (exit))
                );cond
                (setq Field# (1+ Field#))
                (setq StartNo# (+ Cnt# 3))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (setq Item$
            (substr Text$ StartNo# (- (strlen Text$) StartNo#))
          );setq
          (if BlockLib@
            (setq BlockLib@ (append BlockLib@ (list Item$)))
            (setq BlockLib@ (list Item$))
          );if
        );progn
      );if
      (setq EOF t)
    );if
  );while
  (close FileName%)
  (Sel_Lib TitleLib@ SlideLib@ BlockLib@)
  (setq *error* Old_error)
  (princ)
);defun c:Sel_Lib
;-------------------------------------------------------------------------------
; Sel_Lib [Select Block Library] - Select Block Library to use or add or edit
; library definitions.
; Arguments: 3
;   TitleLib@ = List of Library titles
;   SlideLib@ = List of Pathnames and DefFiles for slides
;   BlockLib@ = List of Pathnames and DefFiles for blocks
; Returns: Selects a Block Library to use or adds or edits library definitions.
;-------------------------------------------------------------------------------
(defun Sel_Lib (TitleLib@ SlideLib@ BlockLib@ / Option# Dcl_Id% GetReturn@ Ins#
    intLib# LibrarySub: nthLibrary$ Pick# Rep# SlideMemb@ TitleMemb@ BlkSldScr
  );variables
  ;-----------------------------------------------------------------------------
  ; Sel_Lib subfunctions = Add_Edit_Defs:, Sel_Lib_dat:, LibrarySub:
  ;-----------------------------------------------------------------------------
  ; Add_Edit_Defs: [Add or Edit Library Definitions]
  ; Arguments: 6
  ;   Title$ = Title of Library
  ;   SldPath$ = Pathname and DefFile for slides
  ;   BlkPath$ = Pathname and DefFile for blocks
  ;   Dialog$ = Dialog to use, Add_Lib_Defs or Edit_Lib_Defs
  ;   TitleMemb@ = List of valid TitleLib@ items in all caps.
  ;   SlideMemb@ = List of valid SlideLib@ items in all caps.
  ; Returns: List of Title$, SldPath$ and BlkPath$ or nil.
  ;-----------------------------------------------------------------------------
  (defun Add_Edit_Defs: (Title$ SldPath$ BlkPath$ Dialog$ TitleMemb@ SlideMemb@
      / BlkFlag Option# Dcl_Id% DialogTitle$ NewBlkPath$ NewSldPath$ Num# OldTitle$
      OrgTitle$ Return@ SldFlag ShowPath$ TitleName: ClrTitle: BlkBrowse: FolderOptions:
    );variables
    ;---------------------------------------------------------------------------
    ; Add_Edit_Defs: subfunctions = TitleName:, ClrTitle:, BlkBrowse:, FolderOptions:
    ;---------------------------------------------------------------------------
    ; TitleName:
    ;---------------------------------------------------------------------------
    (defun TitleName: ()
      (setq Title$ (NoSpaces Title$))
      (if (< (strlen Title$) 2)
        (setq Title$ (strcat Title$))
        (setq Title$ (strcat
          (strcase (substr Title$ 1 1))
          (substr Title$ 2 (- (strlen Title$) 1)))
        );setq
      );if
      (if (or (= (strcase Title$) "LIBRARY") (= (strcase Title$) "LIBRARIES"))
        (setq Title$ "")
      );if
      (if (> (strlen Title$) 8)
        (if (= (strcase (substr Title$ (- (strlen Title$) 7) 8)) " LIBRARY")
          (setq Title$ (substr Title$ 1 (- (strlen Title$) 8)))
        );if
      );if
      (if (> (strlen Title$) 10)
        (if (= (strcase (substr Title$ (- (strlen Title$) 9) 10)) " LIBRARIES")
          (setq Title$ (substr Title$ 1 (- (strlen Title$) 10)))
        );if
      );if
      (if (= Title$ "")
        (progn
          (setq Title$ OldTitle$)
          (set_tile "title" Title$)
          (GetOK DialogTitle$ "Enter a Library Title. " "Exclam")
          (mode_tile "title" 2)
        );progn
        (progn
          (if (member Title$ TitleMemb@)
            (progn
              (set_tile "title" Title$)
              (GetOK DialogTitle$
                (strcat Title$ " Library has been defined."
                "\nEnter a different Library Title.") "Exclam"
              );GetOK
              (setq Title$ OrgTitle$)
              (set_tile "title" Title$)
              (setq OldTitle$ Title$)
            );progn
            (progn
              (set_tile "title" Title$)
              (setq OldTitle$ Title$)
            );progn
          );if
        );progn
      );if
      (setq BlkFlag nil SldFlag nil)
    );defun TitleName:
    ;---------------------------------------------------------------------------
    ; ClrTitle:
    ;---------------------------------------------------------------------------
    (defun ClrTitle: ()
      (set_tile "title" "")
      (setq Title$ "")
      (mode_tile "title" 2)
      (setq BlkFlag nil SldFlag nil)
    );defun ClrTitle:
    ;---------------------------------------------------------------------------
    ; BlkBrowse:
    ;---------------------------------------------------------------------------
    (defun BlkBrowse: ()
      (if (/= (strcase Title$) "NEW TITLE NAME")
        (progn
          (FolderOptions:)
          (if (or (= *Fld_Opts* "1")(= *Fld_Opts* "2"))
            (progn
              (if (= *Fld_Opts* "1")
                (GetOK DialogTitle$ (strcat
                  "If needed, create a new folder for the\n"
                  Title$ ".def file for the " Title$ " Library, then\n"
                  "select it as the new folder location.")
                  "Filefolder"
                );GetOK
              );if
              (if (not *LastPath$)
                (setq *LastPath$ "")
              );if
              (setq NewBlkPath$
                (getfiled " Block Folder Location" (strcat *LastPath$ Title$ ".def") "def" (atoi *Fld_Opts*))
              );setq
              (if NewBlkPath$
                (setq *LastPath$ (car (GetPathFile NewBlkPath$)))
              );if
              (if (and NewBlkPath$ (member (strcase NewBlkPath$) SlideMemb@))
                (progn
                  (setq Num# (- (length SlideMemb@)
                    (length (member (strcase NewBlkPath$) SlideMemb@)))
                  );setq
                  (GetOK DialogTitle$ (strcat
                    NewBlkPath$ " has been defined\n"
                    "by the " (nth Num# TitleMemb@) " Library.  Choose or create a new folder\n"
                    "for the " Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewBlkPath$ nil)
                );progn
              );if
              (if (and NewBlkPath$
                  (= (car (GetPathFile NewBlkPath$)) "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\")
                );and
                (progn
                  (GetOK DialogTitle$ (strcat
                    "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld folder is used by Drawing Blocks\n"
                    "and View Blocks.  Choose or create a new folder\n"
                    "for the " Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewBlkPath$ nil)
                );progn
              );if
              (if (and NewBlkPath$
                  (= (car (GetPathFile NewBlkPath$)) "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\")
                );and
                (progn
                  (GetOK DialogTitle$ (strcat
                    "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib folder is used and defined\n"
                    "by Block Library.  Choose or create a new folder\n"
                    "for the " Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewBlkPath$ nil)
                );progn
              );if
              (if (and NewSldPath$
                  (= (car (GetPathFile NewSldPath$)) "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\")
                );and
                (progn
                  (GetOK DialogTitle$ (strcat
                    "No new folder was created inside C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config.\n"
                    "Choose or create a new folder for the\n"
                    Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewSldPath$ nil)
                );progn
              );if
              (if NewBlkPath$
                (progn
                  (if BlkSldScr
                    (setq BlkSldScr nil
                          SldPath$ ""
                    );setq
                  );if
                  (setq *PreviousPath$ NewBlkPath$
                        BlkPath$ NewBlkPath$
                  );setq
                  (if (= SldPath$ "")
                    (progn
                      (setq NewSldPath$ NewBlkPath$
                            *PreviousPath$ NewBlkPath$
                            SldPath$ NewBlkPath$
                      );setq
                      (set_tile "sldpath" SldPath$)
                    );progn
                  );if
                  (setq NewSldPath$ NewBlkPath$
                        *PreviousPath$ NewBlkPath$
                        SldPath$ NewBlkPath$
                  );setq
                );progn
              );if
              (set_tile "blkpath" BlkPath$)
              (setq BlkFlag nil SldFlag nil)
            );progn
          );if
          (if (= *Fld_Opts* "3")
            (progn
              (if *PreviousPath$
                (setq NewBlkPath$ (getfiled (strcat
                  " Select a drawing in a folder for " Title$ " Library")
                  (car (GetPathFile *PreviousPath$)) "dwg" 2)
                );setq
                (progn
                  (if (not *LastPath$)
                    (setq *LastPath$ "")
                  );if
                  (setq NewBlkPath$ (getfiled (strcat
                    " Select a drawing in a folder for " Title$ " Library")
                    *LastPath$ "dwg" 2)
                  );setq
                );progn
              );if
              (if NewBlkPath$
                (setq NewBlkPath$ (strcat (car (GetPathFile NewBlkPath$)) Title$ ".def")
                      *LastPath$ (car (GetPathFile NewBlkPath$))
                );setq
              );if
              (if (and NewBlkPath$ (member (strcase NewBlkPath$) SlideMemb@))
                (progn
                  (setq Num# (- (length SlideMemb@)
                    (length (member (strcase NewBlkPath$) SlideMemb@)))
                  );setq
                  (GetOK DialogTitle$ (strcat
                    NewBlkPath$ " has been defined\n"
                    "by the " (nth Num# TitleMemb@) " Library.  Choose or create a new folder\n"
                    "for the " Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewBlkPath$ nil)
                );progn
              );if
              (if (and NewBlkPath$
                  (= (car (GetPathFile NewBlkPath$)) "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld\\")
                );and
                (progn
                  (GetOK DialogTitle$ (strcat
                    "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld folder is used by Drawing Blocks\n"
                    "and View Blocks.  Choose or create a new folder\n"
                    "for the " Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewBlkPath$ nil)
                );progn
              );if
              (if (and NewBlkPath$
                  (= (car (GetPathFile NewBlkPath$)) "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\")
                );and
                (progn
                  (GetOK DialogTitle$ (strcat
                    "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib folder is used and defined\n"
                    "by Block Library.  Choose or create a new folder\n"
                    "for the " Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewBlkPath$ nil)
                );progn
              );if
              (if (and NewSldPath$
                  (= (car (GetPathFile NewSldPath$)) "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\")
                );and
                (progn
                  (GetOK DialogTitle$ (strcat
                    "No new folder was created inside C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config.\n"
                    "Choose or create a new folder for the\n"
                    Title$ ".def file for the " Title$ " Library.") "Filefolder"
                  );GetOK
                  (setq NewSldPath$ nil)
                );progn
              );if
              (if NewBlkPath$
                (progn
                  (setq BlkSldScr t
                        *PreviousPath$ NewBlkPath$
                        BlkPath$ NewBlkPath$
                        NewSldPath$ NewBlkPath$
                        *PreviousPath$ NewBlkPath$
                        SldPath$ NewBlkPath$
                  );setq
                  (set_tile "sldpath" SldPath$)
                );progn
                (setq BlkSldScr nil)
              );if
              (set_tile "blkpath" BlkPath$)
              (setq BlkFlag nil SldFlag nil)
            );progn
          );if
        );progn
        (progn
          (GetOK DialogTitle$ "Enter a new Library Title name." "Exclam")
          (mode_tile "title" 2)
        );progn
      );if
    );defun BlkBrowse:
    ;---------------------------------------------------------------------------
    ; FolderOptions: - Options for Library Folders
    ;---------------------------------------------------------------------------
    (defun FolderOptions: (/ Dcl_Id% Info:)
      (defun Info: ()
        (GetOK "Block Library Message" (strcat
          "The option, to use a folder with drawings to create\n"
          "a new Library, is only available in a Single Document\n"
          "Interface.  If you need this option, close all other\n"
          "open drawings and try again.") ""
        );GetOK
        (mode_tile "accept" 2)
      );defun Info:
      (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
      (new_dialog "FolderOptions" Dcl_Id%)
      (if (not *Fld_Opts*) (setq *Fld_Opts* "1"))
      (if (and (> (length (GetDwgsList)) 1)(= *Fld_Opts* "3")) (setq *Fld_Opts* "1"))
      (if (> (length (GetDwgsList)) 1)
        (progn
          (mode_tile "3" 1)
          (mode_tile "msg3" 1)
          (mode_tile "msg4" 1)
        );progn
      );if
      (set_tile "title" (strcat " " DialogTitle$))
      (set_tile "radio" *Fld_Opts*)
      (set_tile "msg1" "Select or create a new Library Folder.")
      (set_tile "msg2" "Map to an existing Library folder.")
      (set_tile "msg3" "Use a folder with drawings to create a")
      (set_tile "msg4" "new Library and run a script to make slides.")
      (action_tile "radio" "(setq *Fld_Opts* $value)")
      (action_tile "info"  "(Info:)")
      (start_dialog)
      (unload_dialog Dcl_Id%)
      (princ)
    );defun FolderOptions:
    ;---------------------------------------------------------------------------
    ; Add_Edit_Defs: - Start Main Function
    ;---------------------------------------------------------------------------
    (if (= Dialog$ "Add_Lib_Defs")
      (progn
        (princ "\nAdd Library Definitions ")(princ)
        (setq DialogTitle$ "Add Library Definitions")
      );progn
      (progn
        (princ "\nEdit Library Definitions ")(princ)
        (setq DialogTitle$ "Edit Library Definitions")
      );progn
    );if
    ;---------------------------------------------------------------------------
    ; Load DCL dialog: Add_Lib_Defs or Edit_Lib_Defs in Blk_Lib.dcl
    ;---------------------------------------------------------------------------
    (setq Cloice# 99)
    (if (/= SldPath$ "")
      (setq NewSldPath$ SldPath$)
      (setq NewSldPath$ nil)
    );if
    (if (/= BlkPath$ "")
      (setq NewBlkPath$ BlkPath$)
      (setq NewBlkPath$ nil)
    );if
    (setq OldTitle$ Title$)
    (setq OrgTitle$ Title$)
    (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
    (princ "\nSelect an option: ")(princ)
    (while (/= Option# 1)
      (if (> (GetDclWidth SldPath$) 41.26)
        (progn
          (setq ShowPath$ (strcat (substr SldPath$ 4) (substr SldPath$ 1 3) "..."))
          (while (> (GetDclWidth ShowPath$) 41.26)
            (setq ShowPath$ (substr ShowPath$ 2))
          );while
          (setq ShowPath$ (strcat (substr ShowPath$ (- (strlen ShowPath$) 5)) (substr ShowPath$ 1 (- (strlen ShowPath$) 6))))
        );progn
        (setq ShowPath$ SldPath$)
      );if
      (new_dialog Dialog$ Dcl_Id%)
      (set_tile "title" Title$)
      (set_tile "titlelabel" "Title:")
      (set_tile "folderlabel" "Folder:")
      (set_tile "sldpath" ShowPath$)
      (mode_tile "sldpath" 1);gray out
      (action_tile "title"   "(setq Title$ $value)(TitleName:)")
      (action_tile "clear"   "(ClrTitle:)")
      (action_tile "browse"  "(BlkBrowse:)");browse
      (action_tile "accept"  "(done_dialog 4)");accept
      (action_tile "delete"  "(done_dialog 3)");delete
      (action_tile "cancel"  "(done_dialog 2)");cancel
      (setq Option# (start_dialog))
      (if (= Option# 4);accept
        (progn
          (setq Option# 1)
          (if (and (/= SldPath$ "") (/= BlkPath$ ""))
            (if (not BlkSldScr)
              (setq Return@ (list Title$ SldPath$ BlkPath$)
                    *Blk_Lib* Title$
              );setq
              (setq Return@ (list Title$ SldPath$ BlkPath$)
                    *Blk_Lib* Title$
              );setq
            );if
            (progn
              (GetOK DialogTitle$ "Folder location is required!" "Exclam")
              (Add_Edit_Defs: Title$ SldPath$ BlkPath$ Dialog$ TitleMemb@ SlideMemb@)
            );progn
          );if
        );progn
      );if
      (if (= Option# 3);delete
        (progn
          (setq Option# 1)
          (if (= (GetYesNo DialogTitle$
              (strcat "Are you sure you want to delete"
              "\nthe definitions for " OrgTitle$ " Library?") "Delete")
              "Yes")
            (setq Return@ (list "delete" "delete" "delete"))
            (setq Return@ nil)
          );if
        );progn
      );if
      (if (= Option# 2);cancel
        (setq Option# 1
              Return@ nil
        );setq
      );if
    );while
    (unload_dialog Dcl_Id%)
    Return@
  );defun Add_Edit_Defs:
  ;-----------------------------------------------------------------------------
  ; Sel_Lib_dat: - Updates changes to Sel_Lib.dat Library definitions.
  ; Arguments: 3
  ;   TitleLib@ = List of Library Titles
  ;   SlideLib@ = List of Pathname and DefFile for slides
  ;   BlockLib@ = List of Pathname and DefFile for blocks
  ; Returns: Updates new changes to Sel_Lib.dat Library definitions.
  ;-----------------------------------------------------------------------------
  (defun Sel_Lib_dat: (TitleLib@ SlideLib@ BlockLib@ / FileName% Text$ Q$ Rep#
      WriteData@
    );variables
    (setq Text$ (strcat ";" (String$ 79 "-")))
    (setq FileName% (open PathDat$ "w"))
    (write-line Text$ FileName%)
    (write-line (strcat "; " DatFile$) FileName%)
    (write-line Text$ FileName%)
    (setq Q$ "\"")(setq Rep# 0)
    (foreach Item TitleLib@
      (setq WriteData@ (strcat
        Q$(nth Rep# TitleLib@)Q$ "," ;LibTitle$
        Q$(nth Rep# SlideLib@)Q$ "," ;PathDef$
        Q$(nth Rep# BlockLib@)Q$)    ;PathBlk$
      );setq
      (write-line WriteData@ FileName%)
      (setq Rep# (1+ Rep#))
    );foreach
    (close FileName%)
  );defun Sel_Lib_dat:
  ;-----------------------------------------------------------------------------
  ; LibrarySub:
  ;-----------------------------------------------------------------------------
  (defun LibrarySub: ()
    (setq nthLibrary$ (get_tile "library")
          intLib# (atoi nthLibrary$)
          *Blk_Lib* (nth intLib# TitleLib@)
    );setq
    (set_tile "library" nthLibrary$)
  );defun LibrarySub:
  ;-----------------------------------------------------------------------------
  ; Sel_Lib - Start Main Function
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Sel_Lib in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (setq Cloice# 99)
  (if (and (= (type *Blk_Lib*) 'STR) (member *Blk_Lib* TitleLib@))
    (setq intLib# (- (length TitleLib@) (length (member *Blk_Lib* TitleLib@)))
          nthLibrary$ (itoa intLib#)
    );setq
    (setq *Blk_Lib* "Block"
          intLib# (- (length TitleLib@) (length (member *Blk_Lib* TitleLib@)))
          nthLibrary$ (itoa intLib#)
    );setq
  );if
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (princ "\nSelect an option: ")(princ)
  (while (/= Option# 1)
    (new_dialog "Sel_Lib" Dcl_Id%)
    (start_list "library")
    (mapcar 'add_list TitleLib@)
    (end_list)
    (set_tile "library" nthLibrary$)
    (action_tile "library" "(LibrarySub:)")
    (action_tile "add"     "(done_dialog 5)");add
    (action_tile "edit"    "(done_dialog 4)");edit
    (action_tile "select"  "(done_dialog 3)");select
    (action_tile "cancel"  "(done_dialog 2)");cancel
    (setq Option# (start_dialog))
    (if (= Option# 5);add
      (setq Pick# 5
            Option# 1
      );setq
    );if
    (if (= Option# 4);edit
      (setq Pick# 4
            Option# 1
      );setq
    );if
    (if (= Option# 3);select
      (setq Pick# 3
            Option# 1
      );setq
    );if
    (if (= Option# 2);cancel
      (setq Option# 1)
    );if
  );while
  (unload_dialog Dcl_Id%)
  (if (= Pick# 5);add
    (progn
      (setq TitleMemb@ TitleLib@)
      (setq Rep# 0 SlideMemb@ nil)
      (foreach Item SlideLib@
        (if SlideMemb@
          (setq SlideMemb@ (append SlideMemb@
            (list (strcase (nth Rep# SlideLib@))))
          );setq
          (setq SlideMemb@ (list (strcase (nth Rep# SlideLib@))))
        );if
        (setq Rep# (1+ Rep#))
      );foreach
      (setq GetReturn@ (Add_Edit_Defs:
        "New Title Name" "" "" "Add_Lib_Defs" TitleMemb@ SlideMemb@)
      );setq
      (if GetReturn@
        (progn
          (setq Rep# 0 Ins# nil)
          (foreach Item TitleLib@
            (if (> (strcase (nth 0 GetReturn@))
                (strcase (nth Rep# TitleLib@))
              );>
              (setq Ins# Rep#)
            );if
            (setq Rep# (1+ Rep#))
          );foreach
          (if Ins#
            (setq Ins# (1+ Ins#))
          );if
          (if (not Ins#)
            (setq Ins# 0)
          );if
          (if (= Ins# (length TitleLib@))
            (setq Ins# nil)
          );if
          (if Ins#
            (setq TitleLib@ (Insert_nth Ins# (nth 0 GetReturn@) TitleLib@)
                  SlideLib@ (Insert_nth Ins# (nth 1 GetReturn@) SlideLib@)
                  BlockLib@ (Insert_nth Ins# (nth 2 GetReturn@) BlockLib@)
            );setq
            (setq TitleLib@ (append TitleLib@ (list (nth 0 GetReturn@)))
                  SlideLib@ (append SlideLib@ (list (nth 1 GetReturn@)))
                  BlockLib@ (append BlockLib@ (list (nth 2 GetReturn@)))
            );setq
          );if
          (if BlkSldScr
            (if (= (GetYesNo "Block Library Message"
                (strcat "Block Library needs to run a script"
                "\nto create slides for " (nth 0 GetReturn@) " Library."
                "\nDo you want to continue?") "Quest") "Yes")
              (progn
                (SaveDwgName);Save Drawing*.dwg as a different name
                (Sel_Lib_dat: TitleLib@ SlideLib@ BlockLib@)
                (Slide_Script (nth 2 GetReturn@))
              );progn
              (progn
                (Sel_Lib_dat: TitleLib@ SlideLib@ BlockLib@)
                (c:Sel_Lib)
              );progn
            );if
            (progn
              (Sel_Lib_dat: TitleLib@ SlideLib@ BlockLib@)
              (c:Sel_Lib)
            );progn
          );if
        );progn
        (c:Sel_Lib)
      );if
    );progn
  );if
  (if (= Pick# 4);edit
    (if (= (strcase (nth intLib# TitleLib@)) "BLOCK")
      (progn
        (GetOK "Select Block Library"
          (strcat "Cannot edit the default definitions for Block Library."
          "\nAdd or select another Library to edit definitions.") "AlertX"
        );GetOK
        (c:Sel_Lib)
      );progn
      (progn
        (setq Rep# 0 TitleMemb@ nil)
        (foreach Item TitleLib@
          (if (/= Rep# intLib#)
            (if TitleMemb@
              (setq TitleMemb@ (append TitleMemb@ (list (nth Rep# TitleLib@))))
              (setq TitleMemb@ (list (nth Rep# TitleLib@)))
            );if
          );if
          (setq Rep# (1+ Rep#))
        );foreach
        (setq Rep# 0 SlideMemb@ nil)
        (foreach Item SlideLib@
          (if (/= Rep# intLib#)
            (if SlideMemb@
              (setq SlideMemb@ (append SlideMemb@
                (list (strcase (nth Rep# SlideLib@))))
              );setq
              (setq SlideMemb@ (list (strcase (nth Rep# SlideLib@))))
            );if
          );if
          (setq Rep# (1+ Rep#))
        );foreach
        (setq GetReturn@
          (Add_Edit_Defs: (nth intLib# TitleLib@) (nth intLib# SlideLib@)
          (nth intLib# BlockLib@) "Edit_Lib_Defs" TitleMemb@ SlideMemb@)
        );setq
        (if GetReturn@
          (progn
            (if (= (nth 1 GetReturn@) "delete")
              (setq TitleLib@ (Delete_nth intLib# TitleLib@)
                    SlideLib@ (Delete_nth intLib# SlideLib@)
                    BlockLib@ (Delete_nth intLib# BlockLib@)
              );setq
              (if (= (strcase (nth 0 GetReturn@))
                  (strcase (nth intLib# TitleLib@)))
                (setq
                  TitleLib@ (Change_nth intLib# (nth 0 GetReturn@) TitleLib@)
                  SlideLib@ (Change_nth intLib# (nth 1 GetReturn@) SlideLib@)
                  BlockLib@ (Change_nth intLib# (nth 2 GetReturn@) BlockLib@)
                );setq
                (progn
                  (setq TitleLib@ (Delete_nth intLib# TitleLib@)
                        SlideLib@ (Delete_nth intLib# SlideLib@)
                        BlockLib@ (Delete_nth intLib# BlockLib@)
                  );setq
                  (setq Rep# 0 Ins# nil)
                  (foreach Item TitleLib@
                    (if (> (strcase (nth 0 GetReturn@))
                        (strcase (nth Rep# TitleLib@))
                      );>
                      (setq Ins# Rep#)
                    );if
                    (setq Rep# (1+ Rep#))
                  );foreach
                  (if Ins#
                    (setq Ins# (1+ Ins#))
                  );if
                  (if (not Ins#)
                    (setq Ins# 0)
                  );if
                  (if (= Ins# (length TitleLib@))
                    (setq Ins# nil)
                  );if
                  (if Ins#
                    (setq
                      TitleLib@ (Insert_nth Ins# (nth 0 GetReturn@) TitleLib@)
                      SlideLib@ (Insert_nth Ins# (nth 1 GetReturn@) SlideLib@)
                      BlockLib@ (Insert_nth Ins# (nth 2 GetReturn@) BlockLib@)
                    );setq
                    (setq TitleLib@ (append TitleLib@ (list (nth 0 GetReturn@)))
                          SlideLib@ (append SlideLib@ (list (nth 1 GetReturn@)))
                          BlockLib@ (append BlockLib@ (list (nth 2 GetReturn@)))
                    );setq
                  );if
                );progn
              );if
            );if
            (Sel_Lib_dat: TitleLib@ SlideLib@ BlockLib@)
            (c:Sel_Lib)
          );progn
          (c:Sel_Lib)
        );if
      );progn
    );if
  );if
  (if (= Pick# 3);select
    (Blk_Lib
      (nth intLib# TitleLib@) (nth intLib# SlideLib@) (nth intLib# BlockLib@)
    );Blk_Lib
  );if
);defun Sel_Lib
;-------------------------------------------------------------------------------
; Slide_Script - Creates slides of drawings in folders
; Arguments: 1
;   PathFilename$ = Path and filename of folder to use.
; Returns: Runs a script to create slides in folder.
;-------------------------------------------------------------------------------
(defun Slide_Script (PathFilename$ / DefFile$ DwgList@ DwgName$ DwgNames@
    DwgPathName$ FileName% First FolderDwgs@ PathName$ Q$ SldPathName$ Text$
  );variables
  (setq PathName$ (nth 0 (GetPathFile PathFilename$))
        DefFile$ (nth 1 (GetPathFile PathFilename$))
        Q$ (chr 34)
  );setq
  (setq FolderDwgs@ (vl-directory-files PathName$ "*.dwg" 1))
  (foreach DwgName$ FolderDwgs@
    (setq DwgList@ (append DwgList@ (list (strcat PathName$ DwgName$)))
          Text$ (substr DwgName$ 1 (- (strlen DwgName$) 4))
          DwgNames@ (append DwgNames@ (list Text$))
    );setq
  );foreach
  (if (not (findfile PathFilename$))
    (progn
      (setq FileName% (open PathFilename$ "w"))
      (setq Text$ (strcat ";" (String$ 79 "-")))
      (setq FileName% (open PathFilename$ "w"))
      (write-line Text$ FileName%)
      (setq Text$ (strcat "; " DefFile$))
      (write-line Text$ FileName%)
      (setq Text$ (strcat ";" (String$ 79 "-")))
      (write-line Text$ FileName%)
      (close FileName%)
    );progn
  );if
  (setq FileName% (open PathFilename$ "a"))
  (foreach DwgName$ DwgNames@
    (setq Text$ (strcat Q$ DwgName$ Q$ "," Q$ "Current layer" Q$ "," Q$
      "User selects" Q$ "," Q$ "User selects" Q$ "," Q$ "0" Q$ "," Q$ "0" Q$)
    );setq
    (write-line Text$ FileName%)
  );foreach
  (close FileName%)
  (setq FileName% (open "C:\\Temp\\Temp.scr" "w"))
  (setq First t)
  (foreach DwgPathName$ DwgList@
    (setq SldPathName$ (strcat (substr DwgPathName$ 1 (- (strlen DwgPathName$) 4)) ".sld"))
    (write-line "FileOpen" FileName%)
    (if First
      (if (/= (getvar "DBMOD") 0)
        (write-line "N" FileName%)
      );if
      (write-line "Y" FileName%)
    );if
    (write-line (strcat Q$ DwgPathName$ Q$) FileName%)
    (write-line "DONUT 0 0.00000001 (getvar \"INSBASE\") " FileName%)
    (write-line "ZOOM E" FileName%)
    (write-line "DONUT 0 (/ (getvar \"VIEWSIZE\") 25.0) (getvar \"INSBASE\") " FileName%)
    (write-line "CHPROP L  C 6 " FileName%)
    (write-line "ZOOM 0.95X" FileName%)
    (write-line "REGEN " FileName%)
    (write-line "MSLIDE" FileName%)
    (write-line (strcat Q$ SldPathName$ Q$) FileName%)
    (setq First nil)
  );foreach
  (write-line "FileOpen" FileName%)
  (write-line "Y" FileName%)
  (write-line (strcat Q$ (getvar "DWGPREFIX") (getvar "DWGNAME") Q$) FileName%)
  (write-line "(princ \"\\n \\n \")(princ)" FileName%)
  (close FileName%)
  (command "SCRIPT" "C:\\Temp\\Temp.scr")
	(princ)
);defun Slide_Script
;-------------------------------------------------------------------------------
; c:User_Lib - Calls function User_Lib with default arguments.
;-------------------------------------------------------------------------------
(defun c:User_Lib () (User_Lib nil nil nil) (princ))
;-------------------------------------------------------------------------------
; User_Lib [User Library] - Inserts blocks from Library with user options.
; Arguments: 3
;   LibTitle$ = Library title
;   PathDef$ = Pathname and DefFile name for slides
;   PathBlk$ = Pathname and DefFile name for blocks
; Returns: Inserts a block from Library.
;-------------------------------------------------------------------------------
(defun User_Lib (LibTitle$ PathDef$ PathBlk$ / Attribs$ AttribsList@
    BlkList@ BlkPath$ Block$ BlockLen# Cnt# Dcl_Id% DefFile$ DefPath$ EOF
    Explode$ ExplodeList@ Field# FileName% ImageName$ Item$ Layer$ LayerList@
    Mid$ No_Blks# No_Pages# Old_error Option# Osmode PathBlock$ Pg_No# Pick
    Point@ PointList@ Q$ Ref# Rep# Scale~ ScaleList@ SlideRef$ StartNo# Text$
    UserPoint@ UserScale~
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "ATTREQ" 0) (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  ;-----------------------------------------------------------------------------
  ; Evaluate Arguments and get DefPath$ and DefFile$ variables.
  ;-----------------------------------------------------------------------------
  (if (null LibTitle$)
    (setq LibTitle$ "Block")
  );if
  (if (null PathDef$)
    (setq PathDef$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Blk_Lib.def")
  );if
  (setq DefPath$ (car (GetPathFile PathDef$)))
  (setq DefFile$ (cadr (GetPathFile PathDef$)))
  (if (null PathBlk$)
    (setq PathBlk$ PathDef$)
  );if
  (setq BlkPath$ (car (GetPathFile PathBlk$)))
  (princ (strcat "\n" LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDef$ PathBlk$)
  ;-----------------------------------------------------------------------------
  ; Get Lists from PathDef$ file.
  ;-----------------------------------------------------------------------------
  (if (findfile PathDef$)
    (setq FileName% (open PathDef$ "r"))
    (progn
      (GetOK "Block Library"
        (strcat DefFile$ " file not found\nfor " LibTitle$ " Library.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (setq Q$ "\"") (setq Rep# 0)
  (setq EOF nil) (setq Item$ "")
  (while (null EOF)
    (setq Rep# (1+ Rep#))
    (if (= Rep# 20)
      (progn
        (setq Rep# 0)
        (Whirl)
      );progn
    );if
    (setq Text$ (read-line FileName%))
    (if Text$
      (if (= (substr Text$ 1 1) Q$)
        (progn
          (setq StartNo# 2)
          (setq Cnt# 1)
          (setq Field# 1)
          (while (and (<= Cnt# (strlen Text$)) (< Field# 6))
            (setq Mid$ (substr Text$ Cnt# 3))
            (if (= Mid$ "\",\"")
              (progn
                (setq Item$
                  (substr Text$ StartNo# (- Cnt# StartNo#))
                );setq
                (cond
                  ((= Field# 1)
                    (if BlkList@
                      (setq BlkList@ (append BlkList@ (list Item$)))
                      (setq BlkList@ (list Item$))
                    );if
                  );Field# 1
                  ((= Field# 2)
                    (if LayerList@
                      (setq LayerList@ (append LayerList@ (list Item$)))
                      (setq LayerList@ (list Item$))
                    );if
                  );Field# 2
                  ((= Field# 3)
                    (if PointList@
                      (setq PointList@ (append PointList@ (list Item$)))
                      (setq PointList@ (list Item$))
                    );if
                  );Field# 3
                  ((= Field# 4)
                    (if ScaleList@
                      (setq ScaleList@ (append ScaleList@ (list Item$)))
                      (setq ScaleList@ (list Item$))
                    );if
                  );Field# 4
                  ((= Field# 5)
                    (if ExplodeList@
                      (setq ExplodeList@ (append ExplodeList@ (list Item$)))
                      (setq ExplodeList@ (list Item$))
                    );if
                  );Field# 5
                  (t (exit))
                );cond
                (setq Field# (1+ Field#))
                (setq StartNo# (+ Cnt# 3))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (setq Item$
            (substr Text$ StartNo# (- (strlen Text$) StartNo#))
          );setq
          (if AttribsList@
            (setq AttribsList@ (append AttribsList@ (list Item$)))
            (setq AttribsList@ (list Item$))
          );if
        );progn
      );if
      (setq EOF t)
    );if
  );while
  (princ " ")
  (close FileName%)
  (setq No_Pages# (fix (/ (1- (length BlkList@)) 20.0))
        Pg_No# 0
        BlockLen# (length BlkList@)
        Option# 99
  );setq
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Dwg_Blks in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (princ "\nSelect a block or option: ")(princ)
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (while (/= Option# 1)
    (new_dialog "Dwg_Blks" Dcl_Id%)
    (set_tile "title" (strcat " " LibTitle$ " Library "
      (itoa (+ Pg_No# 1)) " of " (itoa (+ No_Pages# 1)))
    );set_tile
    (if (= Pg_No# No_Pages#)
      (mode_tile "next" 1)
      (mode_tile "next" 0)
    );if
    (if (= Pg_No# 0)
      (mode_tile "previous" 1)
      (mode_tile "previous" 0)
    );if
    (if (> BlockLen# 19)
      (setq No_Blks# 20)
      (setq No_Blks# BlockLen#)
    );if
    (action_tile "next" "(done_dialog 4)")
    (action_tile "previous" "(done_dialog 3)")
    (action_tile "cancel" "(done_dialog 2)")
    (setq Rep# 1)
    (repeat (fix No_Blks#)
      (action_tile (strcat "sld" (itoa Rep#)) "(setq SlideRef$ $key Pick t)")
      (setq ImageName$ (nth (+ (* Pg_No# 20) (- Rep# 1)) BlkList@))
      (start_image (strcat "sld" (itoa Rep#)))
      (slide_image 0 0
        (dimx_tile (strcat "sld" (itoa Rep#)))
        (dimy_tile (strcat "sld" (itoa Rep#)))
        (strcat DefPath$ ImageName$ ".sld")
      );slide_image
      (end_image)
      (set_tile
        (strcat "sld" (itoa Rep#) "text") ImageName$
      );set_tile
      (setq Rep# (1+ Rep#))
    );repeat
    (setq Option# (start_dialog))
    (if (= Option# 4);next
      (setq Pg_No# (1+ Pg_No#)
            BlockLen# (- BlockLen# 20)
            SlideRef$ nil
      );setq
    );if
    (if (= Option# 3);previous
      (setq Pg_No# (- Pg_No# 1)
            BlockLen# (+ BlockLen# 20)
            SlideRef$ nil
      );setq
    );if
    (if (= Option# 2);cancel
      (setq Option# 1
            SlideRef$ nil
      );setq
    );if
  );while
  (unload_dialog Dcl_Id%)
  (if (and SlideRef$ Pick)
    (progn
      (setq Ref# (- (+ (* Pg_No# 20) (atoi (substr SlideRef$ 4))) 1))
      (setq Block$   (nth Ref# BlkList@)
            Layer$   (nth Ref# LayerList@)
            Point@   (nth Ref# PointList@)
            Scale~   (nth Ref# ScaleList@)
            Explode$ (nth Ref# ExplodeList@)
            Attribs$ (nth Ref# AttribsList@)
      );setq
      (setq PathBlock$ (strcat BlkPath$ Block$ ".dwg"))
      (if (or (= Block$ " ") (not (findfile PathBlock$)))
        (progn
          (if (/= Block$ " ")
            (GetOK (strcat LibTitle$ " Library")
              (strcat PathBlock$ "\nblock not found.") "AlertX"
            );GetOK
          );if
          (exit)
        );progn
      );if
      (if (= Point@ "Lower left limits")
        (setq Point@ (getvar "LIMMIN"))
      );if
      (if (= Point@ "Lower right limits")
        (setq Point@ (list (car (getvar "LIMMAX")) (cadr (getvar "LIMMIN"))))
      );if
      (if (= Point@ "Upper left limits")
        (setq Point@ (list (car (getvar "LIMMIN")) (cadr (getvar "LIMMAX"))))
      );if
      (if (= Point@ "Upper right limits")
        (setq Point@ (getvar "LIMMAX"))
      );if
      (if (and (= Point@ "User selects") (/= Scale~ "User selects"))
        (progn
          (while (null UserPoint@)
            (setq UserPoint@ (getpoint
              (strcat "\nInsertion point for " Block$ " block: "))
            );setq
          );while
          (setq Point@ UserPoint@)
        );progn
      );if
      (if (/= Point@ "User selects")
        (progn
          (if (= (type Point@) 'STR)
            (setq Point@ (RealList Point@))
          );if
          (if (/= (type Point@) 'LIST)
            (progn
              (GetOK (strcat LibTitle$ " Library")
                (strcat "Invalid insertion point for " Block$
                "\nblock in " DefFile$ " file.") "AlertX"
              );GetOK
              (exit)
            );progn
          );if
        );progn
      );if
      (if (= Scale~ "Dim scale")
        (if (= (getvar "DIMSCALE") 0)
          (progn
            (GetOK (strcat LibTitle$ " Library")
              "DIMSCALE value must be greater than 0." "AlertX"
            );GetOK
            (exit)
          );progn
          (setq Scale~ (getvar "DIMSCALE"))
        );if
      );if
      (if (and (= Scale~ "User selects") (/= Point@ "User selects"))
        (progn
          (while (or (null UserScale~) (< UserScale~ 0))
            (setq UserScale~
              (getreal
                (strcat "\nScale factor for " Block$ " block <1>: ")
              );getreal
            );setq
            (if (null UserScale~)
              (setq UserScale~ 1.0)
            );if
            (if (<= UserScale~ 0)
              (princ "\nScale factor must be greater than 0.")(princ)
            );if
          );while
          (setq Scale~ (rtosr UserScale~))
        );progn
      );if
      (if (/= Scale~ "User selects")
        (progn
          (if (= (type Scale~) 'STR)
            (setq Scale~ (atof Scale~))
          );if
          (if (<= Scale~ 0)
            (progn
              (GetOK (strcat LibTitle$ " Library")
                (strcat "Invalid scale factor for " Block$
                "\nblock in " DefFile$ " file.") "AlertX"
              );GetOK
              (exit)
            );progn
          );if
        );progn
      );if
      (setq Osmode (getvar "OSMODE"))
      (setvar "OSMODE" 0);None
      (if (= Explode$ "1");Yes
        (progn
          (setq PathBlock$ (strcat "*" BlkPath$ Block$))
          (if (and (= Point@ "User selects") (= Scale~ "User selects"))
            (command ".INSERT" PathBlock$)
            (command ".INSERT" PathBlock$ Point@ Scale~)
          );if
        );progn
        (progn
          (setq PathBlock$ (strcat BlkPath$ Block$))
          (if (/= Layer$ "Current layer")
            (if (tblsearch "LAYER" Layer$)
              (command ".LAYER" "T" Layer$ "U" Layer$ "ON" Layer$ "S" Layer$ "")
              (command ".LAYER" "M" Layer$ "")
            );if
          );if
          (if (and (= Point@ "User selects") (= Scale~ "User selects"))
            (progn
              (command ".INSERT" PathBlock$)
            );progn
            (command ".INSERT" PathBlock$ Point@ Scale~ "")
          );if
        );progn
      );if
      (setvar "OSMODE" Osmode)
    );progn
  );if
  (setq *error* Old_error)
  (princ)
);defun User_Lib
;-------------------------------------------------------------------------------
; c:Library - Select Block Library
;-------------------------------------------------------------------------------
(defun c:Library (/ BlockLib@ Cnt# DatFile$ EOF Field# FileName% Item$ LibTitle$
    Mid$ Old_error PathDat$ Q$ SlideLib@ StartNo# Text$ TitleLib@
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  (setq LibTitle$ "Select Block")
  (setq PathDat$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Sel_Lib.dat")
  (setq DatFile$ (cadr (GetPathFile PathDat$)))
  (princ (strcat "\n" LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDat$ nil)
  ;-----------------------------------------------------------------------------
  ; Get Lists from PathDat$ file.
  ;-----------------------------------------------------------------------------
  (if (findfile PathDat$)
    (setq FileName% (open PathDat$ "r"))
    (progn
      (GetOK (strcat LibTitle$ " Library")
        (strcat DatFile$ " file not found\nfor " LibTitle$ " Library.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (setq Q$ "\"") (setq EOF nil) (setq Item$ "")
  (while (null EOF)
    (setq Text$ (read-line FileName%))
    (if Text$
      (if (= (substr Text$ 1 1) Q$)
        (progn
          (setq StartNo# 2)
          (setq Cnt# 1)
          (setq Field# 1)
          (while (and (<= Cnt# (strlen Text$)) (< Field# 3))
            (setq Mid$ (substr Text$ Cnt# 3))
            (if (= Mid$ "\",\"")
              (progn
                (setq Item$
                  (substr Text$ StartNo# (- Cnt# StartNo#))
                );setq
                (cond
                  ((= Field# 1)
                    (if TitleLib@
                      (setq TitleLib@ (append TitleLib@ (list Item$)))
                      (setq TitleLib@ (list Item$))
                    );if
                  );Field# 1
                  ((= Field# 2)
                    (if SlideLib@
                      (setq SlideLib@ (append SlideLib@ (list Item$)))
                      (setq SlideLib@ (list Item$))
                    );if
                  );Field# 2
                  (t (exit))
                );cond
                (setq Field# (1+ Field#))
                (setq StartNo# (+ Cnt# 3))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (setq Item$
            (substr Text$ StartNo# (- (strlen Text$) StartNo#))
          );setq
          (if BlockLib@
            (setq BlockLib@ (append BlockLib@ (list Item$)))
            (setq BlockLib@ (list Item$))
          );if
        );progn
      );if
      (setq EOF t)
    );if
  );while
  (close FileName%)
  (Library TitleLib@ SlideLib@ BlockLib@)
  (setq *error* Old_error)
  (princ)
);defun c:Library
;-------------------------------------------------------------------------------
; Library - Select Block Library
; Arguments: 3
;   TitleLib@ = List of Library titles
;   SlideLib@ = List of Pathnames and DefFiles for slides
;   BlockLib@ = List of Pathnames and DefFiles for blocks
; Returns: Selects a Block Library to use or adds or edits library definitions.
;-------------------------------------------------------------------------------
(defun Library (TitleLib@ SlideLib@ BlockLib@ / Dcl_Id% intLib# LibrarySub: nthLibrary$
    Option# Pick#
  );variables
  ;-----------------------------------------------------------------------------
  ; Library subfunctions = LibrarySub:
  ;-----------------------------------------------------------------------------
  ; LibrarySub:
  ;-----------------------------------------------------------------------------
  (defun LibrarySub: ()
    (setq nthLibrary$ (get_tile "library")
          intLib# (atoi nthLibrary$)
          *Blk_Lib* (nth intLib# TitleLib@)
    );setq
    (set_tile "library" nthLibrary$)
  );defun LibrarySub:
  ;-----------------------------------------------------------------------------
  ; Library - Start Main Function
  ;-----------------------------------------------------------------------------
  (if (and (= (type *Blk_Lib*) 'STR) (member *Blk_Lib* TitleLib@))
    (setq intLib# (- (length TitleLib@) (length (member *Blk_Lib* TitleLib@)))
          nthLibrary$ (itoa intLib#)
    );setq
    (setq *Blk_Lib* "Block"
          intLib# (- (length TitleLib@) (length (member *Blk_Lib* TitleLib@)))
          nthLibrary$ (itoa intLib#)
    );setq
  );if
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (princ "\nSelect an option: ")(princ)
  (while (/= Option# 1)
    (new_dialog "Library" Dcl_Id%)
    (set_tile "title" " Block Library")
    (start_list "library")
    (mapcar 'add_list TitleLib@)
    (end_list)
    (set_tile "library" nthLibrary$)
    (action_tile "library" "(LibrarySub:)")
    (action_tile "select"  "(done_dialog 3)");select
    (action_tile "cancel"  "(done_dialog 2)");cancel
    (setq Option# (start_dialog))
    (if (= Option# 3);select
      (setq Pick# 3
            Option# 1
      );setq
    );if
    (if (= Option# 2);cancel
      (setq Option# 1)
    );if
  );while
  (unload_dialog Dcl_Id%)
  (if (= Pick# 3);select
    (User_Lib
      (nth intLib# TitleLib@) (nth intLib# SlideLib@) (nth intLib# BlockLib@)
    );User_Lib
  );if
);defun Library
;-------------------------------------------------------------------------------
; c:Add_Dwgs [Add Drawings to Library] - Adds a folder of drawings to a
; library without inserting them into a drawing.
;-------------------------------------------------------------------------------
(defun c:Add_Dwgs (/ Answer$ BlockLib@ Cnt# DatFile$ Dcl_Id% DefFile$ Dwgname$
    DwgPathName$ EOF Field# FileName% First FolderDwgs@ intLib# Item$ LibrarySub:
    LibTitle$ Loop Mid$ nthLibrary$ Old_error Option# PathDat$ PathDef$ Pathname$
    Q$ Rep# SlideLib@ StartNo# Text$ TitleLib$ TitleLib@
  );variables
  (setq Old_error *error* *error* *BL_error*)
  (setvar "CMDECHO" 0) (setvar "REGENMODE" 1)
  ;-----------------------------------------------------------------------------
  ; c:Add_Dwgs subfunctions = LibrarySub:
  ;-----------------------------------------------------------------------------
  ; LibrarySub:
  ;-----------------------------------------------------------------------------
  (defun LibrarySub: ()
    (setq nthLibrary$ (get_tile "library")
          intLib# (atoi nthLibrary$)
          *Blk_Lib* (nth intLib# TitleLib@)
    );setq
    (set_tile "library" nthLibrary$)
  );defun LibrarySub:
  ;-----------------------------------------------------------------------------
  ; c:Add_Dwgs - Start Main Function
  ;-----------------------------------------------------------------------------
  (if (> (length (GetDwgsList)) 1)
    (progn
      (GetOK "Add Drawings Message"
        (strcat "Add Drawings can only be run in a\n"
        "Single Document Interface. Close all\n"
        "other open drawings and try again.") ""
      );GetOK
      (exit)
    );progn
  );if
  (setq LibTitle$ "Add Drawings to")
  (setq PathDat$ "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib\\Sel_Lib.dat")
  (setq DatFile$ (cadr (GetPathFile PathDat$)))
  (princ (strcat "\n" LibTitle$ " Library "))(princ)
  (Custom_Dirs LibTitle$ PathDat$ nil)
  (setq Answer$ (GetYesNoCancel "Add Drawings to Library"
    (strcat "Do you need to add a new Library\n"
    "folder for Add Drawings to Library?") "Folder")
  );setq
  (if (= Answer$ "Yes")
    (progn
      (GetOK "Add Drawings to Library"
        (strcat "Add a new Library Definition and folder,\n"
          "and then re-run Add Drawings to Library."
        );strcat
        "filefolder"
      );GetOK
      (c:Sel_Lib)
      (princ "\nAdd Drawings to Library ")
      (exit)
    );progn
  );if
  (if (= Answer$ "Cancel")
    (exit)
  );if
  ;-----------------------------------------------------------------------------
  ; Get Lists from PathDat$ file.
  ;-----------------------------------------------------------------------------
  (if (findfile PathDat$)
    (setq FileName% (open PathDat$ "r"))
    (progn
      (GetOK (strcat LibTitle$ " Library")
        (strcat DatFile$ " file not found\n"
        "for " LibTitle$ " Library.") "AlertX"
      );GetOK
      (exit)
    );progn
  );if
  (setq Q$ "\"") (setq Rep# 0)
  (setq EOF nil) (setq Item$ "")
  (while (null EOF)
    (setq Text$ (read-line FileName%))
    (if Text$
      (if (= (substr Text$ 1 1) Q$)
        (progn
          (setq StartNo# 2)
          (setq Cnt# 1)
          (setq Field# 1)
          (while (and (<= Cnt# (strlen Text$)) (< Field# 3))
            (setq Mid$ (substr Text$ Cnt# 3))
            (if (= Mid$ "\",\"")
              (progn
                (setq Item$
                  (substr Text$ StartNo# (- Cnt# StartNo#))
                );setq
                (cond
                  ((= Field# 1)
                    (if TitleLib@
                      (setq TitleLib@ (append TitleLib@ (list Item$)))
                      (setq TitleLib@ (list Item$))
                    );if
                  );Field# 1
                  ((= Field# 2)
                    (if SlideLib@
                      (setq SlideLib@ (append SlideLib@ (list Item$)))
                      (setq SlideLib@ (list Item$))
                    );if
                  );Field# 2
                  (t (exit))
                );cond
                (setq Field# (1+ Field#))
                (setq StartNo# (+ Cnt# 3))
              );progn
            );if
            (setq Cnt# (1+ Cnt#))
          );while
          (setq Item$
            (substr Text$ StartNo# (- (strlen Text$) StartNo#))
          );setq
          (if BlockLib@
            (setq BlockLib@ (append BlockLib@ (list Item$)))
            (setq BlockLib@ (list Item$))
          );if
        );progn
      );if
      (setq EOF t)
    );if
  );while
  (close FileName%)
  ;-----------------------------------------------------------------------------
  ; Load DCL dialog: Library in Blk_Lib.dcl
  ;-----------------------------------------------------------------------------
  (if (and (= (type *Blk_Lib*) 'STR) (member *Blk_Lib* TitleLib@))
    (setq intLib# (- (length TitleLib@) (length (member *Blk_Lib* TitleLib@)))
          nthLibrary$ (itoa intLib#)
    );setq
    (setq *Blk_Lib* "Block"
          nthLibrary$ "0"
          intLib# 0
    );setq
  );if
  (princ "\nSelect an option: ")(princ)
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (setq Loop t)
  (while Loop
    (new_dialog "Library" Dcl_Id%)
    (set_tile "title" " Add Drawings to Library")
    (start_list "library")
    (mapcar 'add_list TitleLib@)
    (end_list)
    (set_tile "library" nthLibrary$)
    (action_tile "library" "(LibrarySub:)")
    (action_tile "select"  "(done_dialog 1)");select
    (action_tile "cancel"  "(done_dialog 0)");cancel
    (setq Option# (start_dialog))
    (if (= Option# 1)
      (progn
        (setq PathDef$ (nth intLib# BlockLib@))
        (setq TitleLib$ (nth intLib# TitleLib@))
        (setq Pathname$ (car (GetPathFile PathDef$)))
        (setq FolderDwgs@ (vl-directory-files Pathname$ "*.dwg" 1))
        (if FolderDwgs@
          (setq Loop nil)
          (GetOk "Add Drawings to Library"
            (strcat "The Library folder " Pathname$ "\n"
            "does not have any drawings to Add Drawings\n"
            "to Library! Select a Library folder with drawings.") "exclam"
          );GetOk
        );if
      );progn
    );if
    (if (= Option# 0)
      (setq Loop nil)
    );if
  );while
  (unload_dialog Dcl_Id%)
  (if (= Option# 0);cancel
    (exit)
  );if
  (setq Dwgname$ (strcat (getvar "DWGPREFIX")(getvar "DWGNAME")))
  (SaveDwgName);Save Drawing*.dwg as a different name
  (if (findfile Dwgname$)
    (command ".SAVE" Dwgname$ "Y")
  );if
  (setq FolderDwgs@ (Select_Dwgs FolderDwgs@ TitleLib$ Pathname$))
  (setq FileName% (open "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.scr" "w"))
  (write-line "FileOpen" FileName%)
  (write-line "Y" FileName%)
  (write-line (strcat Q$ (getvar "DWGPREFIX") (getvar "DWGNAME") Q$) FileName%)
  (close FileName%)
  (setq DefFile$ (FindReplace PathDef$ "\\" "\\\\"))
  (setq First t Q$ (chr 34))
  (setq FileName% (open "C:\\Temp\\Temp.scr" "w"))
  (foreach Dwgname$ FolderDwgs@
    (setq DwgPathName$ (strcat Pathname$ Dwgname$))
    (write-line "FileOpen" FileName%)
    (if (not First)
      (write-line "Y" FileName%)
    );if
    (write-line (strcat Q$ DwgPathName$ Q$) FileName%)
    (write-line (strcat "(Add_Dwgs " Q$ TitleLib$ Q$ " " Q$ DefFile$ Q$ ")") FileName%)
    (setq First nil)
  );foreach
  (write-line "FileOpen" FileName%)
  (write-line "Y" FileName%)
  (write-line (strcat Q$ (getvar "DWGPREFIX") (getvar "DWGNAME") Q$) FileName%)
  (write-line "(princ \"\\n \\n \\nAdd Drawings complete.\")(princ)" FileName%)
  (close FileName%)
  (setq *error* Old_error)
  (command "SCRIPT" "C:\\Temp\\Temp.scr")
  (princ)
);defun c:Add_Dwgs
;-------------------------------------------------------------------------------
; Add_Dwgs - Add_Dwgs dialog run by script file
;-------------------------------------------------------------------------------
(defun Add_Dwgs (TitleLib$ PathDef$ / Attribs$ BlkList$ Dcl_Id% DefPath$ Dot$
    DotSub: Dwgname$ Ent1^ Ent2^ Explode$ ExplodeSub: FileName% InsBase Layer$
    LayerSub: LayList@ nthLayList$ nthPntList$ nthScaList$ Option# PntList@ Point$
    PointSub: Q$ Scale$ ScaleSub: ScaList@ TblList@
  );variables
  ;-----------------------------------------------------------------------------
  ; Add_Dwgs subfunctions = LayerSub:, ScaleSub:, PointSub:, ExplodeSub:, DotSub:
  ;-----------------------------------------------------------------------------
  ; LayerSub:
  ;-----------------------------------------------------------------------------
  (defun LayerSub: ()
    (setq nthLayList$ (get_tile "layer"))
    (set_tile "layer" nthLayList$)
    (setq Layer$ (nth (atoi nthLayList$) LayList@))
  );defun LayerSub:
  ;-----------------------------------------------------------------------------
  ; ScaleSub:
  ;-----------------------------------------------------------------------------
  (defun ScaleSub: ()
    (setq nthScaList$ (get_tile "scale"))
    (set_tile "scale" nthScaList$)
    (setq Scale$ (nth (atoi nthScaList$) ScaList@))
  );defun ScaleSub:
  ;-----------------------------------------------------------------------------
  ; PointSub:
  ;-----------------------------------------------------------------------------
  (defun PointSub: ()
    (setq nthPntList$ (get_tile "point"))
    (set_tile "point" nthPntList$)
    (setq Point$ (nth (atoi nthPntList$) PntList@))
  );defun PointSub:
  ;-----------------------------------------------------------------------------
  ; ExplodeSub:
  ;-----------------------------------------------------------------------------
  (defun ExplodeSub: ()
    (if (= Explode$ "0")
      (setq Explode$ "1");yes
      (setq Explode$ "0");no
    );if
  );defun ExplodeSub:
  ;-----------------------------------------------------------------------------
  ; DotSub:
  ;-----------------------------------------------------------------------------
  (defun DotSub: ()
    (if (= Dot$ "0")
      (setq Dot$ "1");yes
      (setq Dot$ "0");no
    );if
  );defun DotSub:
  ;-----------------------------------------------------------------------------
  ; Add_Dwgs - Start Main Function
  ;-----------------------------------------------------------------------------
  (if (>= (atoi (getvar "ACADVER")) 15)
    (command "LAYOUT" "S" "Model")
  );if
  (command "ZOOM" "E")
  (setq LayList@ (list "Current layer"))
  (setq TblList@ (tblnext "LAYER" t))
  (setq LayList@ (append LayList@ (list (Capitals (cdr (assoc 2 TblList@))))))
  (while (setq TblList@ (tblnext "LAYER"))
    (setq LayList@ (append LayList@ (list (Capitals (cdr (assoc 2 TblList@))))))
  );while
  (setq PntList@ (list "User selects" "0,0" "Lower left limits"
    "Lower right limits" "Upper left limits" "Upper right limits")
  );setq
  (setq ScaList@ (list "User selects" "Dim scale" "Full scale"))
  (setq Layer$ (nth 0 LayList@)
        nthLayList$ "0"
        Scale$ (nth 0 ScaList@)
        nthScaList$ "0"
        Point$ (nth 0 PntList@)
        nthPntList$ "0"
        Explode$ "0"
        Dot$ "1"
        Attribs$ "0"
        Dwgname$ (getvar "DWGNAME")
        Dwgname$ (substr Dwgname$ 1 (- (strlen Dwgname$) 4))
  );setq
  (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
  (new_dialog "Add_Dwgs" Dcl_Id%)
  (set_tile "title" (strcat " Add " Dwgname$ " to " TitleLib$ " Library"))
  (start_list "layer")(mapcar 'add_list LayList@)(end_list)
  (set_tile "layer" nthLayList$)
  (start_list "scale")(mapcar 'add_list ScaList@)(end_list)
  (set_tile "scale" nthScaList$)
  (start_list "point")(mapcar 'add_list PntList@)(end_list)
  (set_tile "point" nthPntList$)
  (set_tile "explode" Explode$)
  (set_tile "dot" Dot$)
  (action_tile "layer" "(LayerSub:)")
  (action_tile "scale" "(ScaleSub:)")
  (action_tile "point" "(PointSub:)")
  (action_tile "explode" "(ExplodeSub:)")
  (action_tile "dot" "(DotSub:)")
  (action_tile "skipdwg" "(done_dialog 2)")
  (action_tile "addlib" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")
  (setq Option# (start_dialog))
  (unload_dialog Dcl_Id%)
  (if (= Option# 0)
    (command "SCRIPT" "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib.scr")
  );if
  (if (= Option# 1)
    (progn
      (if (= Scale$ "Full scale")
        (setq Scale$ "1.0")
      );if
      (setq Q$ (chr 34))
      (setq BlkList$ (strcat Q$ Dwgname$ Q$ ","Q$ Layer$ Q$ ","
        Q$ Point$ Q$ "," Q$ Scale$ Q$ "," Q$ Explode$ Q$ "," Q$ Attribs$ Q$)
      );setq
      (setq FileName% (open PathDef$ "a"))
      (write-line BlkList$ FileName%)
      (close FileName%)
      (command ".LAYER" "T" "0" "U" "0" "ON" "0" "S" "0" "")
      (command ".LAYER" "M" "0" "C" "7" "" "")
      (setvar "UCSICON" 0)
      (setq InsBase (getvar "INSBASE"))
      (if (= Dot$ "1")
        (progn
          (command ".DONUT" "0" "0.00000001" InsBase "")
          (setq Ent1^ (entlast))
        );progn
      );if
      (command ".ZOOM" "E")
      (if (= Dot$ "1")
        (progn
          (command ".DONUT" "0" (/ (getvar "VIEWSIZE") 25.0) InsBase "")
          (command ".CHPROP" "L" "" "C" "6" "");Color choice optional
          (setq Ent2^ (entlast))
        );progn
      );if
      (command ".ZOOM" "0.95x")
      (command ".REGEN")
      (setq DefPath$ (car (GetPathFile PathDef$)))
      (command ".MSLIDE" (strcat DefPath$ Dwgname$ ".sld"))
      (if (= Dot$ "1")
        (progn
          (entdel Ent1^)(entdel Ent2^)
        );progn
      );if
    );progn
  );if
  (princ)
);defun Add_Dwgs
;-------------------------------------------------------------------------------
; Select_Dwgs - Creates C:\Temp\Temp.dcl for Select_Dwgs dialog
;-------------------------------------------------------------------------------
(defun Select_Dwgs (FolderDwgs@ TitleLib$ Pathname$ / Dcl_Id% DclList@ FileName%
    Height$ NumberDwgs# Return# SelectAll$ SelectedDwgs@ Set_Vars: Text$ Verify_Info:
  );variables
  ;-----------------------------------------------------------------------------
  ; Select_Dwgs subfunctions = Set_Vars:, Verify_Info:
  ;-----------------------------------------------------------------------------
  ; Set_Vars: - Set dialog tiles and variables
  ;-----------------------------------------------------------------------------
  (defun Set_Vars: (ListName$ VarName$ / SaveVar$)
    (setq SaveVar$ (eval (read VarName$)))
    (if (= VarName$ "SelectAll$")
      (progn
        (setq SelectAll$ $value)
        (if (= SelectAll$ "1")
          (setq SelectedDwgs@ FolderDwgs@)
          (setq SelectedDwgs@ (list nil))
        );if
        (set_tile_list "FolderDwgs" FolderDwgs@ SelectedDwgs@)
      );progn
    );if
    (if (= ListName$ "FolderDwgs@")
      (progn
        (set_multilist_value "FolderDwgs@" "SelectedDwgs@")
        (setq SaveVar$ SelectAll$)
        (if (equal SelectedDwgs@ FolderDwgs@)
          (setq SelectAll$ "1")
          (setq SelectAll$ "0")
        );if
        (if (/= SelectAll$ SaveVar$)
          (set_tile "SelectAll" SelectAll$)
        );if
      );progn
    );if
    (set_tile_list "FolderDwgs" FolderDwgs@ SelectedDwgs@)
  );defun Set_Vars:
  ;-----------------------------------------------------------------------------
  ; Verify_Info: - Verifies dialog information
  ;-----------------------------------------------------------------------------
  (defun Verify_Info: ()
    (if (equal SelectedDwgs@ (list nil))
      (alert "Select one or more drawings to add.")
      (done_dialog 1)
    );if
  );defun Verify_Info:
  ;-----------------------------------------------------------------------------
  ; Select_Dwgs - Start Main Function
  ;-----------------------------------------------------------------------------
  (setq NumberDwgs# (length FolderDwgs@))
  (if (> NumberDwgs# 30)
    (setq NumberDwgs# 30)
  );if
  (setq Height$ (rtos (+ (* (fix (- (1+ NumberDwgs#)(* NumberDwgs# 0.18751)))(/ 16 13.0))0.12)2 2))
  (setq DclList@ (list
    "Select_Dwgs : dialog {"
    "  key = \"title\";"
    "  label = \"\";"
    "  spacer;"
    "  : boxed_column {"
    "    label = \"Multi-select Drawings to Add\";"
    "    : text {"
    "      key = \"FolderName\";"
    "      label = \"\";"
    "    }"
    "    : list_box {"
    "      multiple_select = true;"
    "      key = \"FolderDwgs\";"
    (strcat "      height = " Height$ ";") ;Height$ changes
    "      fixed_height = true;"
    "      width = 39.42;"
    "      fixed_width = true;"
    "    }"
    "    spacer_0;"
    "    : row {"
    "      alignment = centered;"
    "      fixed_width = true;"
    "      spacer;"
    "      : toggle {"
    "        key = \"SelectAll\";"
    "        label = \"Select all Drawings\";"
    "      }"
    "    }"
    "    spacer_0;"
    "  }"
    "  : row {"
    "    : column {"
    "      : ok_button {"
    "        alignment = right;"
    "        width = 11;"
    "      }"
    "    }"
    "    : column {"
    "      : cancel_button {"
    "        alignment = left;"
    "        width = 11;"
    "      }"
    "    }"
    "  }"
    "}"
  ));list;setq
  (setq FileName% (open "C:\\Temp\\Temp.dcl" "w"))
  (foreach Text$ DclList@
    (write-line Text$ FileName%)
  );foreach
  (close FileName%)
  (setq SelectedDwgs@ FolderDwgs@
        SelectAll$ "1"
  );setq
  (if (> (GetDclWidth Pathname$) 39.26)
    (progn
      (setq Pathname$ (strcat (substr Pathname$ 4) (substr Pathname$ 1 3) "..."))
      (while (> (GetDclWidth Pathname$) 39.26)
        (setq Pathname$ (substr Pathname$ 2))
      );while
      (setq Pathname$ (strcat (substr Pathname$ (- (strlen Pathname$) 5)) (substr Pathname$ 1 (- (strlen Pathname$) 6))))
    );progn
  );if
  (princ "\nCommand:\nMulti-select Drawings to Add:\n")(princ)
  (setq Dcl_Id% (load_dialog "C:\\Temp\\Temp.dcl"))
  (new_dialog "Select_Dwgs" Dcl_Id%)
  (set_tile "title" (strcat " Add Drawings to " TitleLib$ " Library"))
  (set_tile "FolderName" Pathname$)
  (set_tile_list "FolderDwgs" FolderDwgs@ SelectedDwgs@)
  (set_tile "SelectAll" SelectAll$)
  ;-----------------------------------------------------------------------------
  ; Dialog Actions
  ;-----------------------------------------------------------------------------
  (action_tile "FolderDwgs" "(Set_Vars: \"FolderDwgs@\" \"SelectedDwgs@\" )")
  (action_tile "SelectAll"  "(Set_Vars: \"\" \"SelectAll$\" )")
  (action_tile "accept"     "(Verify_Info:)")
  (setq Return# (start_dialog))
  (unload_dialog Dcl_Id%)
  (if (= Return# 0) (exit))
  SelectedDwgs@
);defun Select_Dwgs
;-------------------------------------------------------------------------------
; Start of Block Library Support Utility Functions
;-------------------------------------------------------------------------------
; Capitals - Capitalizes the first letter of each group of letters in a String.
; Arguments: 1
;   Str$ = String
; Returns: String with the first letter of each group of letters capitalized.
;-------------------------------------------------------------------------------
(defun Capitals (Str$ / Cnt# CapFlag Char$ Return$)
  (if (= (type Str$) 'STR)
    (progn
      (setq Str$ (strcase Str$ t))
      (setq Cnt# 1 Return$ "")
      (while (<= Cnt# (strlen Str$))
        (setq Char$ (substr Str$ Cnt# 1))
        (if (or (= Char$ ".") (and (>= Char$ "a") (<= Char$ "z")))
          (if CapFlag
            (setq Return$ (strcat Return$ Char$))
            (progn
              (setq Return$ (strcat Return$ (strcase Char$)))
              (setq CapFlag t)
            );progn
          );if
          (progn
            (setq Return$ (strcat Return$ Char$))
            (setq CapFlag nil)
          );progn
        );if
        (setq Cnt# (1+ Cnt#))
      );while
    );progn
  );if
  Return$
);defun Capitals
;-------------------------------------------------------------------------------
; Change_nth - Changes the nth item in a list with a new item value.
; Arguments: 3
;   Num# = Nth number in list to change
;   Value = New item value to change to
;   OldList@ = List to change item value
; Returns: A list with the nth item value changed.
;-------------------------------------------------------------------------------
(defun Change_nth (Num# Value OldList@)
  (if (<= 0 Num# (1- (length OldList@)))
    (if (> Num# 0)
      (cons (car OldList@) (Change_nth (1- Num#) Value (cdr OldList@)))
      (cons Value (cdr OldList@))
    );if
    OldList@
  );if
);defun Change_nth
;-------------------------------------------------------------------------------
; CommaString2List - Converts a comma delimited string into a list
; Arguments: 1
;   CommaString$ = Comma quoted delimited string
; Syntax: (CommaString2List CommaString$)
; Returns: List containing Items in a comma delimited string.
;-------------------------------------------------------------------------------
(defun CommaString2List (CommaString$ / Commas@ First# Index# Mid$ RemoveQuotes:
    Return@ Second# Text$
  );variables
  (defun RemoveQuotes: (Str$)
    (if (> (strlen Str$) 1)
      (if (and (= (substr Str$ 1 1) "\"")(= (substr Str$ (strlen Str$)) "\""))
        (setq Str$ (substr Str$ 2 (- (strlen Str$) 2)))
      );if
    );if
    Str$
  );defun RemoveQuotes:
  (if (wcmatch CommaString$ "*`,*")
    (progn
      (setq Index# 1 Commas@ (list 0))
      (repeat (strlen CommaString$)
        (setq Mid$ (substr CommaString$ Index# 1))
        (if (= Mid$ ",")
          (setq Commas@ (append Commas@ (list Index#)))
        );if
        (setq Index# (1+ Index#))
      );repeat
      (setq Commas@ (append Commas@ (list (1+ (strlen CommaString$)))))
      (setq Index# 0)
      (repeat (1- (length Commas@))
        (setq First# (nth Index# Commas@) Second# (nth (1+ Index#) Commas@))
        (setq Text$ (substr CommaString$ (1+ First#) (- Second# (1+ First#))))
        (setq Return@ (append Return@ (list (RemoveQuotes: Text$))))
        (setq Index# (1+ Index#))
      );repeat
    );progn
    (setq Return@ (list CommaString$))
  );if
  Return@
);defun CommaString2List
;-------------------------------------------------------------------------------
; Delete_nth - Deletes the nth item from a list.
; Arguments: 2
;   Num# = Nth number in list to delete
;   OldList@ = List to delete the nth item
; Returns: A list with the nth item deleted.
;-------------------------------------------------------------------------------
(defun Delete_nth (Num# OldList@)
  (setq Num# (1+ Num#))
  (vl-remove-if '(lambda (x) (zerop (setq Num# (1- Num#)))) OldList@)
);defun Delete_nth
;-------------------------------------------------------------------------------
; FindReplace - Returns Str$ with Find$ changed to Replace$
; Arguments: 3
;   Str$ = Text string
;   Find$ = Phrase string to find
;   Replace$ = Phrase to replace Find$ with
; Syntax: (FindReplace "TO SCALE" "TO" "NOT TO")
; Returns: Returns Str$ with Find$ changed to Replace$
;-------------------------------------------------------------------------------
(defun FindReplace (Str$ Find$ Replace$ / Len# Num# Start#)
  (setq Len# (strlen Replace$))
  (while (setq Num# (vl-string-search Find$ Str$ Start#))
    (setq Str$ (vl-string-subst Replace$ Find$ Str$ Num#)
          Start# (+ Num# Len#)
    );setq
  );while
  Str$
);defun FindReplace
;-------------------------------------------------------------------------------
; GetDate - Date String
; Returns: System date
;-------------------------------------------------------------------------------
(defun GetDate (/ ToDay$)
  (setq ToDay$ (rtos (getvar "cdate") 2 0))
  (strcat (substr ToDay$ 5 2)"/"(substr ToDay$ 7 2)"/"(substr ToDay$ 3 2))
);defun GetDate
;-------------------------------------------------------------------------------
; GetDwgsList - Returns a list of open drawings
; Use (length (GetDwgsList)) for the number of open drawings.
;-------------------------------------------------------------------------------
(defun GetDwgsList (/ AcadObj DocsObj DwgsList@)
  (if (>= (atoi (getvar "ACADVER")) 15)
    (progn
      (setq AcadObj (vlax-get-acad-object)
            DocsObj (vlax-get-property AcadObj "Documents")
      );setq
      (vlax-for ForItem DocsObj
        (setq DwgsList@ (cons (strcat (vlax-get-property ForItem "Path") "\\"
        (vlax-get-property ForItem "Name")) DwgsList@))
      );vlax-for
      (setq DwgsList@ (reverse DwgsList@))
    );progn
    (setq DwgsList@ (list (strcat (getvar "DWGPREFIX") (getvar "DWGNAME"))))
  );if
  DwgsList@
);defun GetDwgsList
;-------------------------------------------------------------------------------
; GetPathFile - Uses the format of the return of the findfile and getfiled
; functions and returns a list of the pathname and filename.
; Arguments: 1
;   DirFile$ = Path and filename string
; Returns: List separating the path and filename.
;-------------------------------------------------------------------------------
(defun GetPathFile (DirFile$ / Cnt# Mid# PathFile@)
  (setq Cnt# 1 Mid# 0)
  (while (< Cnt# (strlen DirFile$))
    (if (= (substr DirFile$ Cnt# 1) (chr 92))
      (setq Mid# Cnt#)
    );if
    (setq Cnt# (1+ Cnt#))
  );while
  (if (> Mid# 0)
    (setq PathFile@ (list
      (substr DirFile$ 1 Mid#)
      (substr DirFile$ (1+ Mid#) (strlen DirFile$)))
    );setq
    (setq PathFile@ DirFile$)
  );if
  PathFile@
);defun GetPathFile
;-------------------------------------------------------------------------------
; GetRnd - Generates a random number
; Arguments: 1
;   Num# = Maximum random number range
; Returns: Random integer number between 0 and Num#.
;-------------------------------------------------------------------------------
(defun GetRnd (Num# / MaxNum# PiDate$ RndNum# Minus Loop)
  (if (or (/= (type Num#) 'INT)(= Num# 0))
    (progn
      (princ "\nSyntax: (GetRnd Num#) Num# = Maximum random integer number range\ngreater than or less than 0.")
      (exit)
    );progn
  );if
  (if (< Num# 0)
    (setq MaxNum# (abs (1- Num#)) Minus t)
    (setq MaxNum# (1+ Num#))
  );if
  (if (not *RndNum*) (setq *RndNum* 10000))
  (setq Loop t)
  (while Loop
    (if (or (null *int*)(> *int* 100))
      (setq *int* 1)
      (setq *int* (1+ *int*))
    );if
    (setq PiDate$ (rtos (* (getvar "cdate") (* pi *int*)) 2 8))
    (cond
      ((>= MaxNum# 10000)
        (setq RndNum# (fix (* (atof (substr PiDate$ 13 5)) (* MaxNum# 0.00001))))
      )
      ((>= MaxNum# 1000)
        (setq RndNum# (fix (* (atof (substr PiDate$ 14 4)) (* MaxNum# 0.0001))))
      )
      ((>= MaxNum# 100)
        (setq RndNum# (fix (* (atof (substr PiDate$ 15 3)) (* MaxNum# 0.001))))
      )
      ((>= MaxNum# 10)
        (setq RndNum# (fix (* (atof (substr PiDate$ 16 2)) (* MaxNum# 0.01))))
      )
      ((>= MaxNum# 1)
        (setq RndNum# (fix (* (atof (substr PiDate$ 17 1)) (* MaxNum# 0.1))))
      )
      (t (setq RndNum# 0))
    );cond
    (if (/= RndNum# *RndNum*)
      (setq Loop nil)
    );if
  );while
  (setq *RndNum* RndNum#)
  (if Minus
    (setq RndNum# (* RndNum# -1))
  );if
  RndNum#
);defun GetRnd
;-------------------------------------------------------------------------------
; GetTime - Time String
; Returns: System time
;-------------------------------------------------------------------------------
(defun GetTime (/ Time$ Hour$ Min$ AM$)
  (setq Time$ (substr (rtos (getvar "cdate") 2 4) 10)
        Hour$ (substr Time$ 1 2)
        Min$ (substr Time$ 3 2)
        AM$ " AM"
  );setq
  (if (and (> (atoi Hour$) 11) (< (atoi Hour$) 24))
    (setq AM$ " PM")
  );if
  (if (< (atoi Hour$) 10)
    (setq Hour$ (substr Hour$ 2 1))
  );if
  (if (> (atoi Hour$) 12)
    (setq Hour$ (itoa (- (atoi Hour$) 12)))
  );if
  (strcat Hour$ ":" Min$ AM$)
);defun GetTime
;-------------------------------------------------------------------------------
; Insert_nth - Inserts a new item value into the nth number in list.
; Arguments: 3
;   Num# = Nth number in list to insert item value
;   Value = Item value to insert
;   OldList@ = List to insert item value
; Returns: A list with the new item value inserted.
;-------------------------------------------------------------------------------
(defun Insert_nth (Num# Value OldList@ / Temp@)
  (if (< -1 Num# (1+ (length OldList@)))
    (progn
      (repeat Num#
        (setq Temp@ (cons (car OldList@) Temp@)
              OldList@ (cdr OldList@)
        );setq
      );repeat
      (append (reverse Temp@) (list Value) OldList@)
    );progn
    OldList@
  );if
);defun Insert_nth
;-------------------------------------------------------------------------------
; Move_nth - Moves the nth Num1# item value to the nth Num2# location in a list.
; Arguments: 3
;   Num1# = Nth number in list to move item value
;   Num2# = Nth number in list to move item value of nth Num1# into
;   OldList@ = List to move item values
; Returns: A list with nth item value moved.
;-------------------------------------------------------------------------------
(defun Move_nth (Num1# Num2# OldList@ / Move_nth:)
  (defun Move_nth: (Num1# Num2# OldList@ Nth# Item)
    (cond
      ((and (> Nth# Num1#) (> Nth# Num2#))
        OldList@
      );case
      ((= Nth# Num1#)
        (Move_nth: Num1# (1+ Num2#) (cdr OldList@) (1+ Nth#) Item)
      );case
      ((= Nth# Num2#)
        (cons Item (Move_nth: (1+ Num1#) Num2# OldList@ (1+ Nth#) Item))
      );case
      ((cons (car OldList@)
        (Move_nth: Num1# Num2# (cdr OldList@) (1+ Nth#) Item))
      );case
    );cond
  );defun Move_nth:
  (if (and (/= Num1# Num2#) (<= 0 Num1# (1- (length OldList@))) (<= 0 Num2# (1- (length OldList@))))
    (Move_nth: Num1# Num2# OldList@ 0 (nth Num1# OldList@))
    OldList@
  );if
);defun Move_nth
;-------------------------------------------------------------------------------
; NoSpaces - Truncates left and right spaces from a string.
; Arguments: 1
;   Str$ = String
; Returns: String with the left and right spaces truncated.
;-------------------------------------------------------------------------------
(defun NoSpaces (Str$)
  (vl-string-trim " " Str$)
);defun NoSpaces
;-------------------------------------------------------------------------------
; RealList - Converts a string of two real numbers separated by a space into a
; realnumber points list.
; Arguments: 1
;   Point$ = String of two real numbers seperated by a space
; Returns: A realnumber points list of the two real numbers point string.
;-------------------------------------------------------------------------------
(defun RealList (Point$ / Cnt# Invalid Mid$ Num# X$ XY@ Xreal~ Y$ Yreal~)
  (setq Num# 1)(setq Cnt# 1)
  (while (<= Num# (strlen Point$))
    (setq Mid$ (substr Point$ Num# 1))
    (if (member Mid$ '
      ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "." "-" " "))
      (if (= (substr Point$ Num# 1) " ")
        (progn
          (setq X$ (substr Point$ 1 (- Num# 1)))
          (setq Y$ (substr Point$ (+ Num# 1) (strlen Point$)))
          (setq Xreal~ (atof X$))
          (setq Yreal~ (atof Y$))
          (setq XY@ (list Xreal~ Yreal~))
          (if (> Cnt# 1) (setq Invalid t))
          (setq Cnt# (1+ Cnt#))
        );progn
      );if
      (setq Invalid t)
    );if
    (setq Num# (1+ Num#))
  );while
  (if (null Invalid) XY@)
);defun RealList
;-------------------------------------------------------------------------------
; ReverseSS - Reverses the order of a selection set.
; Arguments: 1
;   OldSS& = Old selection set
; Returns: NewSS& selection set in the reverse order of the old selection set.
;-------------------------------------------------------------------------------
(defun ReverseSS (OldSS& / Cnt# Ename^ NewSS&)
  (setq NewSS& (ssadd))
  (setq Cnt# (1- (sslength OldSS&)))
  (repeat (sslength OldSS&)
    (setq Ename^ (ssname OldSS& Cnt#))
    (setq NewSS& (ssadd Ename^ NewSS&))
    (setq Cnt# (1- Cnt#))
  );repeat
  NewSS&
);defun ReverseSS
;-------------------------------------------------------------------------------
; rtosr - Used to change a real number into a short string real number
; stripping off all trailing 0's.
; Arguments: 1
;   RealNum~ = Real number to convert to a short string real number
; Returns: ShortReal$ the short string real number value of the real number.
;-------------------------------------------------------------------------------
(defun rtosr (RealNum~ / DimZin# ShortReal$)
  (setq DimZin# (getvar "DIMZIN"))
  (setvar "DIMZIN" 8)
  (setq ShortReal$ (rtos RealNum~ 2 8))
  (setvar "DIMZIN" DimZin#)
  ShortReal$
);defun rtosr
;-------------------------------------------------------------------------------
; runapp - Runs a DOS application with parameters
; Arguments: 2
;   Commands$ = String of DOS application with parameters
;   Visible = t for visible else nil
; Syntax example: (runapp "Notepad.exe \"C:\\Temp\\Temp.txt\"" t)
; Returns: Runs a DOS application and waits for it to finish.
;-------------------------------------------------------------------------------
(defun runapp (Commands$ Visible / WScript)
  (if (setq WScript (vlax-get-or-create-object "WScript.Shell"))
    (if Visible
      (vl-catch-all-apply 'vlax-invoke-method (list WScript "Run" Commands$ 1 :vlax-true))
      (vl-catch-all-apply 'vlax-invoke-method (list WScript "Run" Commands$ 6 :vlax-true))
    );if
  );if
  (princ)
);defun runapp
;-------------------------------------------------------------------------------
; set_multilist_value - Sets SentVar$ to list of the items selected in SentList$
; Arguments: 2
;   SentList$ = String of the list variable name
;   SentVar$ = String of the variable name
; Syntax: (set_multilist_value "ListName" "Variable")
;-------------------------------------------------------------------------------
(defun set_multilist_value (SentList$ SentVar$ / SubList@)
  (setq SubList@ (eval (read SentList$)))
  (set (read SentVar$) (list (nth (atoi $value) SubList@)))
  (setq $value (substr $value (+ (strlen (itoa (atoi $value))) 2)))
  (while (/= $value "")
    (set (read SentVar$) (append (eval (read SentVar$))
      (list (nth (atoi $value) SubList@)))
    );set
    (setq $value (substr $value (+ (strlen (itoa (atoi $value))) 2)))
  );while
);defun set_multilist_value
;-------------------------------------------------------------------------------
; set_tile_list - Sets a dialog popup_list or list_box tile to a list
; Arguments: 3
;   KeyName$ = Key name of tile
;   ListName@ = The list to set in tile
;   Selected = An item in the ListNames@ or a list of items selected
; Syntax: (set_tile_list "TileName" '("A" "B" "C") "B")
;         (set_tile_list "TileName" '("A" "B" "C") '("A" "C"))
; Returns: Sets Selected items in dialog popup_list or list_box tiles.
;-------------------------------------------------------------------------------
(defun set_tile_list (KeyName$ ListName@ Selected / Item)
  (start_list KeyName$ 3)
  (mapcar 'add_list ListName@)
  (end_list)
  (foreach Item (if (listp Selected) Selected (list Selected))
   (if (member Item ListName@)
     (set_tile KeyName$ (itoa (- (length ListName@) (length (member Item ListName@)))))
   );if
  );foreach
);defun set_tile_list
;-------------------------------------------------------------------------------
; String$ - Adds a number of characters or strings together.
; Arguments: 2
;   Num# = Number of characters or strings to add together
;   Char$ = Character or string to add
; Returns: New string with number of characters or strings.
;-------------------------------------------------------------------------------
(defun String$ (Num# Char$ / AddChars$)
  (setq AddChars$ "")
  (if (> Num# 0)
    (repeat Num# (setq AddChars$ (strcat Char$ AddChars$)))
  );if
  AddChars$
);defun String$
;-------------------------------------------------------------------------------
; Switch_nth - Switches the nth Num1# and Num2# item values in a list.
; Arguments: 3
;   Num1# = nth number in list to switch with nth Num2#
;   Num2# = nth number in list to switch with nth Num1#
;   OldList@ = List to switch item values
; Returns: A list with two item values switched.
;-------------------------------------------------------------------------------
(defun Switch_nth (Num1# Num2# OldList@ / Index#)
  (setq Index# -1)
  (if (and (< -1 Num1# (length OldList@)) (< -1 Num2# (length OldList@)))
    (mapcar '(lambda (x) (setq Index# (1+ Index#))
      (cond
        ((= Index# Num2#) (nth Num1# OldList@))
        ((= Index# Num1#) (nth Num2# OldList@))
        (x)
      )) OldList@
    );mapcar
    OldList@
  );if
);defun Switch_nth
;-------------------------------------------------------------------------------
; Whirl - Displays a whirl on the command line used during long delays while
; processing information.
; Returns: Global variable *Whirl# is used for displaying whirls.
;-------------------------------------------------------------------------------
(defun Whirl ()
  (if *Whirl#
    (setq *Whirl# (1+ *Whirl#))
    (setq *Whirl# 1)
  );if
  (if (>= *Whirl# 5)
    (setq *Whirl# 1)
  );if
  (cond
    ((= *Whirl# 1)(princ "-"))
    ((= *Whirl# 2)(princ "\\"))
    ((= *Whirl# 3)(princ "|"))
    ((= *Whirl# 4)(princ "/"))
  );cond
  (princ "\010")
);defun Whirl
;-------------------------------------------------------------------------------
; c:MAT - Shortcut for Match Slides Game
;-------------------------------------------------------------------------------
(defun c:MAT () (c:Match))
;-------------------------------------------------------------------------------
; c:Match - Match Slides Game
;-------------------------------------------------------------------------------
(defun c:Match (/ Dcl_Id% DoneList@ FileName% Finished First$ FolderSlds@ Free
    Loop1 Num# NumTop10# PathFilename$ Pick$ Return# Score# Scores@ Second$
    SldList@ SldPath$ Text$ TopScores# Chk_Match: ShowIt: BlankIt: DelayIt:
    Free: GetSldFolder: ShuffleSlides: MatchScores: Add2Top10: ShowScores:
    NewGame: ExitYesNo:
  );variables
  ;-----------------------------------------------------------------------------
  ; c:Match subfunctions = Chk_Match:, ShowIt:, BlankIt:, DelayIt:, Free:,
  ; GetSldFolder:, ShuffleSlides:, MatchScores:, Add2Top10:, ShowScores:,
  ; NewGame:, ExitYesNo:
  ;-----------------------------------------------------------------------------
  ; Chk_Match: - Check for Matches
  ; Arguments: 1
  ;   $key = Key name of dialog tile
  ; Returns: Determines if the First and Second pick are a match.
  ;-----------------------------------------------------------------------------
  (defun Chk_Match: ($key / NumKey$ Image$ Loop2 Cnt# Num# Answer$ Name$)
    (setq NumKey$ (substr $key (- (strlen $key) 2))) ; Last 3 digits
    (if (= Pick$ "First")
      (setq First$ NumKey$)
    );if
    (if (= Pick$ "Second")
      (setq Second$ NumKey$)
    );if
    (cond
      ( (and (= Pick$ "First") (not (member First$ DoneList@))
             (= (nth (atoi First$) SldList@) "*FREE*"))
        (Free: $key)
        (setq DoneList@ (append DoneList@ (list First$)))
        (setq Free t)
        (GetOK "Match Slides" "Your next pick is FREE!" "smile")
        (setq Pick$ "Second")
      );case
      ( (and (= Pick$ "First") (not (member First$ DoneList@)))
        (setq Score# (+ Score# 10))
        (set_tile "ScoreIs" (strcat "Score: " (itoa Score#)))
        (ShowIt: $key (strcat *SldPath$ (nth (atoi First$) SldList@)))
        (setq Free nil)
        (setq Pick$ "Second")
      );case
      ( (and (= Pick$ "Second") (not (member Second$ DoneList@))
             (/= Second$ First$) Free)
        (ShowIt: $key (strcat *SldPath$ (nth (atoi Second$) SldList@)))
        (setq DoneList@ (append DoneList@ (list Second$)))
        (setq Num# 1)
        (repeat 25
          (if (= (nth Num# SldList@) (nth (atoi Second$) SldList@))
            (if (/= Num# (atoi Second$))
              (setq Cnt# Num#)
            );if
          );if
          (setq Num# (1+ Num#))
        );repeat
        (if (< Cnt# 10)
          (setq NumKey$ (strcat "00" (itoa Cnt#)))
          (setq NumKey$ (strcat "0"  (itoa Cnt#)))
        );if
        (setq Image$ (strcat "Image" NumKey$))
        (ShowIt: Image$ (strcat *SldPath$ (nth Cnt# SldList@)))
        (setq DoneList@ (append DoneList@ (list NumKey$)))
        (setq Pick$ "First")
      );case
      ( (and (= Pick$ "Second") (not (member Second$ DoneList@))
             (/= Second$ First$) (= (nth (atoi Second$) SldList@) "*FREE*"))
        (setq Score# (- Score# 10))
        (set_tile "ScoreIs" (strcat "Score: " (itoa Score#)))
        (Free: $key)
        (setq DoneList@ (append DoneList@ (list First$)))
        (setq DoneList@ (append DoneList@ (list Second$)))
        (GetOK "Match Slides" "Your last pick was FREE!" "smile")
        (setq Num# 1)
        (repeat 25
          (if (= (nth Num# SldList@) (nth (atoi First$) SldList@))
            (if (/= Num# (atoi First$))
              (setq Cnt# Num#)
            );if
          );if
          (setq Num# (1+ Num#))
        );repeat
        (if (< Cnt# 10)
          (setq NumKey$ (strcat "00" (itoa Cnt#)))
          (setq NumKey$ (strcat "0"  (itoa Cnt#)))
        );if
        (setq Image$ (strcat "Image" NumKey$))
        (ShowIt: Image$ (strcat *SldPath$ (nth Cnt# SldList@)))
        (setq DoneList@ (append DoneList@ (list NumKey$)))
        (setq Pick$ "First")
      );case
      ( (and (= Pick$ "Second") (not (member Second$ DoneList@))
             (= (nth (atoi Second$) SldList@) (nth (atoi First$) SldList@))
             (/= Second$ First$))
        (setq Score# (- Score# 10))
        (set_tile "ScoreIs" (strcat "Score: " (itoa Score#)))
        (ShowIt: $key (strcat *SldPath$ (nth (atoi Second$) SldList@)))
        (setq DoneList@ (append DoneList@ (list First$)))
        (setq DoneList@ (append DoneList@ (list Second$)))
        (setq Pick$ "First")
      );case
      ( (and (= Pick$ "Second") (not (member Second$ DoneList@))
             (/= Second$ First$))
        (setq Score# (+ Score# 10))
        (set_tile "ScoreIs" (strcat "Score: " (itoa Score#)))
        (ShowIt: $key (strcat *SldPath$ (nth (atoi Second$) SldList@)))
        (repeat 2000
          (DelayIt: $key)
        );repeat
        (BlankIt: (strcat "Image" First$))
        (BlankIt: (strcat "Image" Second$))
        (setq Pick$ "First")
      );case
    );cond
    (if (and (>= (length DoneList@) 25) (/= Pick$ "No"))
      (progn
        (set_tile "PickIs" "")
        (if (or (< Score# TopScores#) (< NumTop10# 10))
          (progn
            (GetOK "Match Slides" "Game over!  You\nare in the Top 10." "")
            (setq Loop2 t)
            (while Loop2
              (setq Name$ (EditBox "Match Slides" "Enter Name:"
                (getvar "loginname") 10))
              (if Name$
                (progn
                  (Add2Top10:)
                  (MatchScores: t)
                  (ShowScores:)
                  (setq Loop2 nil)
                );progn
                (progn
                  (setq Answer$ (GetYesNo "Match Slides"
                    "Do you want to\nbe in the Top 10?" "")
                  );setq
                  (if (= Answer$ "No")
                    (setq Loop2 nil)
                  );if
                );progn
              );if
            );while
          );progn
          (GetOK "Match Slides"
            (strcat "Game over!\nYour score was " (itoa Score#) ".") "")
        );if
        (setq Pick$ "No" Finished t)
      );progn
      (if (/= Pick$ "No")
        (set_tile "PickIs" (strcat Pick$ " Pick"))
      );if
    );if
    (princ)
  );defun Chk_Match:
  ;-----------------------------------------------------------------------------
  ; ShowIt: - Shows slide in image tile
  ; Arguments: 2
  ;   ImageKey$ = Key name of dialog image tile
  ;   SldPathFilename$ = Path and filename location of slide
  ; Returns: Displays a slide in image tile
  ;-----------------------------------------------------------------------------
  (defun ShowIt: (ImageKey$ SldPathFilename$)
    (start_image ImageKey$)
    (fill_image 0 0 (dimx_tile ImageKey$) (dimy_tile ImageKey$) 0)
    (slide_image 0 0 (dimx_tile ImageKey$) (dimy_tile ImageKey$) SldPathFilename$)
    (end_image)
  );defun ShowIt:
  ;-----------------------------------------------------------------------------
  ; BlankIt: - Blanks out image tile
  ; Arguments: 1
  ;   ImageKey$ = Key name of dialog image tile
  ; Returns: Fills image tile with dialog foreground color
  ;-----------------------------------------------------------------------------
  (defun BlankIt: (ImageKey$)
    (start_image ImageKey$)
    (fill_image 0 0 (dimx_tile ImageKey$) (dimy_tile ImageKey$) -15)
    (end_image)
  );defun BlankIt:
  ;-----------------------------------------------------------------------------
  ; DelayIt: - Delay for image tile
  ; Arguments: 1
  ;   ImageKey$ = Key name of dialog image tile
  ; Returns: Draws an outline outside visible image tile used as a delay
  ;-----------------------------------------------------------------------------
  (defun DelayIt: (ImageKey$ / X Y)
    (start_image ImageKey$)
    (setq X (dimx_tile ImageKey$)
          Y (dimy_tile ImageKey$)
    );setq
    (vector_image 0 0 0 Y -15)
    (vector_image 0 Y X Y -15)
    (vector_image X Y X 0 -15)
    (vector_image X 0 0 0 -15)
    (end_image)
  );defun DelayIt:
  ;-----------------------------------------------------------------------------
  ; Free: - Displays Free image in image tile
  ; Arguments: 1
  ;   ImageKey$ = Key name of dialog image tile
  ; Returns: Displays the Free image in image tile
  ;-----------------------------------------------------------------------------
  (defun Free: (ImageNum$);dotted rectangle = (2,-2)-(125,-77)
    (start_image ImageNum$)
    (fill_image 0 0 (dimx_tile ImageNum$) (dimy_tile ImageNum$) 0)
    (vector_line (list "C" 1 51 33 42 33 37 47 "C" 1 47 39 40 39 "C" 1 50 47 55
      33 62 33 63 34 62 38 61 39 53 39 "C" 1 60 47 57 39 "C" 1 78 33 69 33 65 47
      74 47 "C" 1 68 39 74 39 "C" 1 90 33 82 33 78 47 87 47 "C" 1 81 39 87 39
      "C" 2 63 2 66 20 74 9 74 19 84 10 82 19 101 7 89 23 106 18 94 29 125 24 99
      35 121 37 97 41 114 48 96 49 117 66 88 54 92 66 77 58 80 77 68 60 64 72 59
      58 51 74 49 59 36 72 42 55 29 64 34 52 17 59 30 46 10 48 29 40 2 35 31 33
      18 25 35 27 22 15 42 24 38 11 50 21 52 8 59 19 63 2)
    );vector_line
    (end_image)
  );defun Free:
  ;-----------------------------------------------------------------------------
  ; GetSldFolder: - Gets folder with slides
  ;-----------------------------------------------------------------------------
  (defun GetSldFolder: (/ NewSldPath$ Num#)
    (if (= (GetOkCancel "Match Slides"
        "Select a folder with at least\n12 slides for Match Slides." "Inform")
        "Cancel")
      (exit)
    );progn
    (if *SldPath$
      (setq NewSldPath$ (getfiled
        " Select a slide in a folder for Match Slides"
        *SldPath$ "sld" 2)
      );setq
      (setq NewSldPath$ (getfiled
        " Select a slide in a folder for Match Slides"
        "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\" "sld" 2)
      );setq
    );if
    (if (not (findfile "C:\\Temp\\Temp.txt"))
      (vl-mkdir "C:\\Temp")
    );if
    (if NewSldPath$
      (progn
        (setq *SldPath$ (car (GetPathFile NewSldPath$)))
        (setq FolderSlds@ (vl-directory-files *SldPath$ "*.sld" 1))
        (if (< (length FolderSlds@) 12)
          (if (= (GetOkCancel "Match Slides"
              "Select a folder with at least\n12 slides for Match Slides."
              "Inform")
              "OK")
            (GetSldFolder:)
            (exit)
          );if
          (progn
            (setq Num# (1- (length FolderSlds@)))
            (repeat (1+ (fix (/ Num# 2.0)))
              (setq FolderSlds@ (Switch_nth (getrnd Num#) (getrnd Num#) FolderSlds@)
                    FolderSlds@ (Move_nth (getrnd Num#) (getrnd Num#) FolderSlds@)
                    FolderSlds@ (reverse FolderSlds@)
              );setq
            );repeat
            (setq SldList@ nil)
            (setq Num# 0)
            (repeat 12
              (setq SldList@ (append SldList@ (list (nth Num# FolderSlds@))))
              (setq Num# (1+ Num#))
            );repeat
            (setq SldList@ (append SldList@ SldList@))
            (setq SldList@ (Insert_nth (getrnd 23) "*FREE*" SldList@))
            (repeat 12
              (setq SldList@ (Switch_nth (getrnd 24) (getrnd 24) SldList@)
                    SldList@ (Move_nth (getrnd 24) (getrnd 24) SldList@)
                    SldList@ (reverse SldList@)
              );setq
            );repeat
            (setq SldList@ (Insert_nth 0 "*FREE*" SldList@))
          );progn
        );if
      );progn
      (progn
        (if (= (GetYesNo "Match Slides"
          "No folder was selected for Match Slides.\nDo you want to exit?" "")
          "Yes")
          (exit)
          (GetSldFolder:)
        );if
      );progn
    );if
  );defun GetSldFolder:
  ;-----------------------------------------------------------------------------
  ; ShuffleSlides: - Shuffle Slides on dialog
  ;-----------------------------------------------------------------------------
  (defun ShuffleSlides: (/ ImageNums@ Num# Image_A$ Image_B$ Image_C$ RndSlds@)
    (setq Num# 1)
    (repeat 25
      (if (< Num# 10)
        (setq ImageNums@ (append ImageNums@
          (list (strcat "Image00" (itoa Num#)))))
        (setq ImageNums@ (append ImageNums@
          (list (strcat "Image0"  (itoa Num#)))))
      );if
      (setq Num# (1+ Num#))
    );repeat
    (repeat 12
      (setq ImageNums@ (Switch_nth (getrnd 24) (getrnd 24) ImageNums@)
            ImageNums@ (Move_nth (getrnd 24) (getrnd 24) ImageNums@)
            ImageNums@ (reverse ImageNums@)
      );setq
    );repeat
    (setq ImageNums@ (append ImageNums@ (reverse ImageNums@)))
    (setq RndSlds@ (reverse FolderSlds@))
    (repeat 5
      (if (< (length RndSlds@) 50)
        (setq RndSlds@ (append RndSlds@ (reverse FolderSlds@)))
      );if
    );repeat
    (repeat 2
      (setq Num# 0)
      (repeat 47
        (setq Image_A$ (nth Num# ImageNums@))
        (ShowIt: Image_A$ (strcat *SldPath$ (nth Num# RndSlds@)))
        (setq Image_B$ (nth (1+ Num#) ImageNums@))
        (ShowIt: Image_B$ (strcat *SldPath$ (nth (1+ Num#) RndSlds@)))
        (setq Image_C$ (nth (+ Num# 2) ImageNums@))
        (ShowIt: Image_C$ (strcat *SldPath$ (nth (+ Num# 2) RndSlds@)))
        (BlankIt: Image_A$)
        (setq Num# (1+ Num#))
      );repeat
      (BlankIt: Image_B$)
      (BlankIt: Image_C$)
    );repeat
  );defun ShuffleSlides:
  ;-----------------------------------------------------------------------------
  ; MatchScores: - Reads or writes the Score data and stores it in Scores@
  ; Arguments: 1
  ;   Write = t if writing to file, or nil if reading from file
  ; Returns: Creates Scores@ variable and reads or writes C:\Temp\Match.dat
  ;-----------------------------------------------------------------------------
  (defun MatchScores: (Write / FileName% Q QcQ Cnt# Text$)
    (setq Q "\"" QcQ "\",\"" Cnt# 0)
    (if Write
      (progn
        (setq FileName% (open "C:\\Temp\\Match.dat" "w"))
        (repeat 10
          (setq Text$ (strcat Q
            (nth 0 (nth Cnt# Scores@)) QcQ
            (nth 1 (nth Cnt# Scores@)) QcQ
            (nth 2 (nth Cnt# Scores@)) QcQ
            (nth 3 (nth Cnt# Scores@)) Q)
          );setq
          (write-line Text$ FileName%)
          (setq Cnt# (1+ Cnt#))
        );repeat
        (close FileName%)
      );progn
      (progn
        (setq Scores@ nil)
        (if (findfile "C:\\Temp\\Match.dat")
          (progn
            (setq FileName% (open "C:\\Temp\\Match.dat" "r"))
            (while (setq Text$ (read-line FileName%))
              (setq Scores@ (append Scores@ (list (CommaString2List Text$))))
            );while
            (close FileName%)
          );progn
        );if
        (repeat (- 10 (length Scores@))
          (setq Scores@ (append Scores@ (list (list "" "" "" ""))))
        );repeat
        (repeat (- (length Scores@) 10)
          (setq Scores@ (Delete_nth (1- (length Scores@)) Scores@))
        );repeat
      );progn
    );if
    (setq Cnt# 0 TopScores# 99999999 NumTop10# 0)
    (repeat 10
      (if (> (atoi (nth 3 (nth Cnt# Scores@))) 0)
        (setq TopScores# (atoi (nth 3 (nth Cnt# Scores@)))
              NumTop10# (1+ NumTop10#)
        );setq
      );if
      (setq Cnt# (1+ Cnt#))
    );repeat
    (princ)
  );defun MatchScores:
  ;-----------------------------------------------------------------------------
  ; Add2Top10: - Adds Name, Date and Score to Top 10 list
  ; Returns: Updates ScoreData@ list
  ;-----------------------------------------------------------------------------
  (defun Add2Top10: (/ Cnt# Num# ScoreData@)
    (setq ScoreData@ (list Name$ (GetDate) (GetTime) (itoa Score#)))
    (cond
      ( (= NumTop10# 0)
        (setq Scores@ (Change_nth 0 ScoreData@ Scores@))
      );case
      ( (< Score# (atoi (nth 3 (nth 0 Scores@))))
        (setq Scores@ (Insert_nth 0 ScoreData@ Scores@))
        (setq Scores@ (Delete_nth (1- (length Scores@)) Scores@))
      );case
      ( (> Score# TopScores#)
        (setq Scores@ (Change_nth NumTop10# ScoreData@ Scores@))
      );case
      (t (setq Cnt# 0)
        (repeat NumTop10#
          (if (> Score# (atoi (nth 3 (nth Cnt# Scores@))))
            (setq Num# (1+ Cnt#))
          );if
          (setq Cnt# (1+ Cnt#))
        );repeat
        (setq Scores@ (Insert_nth Num# ScoreData@ Scores@))
        (setq Scores@ (Delete_nth (1- (length Scores@)) Scores@))
      );case
    );cond
  );defun Add2Top10:
  ;-----------------------------------------------------------------------------
  ; ShowScores: - Displays score board
  ;-----------------------------------------------------------------------------
  (defun ShowScores: (/ Dcl_Id% Num# ClearScores:)
    ;---------------------------------------------------------------------------
    ; ShowScores: subfunction = ClearScores:
    ;---------------------------------------------------------------------------
    ; ClearScores: - Clears previous scores
    ; Returns: Clears and displays score board
    ;---------------------------------------------------------------------------
    (defun ClearScores: (/ Answer$ Cnt#)
      (if (/= NumTop10# 0)
        (progn
          (setq Answer$ (GetYesNo "Match Slides"
            "Are you sure you want to\nclear the previous scores?" "")
          );setq
          (if (= Answer$ "Yes")
            (progn
              (setq Scores@ nil
                    TopScores# 99999999
                    NumTop10# 0
              );setq
              (repeat 10
                (setq Scores@ (append Scores@ (list (list "" "" "" ""))))
              );repeat
              (MatchScores: t)
              (setq Cnt# 1)
              (repeat 10
                (set_tile (strcat "Name"  (itoa Cnt#))
                  (nth 0 (nth (1- Cnt#) Scores@)))
                (set_tile (strcat "Date"  (itoa Cnt#))
                  (nth 1 (nth (1- Cnt#) Scores@)))
                (set_tile (strcat "Time"  (itoa Cnt#))
                  (nth 2 (nth (1- Cnt#) Scores@)))
                (set_tile (strcat "Score" (itoa Cnt#))
                  (nth 3 (nth (1- Cnt#) Scores@)))
                (setq Cnt# (1+ Cnt#))
              );repeat
            );progn
          );if
        );progn
      );if
    );defun ClearScores:
    ;---------------------------------------------------------------------------
    ; ShowScores: - Start Main Function
    ;---------------------------------------------------------------------------
    (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
    (new_dialog "Scores" Dcl_Id%)
    (setq Num# 1)
    (repeat 10
      (set_tile (strcat "Name"  (itoa Num#)) (nth 0 (nth (1- Num#) Scores@)))
      (set_tile (strcat "Date"  (itoa Num#)) (nth 1 (nth (1- Num#) Scores@)))
      (set_tile (strcat "Time"  (itoa Num#)) (nth 2 (nth (1- Num#) Scores@)))
      (set_tile (strcat "Score" (itoa Num#)) (nth 3 (nth (1- Num#) Scores@)))
      (setq Num# (1+ Num#))
    );repeat
    (action_tile "Clear" "(ClearScores:)")
    (start_dialog)
    (unload_dialog Dcl_Id%)
  );defun ShowScores:
  ;-----------------------------------------------------------------------------
  ; NewGame: - Prompt before quiting game
  ;-----------------------------------------------------------------------------
  (defun NewGame: ()
    (if (not Finished)
      (if (= (GetYesNo "Match Slides"
          "Do you want to quit\nthe current game?" "")
          "Yes")
        (done_dialog 1)
      );if
      (done_dialog 1)
    );if
  );defun NewGame:
  ;-----------------------------------------------------------------------------
  ; ExitYesNo: - Prompt before exiting Match Slides
  ;-----------------------------------------------------------------------------
  (defun ExitYesNo: ()
    (if (not Finished)
      (if (= (GetYesNo "Match Slides"
          "Are you sure you want to exit Match Slides?" "")
          "Yes")
        (done_dialog 0)
      );if
      (done_dialog 0)
    );if
  );defun ExitYesNo:
  ;-----------------------------------------------------------------------------
  ; c:Match - Start Main Function
  ;-----------------------------------------------------------------------------
  (princ "\nMatch Slides\n")(princ)
  (setvar "CMDECHO" 0)
  (MatchScores: nil)
  (setq Loop1 t)
  (while Loop1
    (GetSldFolder:)
    (setq Pick$ "First"
          DoneList@ nil
          Score# 0
          Finished nil
    );setq
    (setq Dcl_Id% (load_dialog "Blk_Lib.dcl"))
    (new_dialog "Match" Dcl_Id%)
    ;---------------------------------------------------------------------------
    ; Set Dialog Initial Settings
    ;---------------------------------------------------------------------------
    (set_tile "ScoreIs" (strcat "Score: " (itoa Score#)))
    (set_tile "Title" " Match Slides")
    (ShuffleSlides:)
    (GetOK "Match Slides" "New Game! Pick slides\nto find matches." "")
    (set_tile "PickIs" (strcat Pick$ " Pick"))
    ;---------------------------------------------------------------------------
    ; Dialog Actions
    ;---------------------------------------------------------------------------
    (setq Num# 1)
    (repeat 25
      (if (< Num# 10)
        (action_tile (strcat "Image00" (itoa Num#)) "(Chk_Match: $key)")
        (action_tile (strcat "Image0"  (itoa Num#)) "(Chk_Match: $key)")
      );if
      (setq Num# (1+ Num#))
    );repeat
    (action_tile "New"    "(NewGame:)")
    (action_tile "Scores" "(ShowScores:)")
    (action_tile "cancel" "(ExitYesNo:)")
    (setq Return# (start_dialog))
    (unload_dialog Dcl_Id%)
    (if (= Return# 0)
      (setq Loop1 nil)
    );if
  );while
  (princ)
);defun c:Match
;-------------------------------------------------------------------------------
; Blk_Lib_Support - Checks to see if supporting functions are loaded
;-------------------------------------------------------------------------------
(defun Blk_Lib_Support ()
  (vl-load-com)
  (if (not (findfile "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Temp.bat"))
    (progn
      (vl-mkdir "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config")
      (vl-mkdir "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Lib")
      (vl-mkdir "C:\\Autodesk\\Civil_3D\\2020\\Block\\_Config\\Blk_Sld")
    );progn
  );if
  (if (not (findfile "C:\\Temp\\Temp.scr"))
    (vl-mkdir "C:\\Temp")
  );if
  (if (or (not GetOK)(not EditBox))
    (progn
      (if (findfile "Blk_Lib_GetIcon.lsp")
        (load "Blk_Lib_GetIcon.lsp")
      );if
      (if (or (not GetOK)(not EditBox))
        (progn
          (alert (strcat "Blk_Lib requires the functions inside of GetIcon.lsp."
            "\nDownload the latest version from AutoLISP Exchange,"
            "\n(URL: http://web2.airmail.net/terrycad).")
          );alert
          (exit)
        );progn
      );if
    );progn
  );if
);defun Blk_Lib_Support
(Blk_Lib_Support)
;-------------------------------------------------------------------------------
(princ);End of Blk_Lib.lsp
