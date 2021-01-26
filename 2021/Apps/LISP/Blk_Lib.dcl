dcl_settings : default_dcl_settings { audit_level = 3; }
//------------------------------------------------------------------------------
// Program Name: Blk_Lib.dcl [Block Library R17]
// Created By:   Terry Miller (Email: terrycadd@yahoo.com)
//               (URL: http://web2.airmail.net/terrycad)
// Date Created: 1-1-00
// Function:     Loads Block Library functions to insert blocks and adds blocks
//               to Block Libraries.
// Note:         Blk_Lib requires functions inside of GetIcon.lsp.
//------------------------------------------------------------------------------
// Revision History
// Rev  By     Date    Description
//------------------------------------------------------------------------------
// 1    TM   1-1-00    Initial version.
// 2    TM   2-1-00    Added c:Sel_Lib, Select Block Library.
// 3    TM   3-1-00    Revised c:InBlocks and c:All_Blk_Lib functions.
// 4    TM   4-1-00    Created and added GetIcon.lsp Get functions.
// 5    TM   5-1-00    Revised code for AutoCAD 2000 compatibility.
// 6    TM   12-1-00   Revised GetIcon.lsp to allow up to 4 lines and to allow
//                     choosing different icons.
// 7    TM   1-1-01    Included Blk_Lib as the main command function and added
//                     more icons to GetIcon.lsp.
// 8    TM   10-1-03   Added c:LIB, shortcut for c:Library, for a user version
//                     of Select Block Library. Included insertion dot as the
//                     default for slides. Allow user to control block rotation.
// 9    TM   10-20-03  Added Slide_Script function to be used with Select Block
//                     Library to add folders of drawings to Block Libraries.
//                     Added c:Mat, shortcut for c:Match, the Match Slides Game.
// 10   TM   5-20-04   Added GetDwgsList function to check if drawing environment
//                     is a Single Document Interface before running scripts.
// 11   TM   12-20-04  Revised code for AutoCAD 2005 compatibility. Redesigned
//                     the dialogs with slide images, and increased the width
//                     for block names to allow more room for longer block names.
// 12   TM   3-20-05   Detached GetIcon.lsp functions into a separate file.
// 13   TM   9-20-05   Revised some of the dialog control functions and reworded
//                     some of the dialog messages.
// 14   TM   7-20-06   Revised code for AutoCAD 2007 compatibility.
// 15   TM   11-30-07  Added runapp function for DOS applications.
// 16   TM   2-1-08    Revised INBL, shortcut for c:InBlocks function, to insert
//                     blocks from a folder of drawings into a blank drawing to
//                     manage and add blocks to Block Libraries.
// 17   TM   2-20-08   Added ADL, shortcut for c:Add_Dwgs function, to add a
//                     folder of drawings to a library without inserting them
//                     into a drawing.
//------------------------------------------------------------------------------
// Blk_Lib_Msg [Block Library Message] - Installation Instructions
//------------------------------------------------------------------------------
//  Note: Block Library saves a backup of your drawing as C:\Custom\Blk_Lib.dwg
//  before it creates slides and wblocks.  If the program is interrupted or
//  canceled open C:\Custom\Blk_Lib.dwg as a backup.
//------------------------------------------------------------------------------
Blk_Lib_Msg : dialog {
  label = " Block Library Message";
  width = 68;
  : boxed_column {
    : text {
      key = "msg1";
      label = "";
      alignment = left;
    }
    : text {
      key = "msg2";
      label = "";
      alignment = left;
    }
    : text {
      key = "msg3";
      label = "";
      alignment = left;
    }
    : text {
      key = "msg4";
      label = "";
      alignment = left;
    }
    : text {
      key = "msg5";
      label = "";
      alignment = left;
    }
    spacer_1;
  }
  : ok_button {
    label = "Accept";
    mnemonic = "A";
  }
}//Blk_Lib_Msg
//------------------------------------------------------------------------------
// Blk_Lib [Block Library] - Inserts blocks from Library
//------------------------------------------------------------------------------
Blk_Lib : dialog {
  label = "";
  key = "title";
  initial_focus = "cancel";
  spacer;
  : row {
    : icon_image {
      key = "sld1";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld2";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld3";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld4";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld5";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld1text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld2text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld3text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld4text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld5text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld6";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld7";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld8";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld9";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld10";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld6text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld7text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld8text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld9text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld10text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld11";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld12";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld13";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld14";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld15";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld11text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld12text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld13text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld14text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld15text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld16";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld17";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld18";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld19";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld20";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld16text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld17text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld18text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld19text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld20text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : column {
      : button {
        key   = "previous";
        label = "< Previous";
        mnemonic = "P";
        width = 66.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : button {
        key   = "next";
        label = "Next >";
        mnemonic = "N";
        width = 66.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : column {
      : ok_button {
        label = "Insert Block";
        mnemonic = "I";
        width = 44.09;
        alignment = left;
      }
    }
    : column {
      : cancel_button {
        mnemonic = "C";
        width = 44.09;
        alignment = right;
      }
    }
  }
}//Blk_Lib
//------------------------------------------------------------------------------
// Blk_Mgr [Block Library Manager] - Manages blocks for Library
//------------------------------------------------------------------------------
Blk_Mgr : dialog {
  label = "";
  key = "title";
  : row {
    alignment= centered;
    spacer_0;
    : boxed_column {
      width = 18.26;
      fixed_width = true;
      : text {
        label = "Select Block";
      }
      : popup_list {
        key = "block";
      }
      spacer;
      : text {
        label = "Insertion Point";
      }
      : popup_list {
        key = "point";
      }
      spacer;
      : toggle {
        label = "Explode";
        key = "explode";
      }
      spacer;
    }
    spacer_0;
    : column {
      spacer;
      : icon_image {
        key = "blocksld";
        width = 36.26;
        height = 11.12;
        aspect_ratio = 1;
        alignment = centered;
      }
    }
    spacer_0;
    : boxed_column {
      width = 18.26;
      fixed_width = true;
      : text {
        label = "Insertion Layer";
      }
      : popup_list {
        key = "layer";
      }
      spacer;
      : text {
        label = "Insertion Scale";
      }
      : popup_list {
        key = "scale";
      }
      spacer;
      : toggle {
        key = "dot";
        label = "Insertion Dot";
      }
      spacer;
    }
  }
  spacer;
  : row {
    alignment = centered;
    fixed_width = true;
    : button {
      key = "addlib";
      label = "Add to Library";
      mnemonic = "A";
      width = 20.26;
      fixed_width = true;
    }
    : button {
      key = "viewblk";
      label = "View Blocks";
      mnemonic = "V";
      width = 17.59;
      fixed_width = true;
    }
    : button {
      key = "editlib";
      label = "Edit Library";
      mnemonic = "E";
      width = 17.59;
      fixed_width = true;
    }
    : cancel_button {
      mnemonic = "C";
      width = 20.26;
    }
  }
}
//------------------------------------------------------------------------------
// Edit_Lib [Edit Block Library] - Rearrange or delete block insertion data
//------------------------------------------------------------------------------
Edit_Lib : dialog {
  label = "";
  key = "title";
  initial_focus = "cancel";
  spacer;
  : row {
    : icon_image {
      key = "sld1";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld2";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld3";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld4";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld5";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld1text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld2text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld3text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld4text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld5text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld6";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld7";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld8";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld9";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld10";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld6text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld7text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld8text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld9text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld10text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld11";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld12";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld13";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld14";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld15";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld11text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld12text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld13text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld14text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld15text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld16";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld17";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld18";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld19";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld20";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld16text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld17text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld18text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld19text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld20text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : column {
      : button {
        key   = "previous";
        label = "< Previous";
        mnemonic = "P";
        width = 66.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : button {
        key   = "next";
        label = "Next >";
        mnemonic = "N";
        width = 66.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : column {
      : button {
        key   = "move";
        label = "Move";
        mnemonic = "M";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : button {
        key   = "paste";
        label = "Paste";
        mnemonic = "P";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : button {
        key   = "delete";
        label = "Delete";
        mnemonic = "D";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : button {
        key   = "accept";
        label = "Accept";
        mnemonic = "A";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : cancel_button {
        mnemonic = "C";
        width = 25.92;
      }
    }
  }
}//Edit_Lib
//------------------------------------------------------------------------------
// Dwg_Blks [Drawing Blocks] - View and select drawing blocks
//------------------------------------------------------------------------------
Dwg_Blks : dialog {
  label = "";
  key = "title";
  initial_focus = "cancel";
  spacer;
  : row {
    : icon_image {
      key = "sld1";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld2";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld3";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld4";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld5";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld1text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld2text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld3text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld4text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld5text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld6";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld7";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld8";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld9";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld10";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld6text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld7text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld8text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld9text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld10text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld11";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld12";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld13";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld14";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld15";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld11text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld12text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld13text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld14text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld15text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : icon_image {
      key = "sld16";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld17";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld18";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld19";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
    : icon_image {
      key = "sld20";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
    }
  }
  : row {
    : column {
      : text {
        key = "sld16text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld17text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld18text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld19text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : text {
        key = "sld20text";
        label = "";
        width = 25.92;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : column {
      : button {
        key   = "previous";
        label = "< Previous";
        mnemonic = "P";
        width = 66.92;
        fixed_width = true;
        alignment = centered;
      }
      : ok_button {
        mnemonic = "O";
        alignment = right;
        width = 11;
      }
    }
    : column {
      : button {
        key   = "next";
        label = "Next >";
        mnemonic = "N";
        width = 66.92;
        fixed_width = true;
        alignment = centered;
      }
      : cancel_button {
        mnemonic = "C";
        alignment = left;
        width = 11;
      }
    }
  }
}//Dwg_Blks
//------------------------------------------------------------------------------
// Sel_Lib [Select Block Library]
//------------------------------------------------------------------------------
Sel_Lib : dialog {
  label = " Select Block Library";
  initial_focus = "cancel";
  : boxed_column {
    label = "Libraries";
    : list_box {
      key = "library";
      height = 11.20;
      allow_accept = true;
    }
    spacer;
  }
  : row {
    : column {
      : button {
        key   = "add";
        label = "Add";
        mnemonic = "A";
        width = 14;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : button {
        key   = "edit";
        label = "Edit";
        mnemonic = "E";
        width = 14;
        fixed_width = true;
        alignment = centered;
      }
    }
  }
  : row {
    : column {
      : button {
        key   = "select";
        label = "Select";
        mnemonic = "S";
        width = 14;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : cancel_button {
        mnemonic = "C";
        width = 14;
      }
    }
  }
}//Sel_Lib
//------------------------------------------------------------------------------
// Library
//------------------------------------------------------------------------------
Library : dialog {
  label = "";
  key = "title";
  initial_focus = "cancel";
  : boxed_column {
    label = "Select Library";
    : list_box {
      key = "library";
      height = 11.20;
      allow_accept = true;
    }
    spacer;
  }
  : row {
    : column {
      : button {
        key   = "select";
        label = "Select";
        mnemonic = "S";
        width = 14;
        fixed_width = true;
        alignment = centered;
      }
    }
    : column {
      : cancel_button {
        mnemonic = "C";
        width = 14;
      }
    }
  }
}//Library
//------------------------------------------------------------------------------
// Add_Lib_Defs [Add Library Definitions]
//------------------------------------------------------------------------------
Add_Lib_Defs : dialog {
  label = " Add Library Definitions";
  initial_focus = "cancel";
  : boxed_column {
    label = "Library Definitions";
    : row {
      : column {
        width = 6.59;
        fixed_width = true;
        spacer;
        : text {
          key = "titlelabel";
          label = "";
        }
        spacer_0;
      }
      : edit_box {
        key = "title";
        edit_width = 39.76;
        edit_limit = 40;
      }
      : button {
        key   = "clear";
        label = "Clear";
        width = 11.76;
        fixed_width = true;
        alignment = centered;
      }
    }
    : row {
      : column {
        width = 6.59;
        fixed_width = true;
        spacer;
        : text {
          key = "folderlabel";
          label = "";
        }
        spacer_0;
      }
      : edit_box {
        key = "sldpath";
        edit_width = 39.76;
        is_tab_stop = false;
      }
      : button {
        key   = "browse";
        label = "Browse";
        width = 11;
        fixed_width = true;
        alignment = centered;
      }
    }
    : spacer {
    }
  }
  : row {
    : column {
      : button {
        key   = "accept";
        label = "Accept";
        mnemonic = "A";
        width = 11;
        fixed_width = true;
        alignment = right;
      }
    }
    : column {
      : cancel_button {
        mnemonic = "C";
        alignment = left;
        width = 11;
      }
    }
  }
}//Add_Lib_Defs
//------------------------------------------------------------------------------
// Edit_Lib_Defs [Edit Library Definitions]
//------------------------------------------------------------------------------
Edit_Lib_Defs : dialog {
  label = " Edit Library Definitions";
  initial_focus = "cancel";
  : boxed_column {
    label = "Library Definitions";
    : row {
      : column {
        width = 6.59;
        fixed_width = true;
        spacer;
        : text {
          key = "titlelabel";
          label = "";
        }
        spacer_0;
      }
      : edit_box {
        key = "title";
        edit_width = 39.76;
        edit_limit = 40;
      }
      : button {
        key   = "clear";
        label = "Clear";
        width = 11.76;
        fixed_width = true;
        alignment = centered;
      }
    }
    : row {
      : column {
        width = 6.59;
        fixed_width = true;
        spacer;
        : text {
          key = "folderlabel";
          label = "";
        }
        spacer_0;
      }
      : edit_box {
        key = "sldpath";
        edit_width = 39.76;
        is_tab_stop = false;
      }
      : button {
        key   = "browse";
        label = "Browse";
        width = 11;
        fixed_width = true;
        alignment = centered;
      }
    }
    : spacer {
    }
  }
  : row {
    fixed_width = true;
    alignment = centered;
    : button {
      key   = "accept";
      label = "Accept";
      mnemonic = "A";
      width = 11;
    }
    : button {
      key   = "delete";
      label = "Delete";
      mnemonic = "D";
      width = 11;
    }
    : cancel_button {
      mnemonic = "C";
      width = 11;
    }
  }
}//Edit_Lib_Defs
//------------------------------------------------------------------------------
// FolderOptions - Library Folder Options
//------------------------------------------------------------------------------
FolderOptions : dialog {
  label = "";
  key = "title";
  : boxed_row {
    label = "Library Folder Options";
    : column {
      : radio_column {
        key = "radio";
        : radio_button {
          key = "1";
          vertical_margin = none;
        }
        : radio_button {
          key = "2";
          vertical_margin = none;
        }
        : radio_button {
          key = "3";
          vertical_margin = none;
        }
      }
      : spacer {
        height = 1.43;
        fixed_height = true;
      }
    }
    : column {
      width = 35.1;
      fixed_width = true;
      : text {
        key = "msg1";
        label = "";
      }
      : text {
        key = "msg2";
        label = "";
      }
      : text {
        key = "msg3";
        label = "";
      }
      : text {
        key = "msg4";
        label = "";
      }
      : spacer { height = 0.2; }
    }
  }
  : row {
    : column {
      : ok_button {
        label = "OK";
        width = 8.0;
        is_cancel = true;
        alignment = right;
      }
    }
    : column {
      : button {
        key = "info";
        label = "Info";
        width = 8.0;
        fixed_width = true;
        alignment = left;
      }
    }
  }
}//FolderOptions
//------------------------------------------------------------------------------
// Add_Dwgs - Add Drawings to Library
//------------------------------------------------------------------------------
Add_Dwgs : dialog {
  label = "";
  key = "title";
  : boxed_column {
    : row {
      fixed_width = true;
      : column {
        width = 18;
        spacer;
        : text {
          label = "Insertion Layer";
        }
      }
      : popup_list {
        key = "layer";
        width = 24;
        fixed_width = true;
      }
    }
    : row {
      fixed_width = true;
      : column {
        width = 18;
        spacer;
        : text {
          label = "Insertion Scale";
        }
      }
      : popup_list {
        key = "scale";
        width = 24;
        fixed_width = true;
      }
    }
    : row {
      fixed_width = true;
      : column {
        width = 18;
        spacer;
        : text {
          label = "Insertion Point";
        }
      }
      : popup_list {
        key = "point";
        width = 24;
        fixed_width = true;
      }
    }
    spacer;
    : row {
      fixed_width = true;
      alignment = centered;
      : toggle {
        label = "Explode";
        key = "explode";
      }
      spacer;
      : toggle {
        key = "dot";
        label = "Insertion Dot";
      }
    }
    spacer;
  }
  spacer;
  : row {
    alignment = centered;
    fixed_width = true;
    : button {
      key = "skipdwg";
      label = "Skip";
      mnemonic = "S";
      width = 11;
      fixed_width = true;
    }
    : button {
      key = "addlib";
      label = "Add to Library";
      mnemonic = "A";
      width = 19;
      fixed_width = true;
    }
    : cancel_button {
      mnemonic = "C";
      width = 11;
    }
  }
}//Add_Dwgs
//------------------------------------------------------------------------------
// Blk_Menu - Block Library Menu
//------------------------------------------------------------------------------
Blk_Menu : dialog {
  label = " Block Library Menu";
  : list_box {
    allow_accept = true;
    key = "CommandList";
    label = "Shortcuts,    Commands,  and Descriptions";
    tabs = "10";
    height = 9.97;
    width = 42;
  }
  : cancel_button {
  }
}//Blk_Menu
//------------------------------------------------------------------------------
// Match
//------------------------------------------------------------------------------
Match : dialog {
  label = "";
  key = "Title";
  initial_focus = "cancel";
  spacer;
  : row {
    : icon_image {
      key = "Image001";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image002";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image003";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image004";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image005";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
  }
  : row {
    : icon_image {
      key = "Image006";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image007";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image008";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image009";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image010";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
  }
  : row {
    : icon_image {
      key = "Image011";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image012";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image013";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image014";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image015";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
  }
  : row {
    : icon_image {
      key = "Image016";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image017";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image018";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image019";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image020";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
  }
  : row {
    : icon_image {
      key = "Image021";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image022";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image023";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image024";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
    : icon_image {
      key = "Image025";
      width = 25.92;
      height = 7.97;
      aspect_ratio = 1;
      alignment = centered;
      color = -15;
    }
  }
  : spacer { height = 0.01; }
  : row {
    : column {
      width = 22;
      fixed_width = true;
      : spacer { height = 0.01;}
      : text {
        key = "ScoreIs";
        label = "";
        alignment = left;
      }
      : spacer { height = 0.01;}
    }
    : row {
      fixed_width = true;
      : button {
        key = "New";
        label = "&New";
        width = 11;
        fixed_width = true;
      }
      : button {
        key = "Scores";
        label = "&Scores";
        width = 11;
        fixed_width = true;
      }
      : cancel_button {
        label = "&Exit";
        width = 11;
      }
    }
    : column {
      width = 22;
      fixed_width = true;
      : spacer { height = 0.01;}
      : text {
        key = "PickIs";
        label = "";
        alignment = right;
      }
      : spacer { height = 0.01;}
    }
  }
}//Match
//------------------------------------------------------------------------------
// Scores
//------------------------------------------------------------------------------
Scores : dialog {
  label = " Match Slides";
  spacer;
  : boxed_column {
    label = "Top 10 Scores";
    width = 54;
    : row {
      : text { label = " ";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              label = "Name";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              label = "Date";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              label = "Time";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              label = "Score";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 1";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name1";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date1";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time1";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score1";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 2";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name2";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date2";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time2";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score2";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 3";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name3";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date3";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time3";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score3";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 4";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name4";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date4";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time4";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score4";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 5";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name5";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date5";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time5";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score5";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 6";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name6";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date6";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time6";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score6";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 7";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name7";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date7";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time7";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score7";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 8";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name8";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date8";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time8";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score8";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = " 9";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name9";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date9";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time9";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score9";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : row {
      : text { label = "10";}
      : column {
        width = 50;
        fixed_width = true;
        : row {
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Name10";
              label = "";
              alignment = left;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Date10";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Time10";
              label = "";
              alignment = centered;
            }
          }
          : column {
            width = 12;
            fixed_width = true;
            : text {
              key = "Score10";
              label = "";
              alignment = right;
            }
          }
        }
      }
    }
    : spacer { height = 0.01;}
  }
  : row {
    : column {
      : ok_button {
        is_cancel = true;
        alignment = right;
        width = 11;
      }
    }
    : column {
      : button {
        key = "Clear";
        label = "Clear";
        alignment = left;
        width = 11;
        fixed_width = true;
      }
    }
  }
}//Scores
//------------------------------------------------------------------------------
