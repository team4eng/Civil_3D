// DT_Curve.dcl for use in conjunction with DTCurve.lsp  //
// Copyright © November 2009 Lee Mac                     //

dt_curve : dialog { key = "stitle";

  spacer;
   
  : row { alignment = centered;
  
    : boxed_column { label = "TextStyle";
    
      : popup_list { key = "styl"; alignment = centered; fixed_width = true; width = 20; }

      spacer_1;

    }

    : boxed_column { label = "Height";

      : row { children_alignment = centered; spacer;

        : edit_box { key = "hgt"; edit_width = 5; alignment = centered; }
  
        spacer;

        : column {

          : spacer { height = 0.01; }

          : toggle { key = "bstyl"; label = " By Style"; alignment = centered; }

        }

      }
      
      spacer;
      
    }

  }

  spacer;

  : row { alignment = centered;

    : spacer { width = 16.06; fixed_width = true; }

    ok_cancel;

    : image { key = "logo"  ; alignment    = centered;
              width = 16.06 ; fixed_width  = true;
              height = 2.06 ; fixed_height = true; color = -15; }
              
  }
  
}
