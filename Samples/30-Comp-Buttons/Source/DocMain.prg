#include "xaWeb.ch"

/*
This sample only works on a external web navigator
You must copy the JS files under www/js to web server Home_directory + "\js"
*/


CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cJs, cCode

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   WITH OBJECT WParagraph():New( Self )
      ECHO "<h1>xaWeb Components demo</h1>"
      WITH OBJECT WCmpButtonIcon():New( Self )
         :cIcon := "save"
         :cText := "Save changes"
      END WITH
      WITH OBJECT WCmpButtonIcon():New( Self )
         :cIcon    := "checkmark"
         :cVariant := "success"

         :cText    := "Successful operation"
      END WITH
      WITH OBJECT WCmpButtonIcon():New( Self )
         :cIcon    := "bug"
         :cVariant := "error"
         :cText    := "Compiling error"
      END WITH
      WITH OBJECT WCmpButtonSpinner():New( Self )
         :cText    := "Click on Me!"
         :cType    := "button"
         :Onclick  :='<script>setLoading();</script>'
         :cId := "mySpinner"
         :Create()
      END WITH

      WITH OBJECT WButton():New( Self )
         :cText    := "Standard button"
         :cType    := "button"
      END WITH

   END WITH

   WITH OBJECT WDiv():New(Self )
      :AddStyle( "width:50%;margin:0 auto;" )
      WITH OBJECT WEdit():New( SO )
         :cId = "keypad"
         :oError:AddStyle( "display:none;")
         :Create()
      END WITH
      WITH OBJECT WCmpNumerickeypad():New( SO )
         :lDecimalDot := .f.
         :cEditId     := "keypad"
      END WITH
   END WITH

   TEXT INTO cJs
      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.value);
            ele.hidden = false;
            document.getElementById("btnsource").style.display = "none";
         }
      }
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WButton():New( Self )
      :cText := "This button shows xaWeb source code"
      :Onclick := "showCode"
      :cId := "btnsource"
      :Create()
   END WITH

   WITH OBJECT WTextArea():New( Self )
      :oStyle:Margin_top := "20px"
      :oStyle:Font_Family := "monospace"
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------



