#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cJs, cCode

   #IFDEF _LINUX_
      Engine:cLogFile := "/mnt/c/xailerweb/samples/30-Comp-Buttons/error.log"
   #ENDIF

   Engine:lDebug := .t.

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   WITH OBJECT WParagraph():New( Self )
      ECHO "<h1>XailerWeb Components demo</h1>"
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
      END WITH
      WITH OBJECT WCmpNumerickeypad():New( SO )
         :lDecimalDot := .f.
         :cEditId     := "keypad"
      END WITH
   END WITH

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WTextArea():New( Self )
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------



