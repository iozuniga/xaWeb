
#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oMainSection

   METHOD CreateDoc()
   METHOD DefaultSection()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::DefaultSection()

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultSection() CLASS WDocMain

   LOCAL cCode, cJs

   ::oMainSection := ::AddSection( "Default" )

   ECHO "<h1>xaWeb - Google Icons</h1>" INTO Self

   WITH OBJECT WDiv():New( ::oMainSection )
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "face"
      END WITH
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "face"
         :nSize := 48
      END WITH
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "face"
         :lDark := .T.
      END WITH
      WITH OBJECT WIconGoogle():New( SO )
         :oStyle:Background_Color := "black"
         :cText := "face"
         :lLight := .T.
      END WITH
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "face"
         :lDisabled := .T.
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

   ECHO "<hr>" INTO ::oMainSection

   FILE "DocMain.prg" INTO cCode

      WITH OBJECT WButton():New( ::oMainSection )
         :cText := "This button shows xaWeb source code"
         :Onclick := "showCode"
         :cId := "btnsource"
         :Create()
      END WITH

      WITH OBJECT WTextArea():New( ::oMainSection )
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

