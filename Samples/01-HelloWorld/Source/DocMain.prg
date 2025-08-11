#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS


METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs, oStyle

   //Engine:lDebug := .t.

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
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

   WITH OBJECT WParagraph():New( Self )
      :cText := "<h1>xaWeb - Hello World!</h1>"
      :oStyle:Color := "blue"
      :Create()
   END WITH

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
