// https://test.ozs.es/cgi-bin/xaWeb/containers.cgi

#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oMyDiv

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ECHO "<h1>xaWeb - Containers</h1>" INTO Self

   WITH OBJECT ::oMyDiv := WDiv():New( Self )
      :Create()
      WITH OBJECT WParagraph():New( SO )
         :Create()
         ECHO "This goes on first paragraph"
         WITH OBJECT WButton():New( SO )
            :cText := "Click Me!"
            :Create()
         END WITH
      END WITH
   END WITH

   WITH OBJECT WDiv():New( Self )
      :Create()
      WITH OBJECT WParagraph():New( SO )
         ECHO "This goes on second paragraph"
         :Create()
         WITH OBJECT WButton():New( SO )
            :cText := "Click Me again!"
            :Create()
         END WITH
      END WITH
   END WITH

   WITH OBJECT WDiv():New( Self )
      :Create()
      WITH OBJECT WDiv():New( SO )
         :Create()
         WITH OBJECT WDiv():New( SO )
            :Create()
            WITH OBJECT WDiv():New( SO )
               :Create()
               ECHO "Testing anidation"
            END WITH
         END WITH
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
      :cText := "This button shows xaWeb source code>"
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
