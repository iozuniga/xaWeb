// https://test.Oz Software/cgi-bin/xailerweb/containers.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ECHO "<h1>XailerWeb - Containers</h1>" INTO Self

   WITH OBJECT WDiv():New( Self )
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

   TEXT INTO cJS
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
