// https://test.Oz Software/cgi-bin/xailerweb/hello.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs, oStyle

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "auto"
   END WITH

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   WITH OBJECT WText():New( Self )
      :cText := "<h1>XailerWeb - Hello World!</h1>"
      :oStyle:cColor := "blue"
      :Create()
   END WITH

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






