// https://test.Oz Software/cgi-bin/xailerweb/hello.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs, oStyle

   WMaterializeContext():New( Self )

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      :cTitle := "Switch to dark mode"
      WITH OBJECT WControl():New( SO )
         :cTag := "i"
         :AddClass( "material-icons" )
         :cText := "dark_mode"
      END WITH
   END WITH

   WITH OBJECT WDiv():New( Self )
      :AddClass( "container" )
      WITH OBJECT WText():New( SO )
         :cText := "<h1>XailerWeb - Hello World!</h1>"
         :Create()
      END WITH

      FILE "DocMain.prg" INTO cCode

      WITH OBJECT WTextArea():New( SO )
         :AddClass( "materialize-textarea" )
         :nCols := 80
         :cId   := "source"
         :nRows := 40
         :cText := HB_Base64Encode( cCode )
         :lReadOnly := .t.
         :Create()
      END WITH

   END WITH

RETURN nil

//------------------------------------------------------------------------------






