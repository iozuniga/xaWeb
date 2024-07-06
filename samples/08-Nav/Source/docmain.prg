// https://test.Oz Software/cgi-bin/xailerweb/nav.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

   METHOD DefaultPage()
   METHOD Act_Page1()
   METHOD Act_Page2()
   METHOD Act_Page3()
   METHOD CodePage()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   #IFDEF _LINUX_
      Engine:cLogFile := "/mnt/c/xailerweb/samples/08-Nav/error.log"
   #ENDIF

   Engine:lDebug := .t.

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::DefaultPage()
   ::CodePage()

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultPage() CLASS WDocMain

   LOCAL oPage

   WITH OBJECT oPage := ::AddPage( "default" )
      :lForced := .t.
      ECHO "<h1>XailerWeb - Simple Nav demo</h1>"

      WITH OBJECT WNav():New( oPage )
         ECHO "<ul>"
         WITH OBJECT WLink():New( SO )
            :cText := "First page"
            :cHref := ::Action( "Act_Page1" )
         END WITH
         WITH OBJECT WLink():New( SO )
            :cText := "Second page"
            :cHref := ::Action( "Act_Page2" )
         END WITH
         WITH OBJECT WLink():New( SO )
            :cText := "Third page"
            :cHref := ::Action( "Act_Page3" )
         END WITH
         ECHO "</ul>"
      END WITH
   END WITH

RETURN oPage

//------------------------------------------------------------------------------

METHOD Act_Page1() CLASS WDocMain

   LOCAL oPage

   WITH OBJECT oPage := ::AddPage( "page1" )
      ECHO "<ul><h1>This is page number one</h1></ul>"
   END WITH

RETURN oPage

//------------------------------------------------------------------------------

METHOD Act_Page2() CLASS WDocMain

   LOCAL oPage

   WITH OBJECT oPage := ::AddPage( "page2" )
      ECHO "<ul><h1>This is page number two</h1></ul>"
   END WITH

RETURN oPage

//------------------------------------------------------------------------------

METHOD Act_Page3() CLASS WDocMain

   LOCAL oPage

   WITH OBJECT oPage := ::AddPage( "page3" )
      ECHO "<ul><h1>This is page number three</h1></ul>"
   END WITH

RETURN oPage

//------------------------------------------------------------------------------

METHOD CodePage() CLASS WDocMain

   LOCAL oPage
   LOCAL cCode, cJs

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   WITH OBJECT oPage := ::AddPage( "code" )
      :lForced := .t.
      :lFooter := .t.

      ECHO "<hr>"

      FILE "DocMain.prg" INTO cCode

      WITH OBJECT WTextArea():New( oPage )
         :nCols := 80
         :cId   := "source"
         :nRows := 40
         :cText := HB_Base64Encode( cCode )
         :lReadOnly := .t.
         :Create()
      END WITH
   END WITH

RETURN oPage

//------------------------------------------------------------------------------

