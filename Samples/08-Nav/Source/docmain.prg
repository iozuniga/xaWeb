// https://test.ozs.es/cgi-bin/xaWeb/nav.cgi

#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

   METHOD DefaultSection()
   METHOD Act_Section1()
   METHOD Act_Section2()
   METHOD Act_Section3()
   METHOD CodeSection()

END CLASS

METHOD CreateDoc() CLASS WDocMain

//   Engine:lDebug := .t.

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::DefaultSection()
   ::CodeSection()

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultSection() CLASS WDocMain

   LOCAL oSection

   WITH OBJECT oSection := ::AddSection( "default" )
      :lDeploy := .t.
      ECHO "<h1>xaWeb - Simple Nav demo</h1>"

      WITH OBJECT WNav():New( SO )
         :AddStyle( "display:flex; gap:0.5em; flex-wrap:wrap;" )
         WITH OBJECT WLink():New( SO )
            :cText := "First section"
            :cHref := ::Action( "Act_Section1" )
         END WITH
         WITH OBJECT WLink():New( SO )
            :cText := "Second section"
            :cHref := ::Action( "Act_Section2" )
         END WITH
         WITH OBJECT WLink():New( SO )
            :cText := "Third section"
            :cHref := ::Action( "Act_Section3" )
         END WITH
      END WITH
   END WITH

RETURN oSection

//------------------------------------------------------------------------------

METHOD Act_Section1() CLASS WDocMain

   LOCAL oSection

   WITH OBJECT oSection := ::AddSection( "section1" )
      ECHO "<ul><h4>This is section number one</h4></ul>"
   END WITH

RETURN oSection

//------------------------------------------------------------------------------

METHOD Act_Section2() CLASS WDocMain

   LOCAL oSection

   WITH OBJECT oSection := ::AddSection( "section2" )
      ECHO "<ul><h4>This is section number two</h4></ul>"
   END WITH

RETURN oSection

//------------------------------------------------------------------------------

METHOD Act_Section3() CLASS WDocMain

   LOCAL oSection

   WITH OBJECT oSection := ::AddSection( "section3" )
      ECHO "<ul><h4>This is section number three</h4></ul>"
   END WITH

RETURN oSection

//------------------------------------------------------------------------------

METHOD CodeSection() CLASS WDocMain

   LOCAL oSection
   LOCAL cCode, cJs

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

   WITH OBJECT oSection := ::AddSection( "code" )
      :lDeploy := .t.
      :lFooter := .t.

      ECHO "<hr>"

      FILE "DocMain.prg" INTO cCode

      WITH OBJECT WButton():New( SO )
         :cText := "This button shows xaWeb source code"
         :Onclick := "showCode"
         :cId := "btnsource"
         :Create()
      END WITH

      WITH OBJECT WTextArea():New( SO )
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

   END WITH

RETURN oSection

//------------------------------------------------------------------------------

