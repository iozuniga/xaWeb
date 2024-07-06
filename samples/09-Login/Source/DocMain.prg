// https://test.Oz Software/cgi-bin/xailerweb/login.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   DATA cUser INIT "" PERSISTENT

   METHOD CreateDoc()
   METHOD DefaultPage()
   METHOD CodePage()

   METHOD Act_LoginPage()
   METHOD Act_Page2()
   METHOD Act_Page3()

   METHOD Frm_PageCheck( hParams )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   #IFDEF _LINUX_
      Engine:cLogFile := "/mnt/c/xailerweb/samples/09-Login/error.log"
   #ENDIF

   Engine:lDebug := .t.

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::RegisterPage( "DefaultPage" )
   ::RegisterPage( "CodePage" )

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultPage() CLASS WDocMain

   LOCAL oPage

   oPage := ::AddPage( "default" )

   WITH OBJECT WParagraph():New( oPage )
      ECHO "<h1>XailerWeb - Simple login demo</h1>"
      IF !Empty( ::cUser )
         ECHO "<p>You are logged as user: " + ::cUser + "</p>"
      ENDIF
   END WITH

   WITH OBJECT WNav():New( oPage )
      ECHO "<ul>"
      WITH OBJECT WLink():New( SO )
         :cText := "Log in"
         :cHref := ::Action( "Act_LoginPage" )
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

   oPage:lForced := .T.

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

METHOD Act_LoginPage() CLASS WDocMain

   LOCAL oPage, oForm, oGroup

   oPage := ::AddPage( "PageLogin" )

   WParagraph():New( oPage )

   WITH OBJECT oForm := WForm():New( oPage )
      :cMethod := "post"
      :cName := "Frm_PageCheck"
      :lAutoComplete := .T.
      :Create()
      WITH OBJECT oGroup := WFieldset():New( oForm )
         :cLegend := "Login credentials"
         :cName := "User data"
         :AddStyle( "width: 400px" )
         :Create()
         WITH OBJECT WEmail():New( oGroup )
            :cLabel := "Email:"
            :lLabelNewLine := .t.
            :cId := "email"
            :cName := "email"
            :cPlaceHolder := "email"
            :cAutoComplete := "email"
            :nSize := 50
            :nMaxLength := 50
            :nMinLength := 10
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :cLabel := "Password:"
            :lLabelNewLine := .t.
            :cType := "password"
            :cId := "password"
            :cName := "password"
            :cPlaceHolder := "password"
            :cAutoComplete := "password"
            :nSize := 50
            :nMaxLength := 50
            :nMinLength := 10
            :Create()
         END WITH
      END WITH
      WITH OBJECT WButton():New( oForm )
         :cType := "submit"
         :cText := "Submit"
         :cId := "button"
         :Create()
      END WITH
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

METHOD Frm_PageCheck( hParams ) CLASS WDocMain

   LOCAL oPage

   ::cUser := hParams[ "email" ]

   WITH OBJECT oPage := ::AddPage( "checklogin" )
      ECHO "<ul>"
      ECHO "<h1>Login successful</h1>"
      ECHO "<p>Welcome " + hParams[ "email" ] + "!</p>"
      ECHO "</ul>"
   END WITH

RETURN oPage

//------------------------------------------------------------------------------
