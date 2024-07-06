// https://test.Oz Software/cgi-bin/xailerweb/modalformfetch.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oFormPage, oMainPage
   DATA oForm
   DATA oResult

   METHOD CreateDoc()
   METHOD DefaultPage()
   METHOD FormPage()

   METHOD Srv_CheckDate()
   METHOD Srv_FormData()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::DefaultPage()
   ::FormPage()

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultPage() CLASS WDocMain

   LOCAL cCode, cJs

   ::oMainPage := ::AddPage( "Default" )

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<h1>XailerWeb - Modal form via FETCH</h1>" INTO Self

   WITH OBJECT WButton():New( ::oMainPage  )
      :cText := "Run modal form via FETCH Api"
      :OnClick := :ShowPage( "Page_FormPage" )
      :cId := "mybutton"
   END WITH


   WITH OBJECT ::oResult := WText():New( ::oMainPage )
      :cText := "Form results will be shown here (does not work on built-in Xailer navigator)"
      :AddStyle( "display: block;" )
      :cId := "result"
   END WITH

   ECHO "<hr>" INTO ::oMainPage

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WTextArea():New( ::oMainPage  )
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD FormPage() CLASS WDocMain

   LOCAL oGroup, cJs

   WITH OBJECT ::oFormPage := ::AddPage( "Page_FormPage" )
     :lHide   := .T.
     :lForced := .T.
     :lFooter := .T.
   END WITH

   WITH OBJECT ::oForm := WForm():New( ::oFormPage )
      :cMethod := "post"
      :cName := "myform"
      :lAutoComplete := .T.
      :lModal := .t.
      :AddStyle( "width: 80ch;" )
      :OnSubmit := :SubmitToService( "Srv_FormData" )
      :Create()
      WITH OBJECT oGroup := WFieldset():New( ::oForm )
         :cLegend := "Personal data"
         :cName := "User data"
         :Create()
         WITH OBJECT WNumber():New( oGroup )
            :cId := "usercode"
            :cPlaceHolder := "code"
            :cText := " (5 digits)"
            :cLabel := "User code:"
            :nMin := 1
            :nMax := 99999
            :nSize := 5
            :OnValidate := "CheckUserCode"
            :lLabelNewLine := .t.
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :cId := "username"
            :cLabel := "User name:"
            :cPlaceHolder := "user name"
            :cAutoComplete := "name"
            :nMaxLength := 50
            :nMinLength := 10
            :lLabelNewLine := .T.
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :cId := "address1"
            :cPlaceHolder := "address"
            :cAutoComplete := "address-line1"
            :cLabel := "Address:"
            :nMaxLength := 50
            :nMinLength := 10
            :lLabelNewLine := .T.
            :Create()
         END WITH
         WITH OBJECT WEmail():New( oGroup )
            :cId := "email"
            :cName := "email"
            :cPlaceHolder := "email"
            :cAutoComplete := "email"
            :cLabel := "Email:"
            :nMaxLength := 50
            :nMinLength := 10
            :lLabelNewLine := .T.
            :Create()
         END WITH
         WITH OBJECT WDateTime():New( oGroup )
            :cId := "birthday"
            :cName := "birthday"
            :cAutoComplete := "bday"
            :cLabel := "Day of birth:"
            :lLabelNewLine := .T.
            :OnValidate := "Srv_CheckDate"
            :Create()
         END WITH
         WITH OBJECT WCheckbox():New( oGroup )
            :cId := "adv"
            :cName := "adv"
            :cLabel := "You want to receive advertising from us"
            :Create()
         END WITH
         WITH OBJECT WText():New( oGroup )
            :cText := "Your actual Xailer version:<br>"
            :AddStyle("display:block;margin:10px 0px;")
            :Create()
         END WITH
         WITH OBJECT WRadioMenu():New( oGroup )
            :cId := "xailer_version"
            :cName := "xailer_version"
            :AddRadio( "None" )
            :AddRadio( "Personal" )
            :AddRadio( "Professional" )
            :AddRadio( "Enterprise" )
            :nSelected := 1
            :cMargin := "5px"
            :lHorizontal := .T.
            :Create()
         END WITH
      END WITH
      WITH OBJECT WButton():New( ::oForm )
         :cType := "submit"
         :cText := "Ok"
         :cId := "btnOk"
         :cForm := "myform"
         :Create()
      END WITH
      WITH OBJECT WButton():New( ::oForm )
         :cText := "Cancel"
         :cId := "btnCancel"
         :cType := "cancel"
         :Create()
      END WITH
      WITH OBJECT WButton():New( ::oForm )
         :cText := "Reset"
         :cId := "btnReset"
         :cType := "reset"
         :Create()
      END WITH
   END WITH

   TEXT INTO cJs
      function CheckUserCode( value, element ) {
         if (value == 0) {
            return { pass: false, error: "El código no puede ser cero" };
         }
         else {
            return { pass: true };
         }
      }

   ENDTEXT

   ::AddScript( cJs )

RETURN ::oFormPage

//------------------------------------------------------------------------------

METHOD Srv_CheckDate( hParams ) CLASS WDocMain

   LOCAL hData := { => }

   IF Empty( hParams[ 'value' ] )
      HB_HSet( hData, "pass", .F. )
      HB_HSet( hData, "error", "No puede dejar la fecha en blanco (resuelto en Harbour)" )
   ELSE
      HB_HSet( hData, "pass", .T. )
      HB_HSet( hData, "error", "" )
   ENDIF

RETURN HB_JsonEncode( hData )

//------------------------------------------------------------------------------

METHOD Srv_FormData( hParams ) CLASS WDocMain

   LOCAL hPost := Engine:hPost
   LOCAL cKey, cVal, cHtml

   cHtml := "<h1>Form values:</h1><ul>"

   FOR EACH cKey, cVal IN HB_HKeys( hPost ), HB_HValues( hPost )
      cHtml += "<li>" + cKey + ": " + cVal + "</li>"
   NEXT

   cHtml += "</ul>"

   ::oResult:cText := cHtml

RETURN nil