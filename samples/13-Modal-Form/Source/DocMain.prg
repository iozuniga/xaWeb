// https://test.Oz Software/cgi-bin/xailerweb/modalform.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oForm

   METHOD CreateDoc()
   METHOD Act_RunForm()
   METHOD Srv_CheckDate( hParams )
   METHOD Frm_MyForm( hParams )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<h1>XailerWeb - Modal form</h1>" INTO Self

   WITH OBJECT WButton():New( Self )
      :cText   := "Run modal form"
      :OnClick := "Act_RunForm"
      :cId     := "mybutton"
   END WITH

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WTextArea():New( Self )
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :AddStyle( "max-width: 90%; margin: 0px auto; display:block;" )
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_RunForm() CLASS WDocMain

   LOCAL oGroup, cJs

   WITH OBJECT ::oForm := WForm():New( Document )
      :cMethod := "post"
      :cName := "Frm_Myform"
      :lAutoComplete := .T.
      :lModal := .t.
      :AddStyle( "width: 80ch;max-width: 100%;" )
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
         :Create()
      END WITH
      WITH OBJECT WButton():New( ::oForm )
         :cText := "Cancel"
         :cId := "btnCancel"
         :cType := "cancel"
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

RETURN nil

//------------------------------------------------------------------------------

METHOD Frm_MyForm( hParams ) CLASS WDocMain

   LOCAL oPage
   LOCAL cKey, cValue

   oPage := Document:AddPage( "results" )

   WITH OBJECT WDiv():New( oPage )
      ECHO "<h1>XailerWeb</h1>"
      ECHO "<p>This are the inputs from your form:</p>"
      FOR EACH cKey, cValue IN HB_HKeys( hParams ), HB_HValues( hParams )
         ECHO cKey + ": " + cValue + "<br>"
      NEXT
   END WITH

RETURN oPage

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

