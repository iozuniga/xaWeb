
#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oFormSection, oMainSection
   DATA oForm
   DATA oResult

   METHOD CreateDoc()
   METHOD DefaultSection()
   METHOD FormSection()

   METHOD Srv_CheckDate()
   METHOD Srv_FormData()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::DefaultSection()
   ::FormSection()

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultSection() CLASS WDocMain

   LOCAL cCode, cJs

   ::oMainSection := ::AddSection( "Default" )

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

   ECHO "<h1>xaWeb - Modal form via FETCH</h1>" INTO Self

   WITH OBJECT WButton():New( ::oMainSection  )
      :cText := "Run modal form via FETCH Api"
      :OnClick := :ShowSection( "Section_FormSection" )
      :cId := "mybutton"
      :Create()
   END WITH


   WITH OBJECT ::oResult := WSpan():New( ::oMainSection )
      :cText := "Form results will be shown here (does not work on built-in Xailer navigator)"
      :AddStyle( "display: block;" )
      :cId := "result"
      :Create()
   END WITH

   ECHO "<hr>" INTO ::oMainSection

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WButton():New( ::oMainSection )
      :cText := "This button shows xaWeb source code"
      :Onclick := "showCode"
      :cId := "btnsource"
      :Create()
   END WITH

   WITH OBJECT WTextArea():New( ::oMainSection )
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

METHOD FormSection() CLASS WDocMain

   LOCAL oGroup, cJs

   WITH OBJECT ::oFormSection := ::AddSection( "Section_FormSection" )
     :lHide   := .T.
     :lDeploy := .T.
     :lFooter := .T.
   END WITH

   WITH OBJECT ::oForm := WForm():New( ::oFormSection )
      :cMethod := "post"
      :cId := "myForm"
      :lAutoComplete := .T.
      :lModal := .t.
      :AddStyle( "width: 80ch;" )
      :OnSubmit := :SubmitToService( "Srv_FormData", .f. )
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
         WITH OBJECT WSpan():New( oGroup )
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
            :lHorizontal := .T.
            :Create()
         END WITH
      END WITH
      WITH OBJECT WButton():New( ::oForm )
         :cType := "submit"
         :cText := "Ok"
         :cId := "btnOk"
         :cForm := "myForm"
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

RETURN ::oFormSection

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