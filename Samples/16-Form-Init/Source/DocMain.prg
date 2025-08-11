#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oForm

   METHOD CreateDoc()
   METHOD Act_RunForm()
   METHOD Srv_CheckDate( hParams )
   METHOD Frm_MyForm( hParams )
   METHOD Srv_FrmInit( hParams )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
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

   Engine:lDebug := .T.

   ::AddScript( cJs )

   ECHO "<h1>xaWeb - Form init</h1>" INTO Self

   ECHO "<p>This sample show how to initialize the input fields values with a service event. Does not work on Xailer built-in navigator</p>" INTO Self

   WITH OBJECT WButton():New( Self )
      :cText   := "Form init"
      :OnClick := "Act_RunForm"
      :cId     := "mybutton"
      :Create()
   END WITH

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WButton():New( Self )
      :cText := "This button shows xaWeb source code"
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

METHOD Act_RunForm() CLASS WDocMain

   LOCAL oGroup, cJs

   WITH OBJECT ::oForm := WForm():New( Document )
      :cMethod := "post"
      :cName := "Frm_Myform"
      :cId := "Frm_Myform"
      :lAutoComplete := .T.
      :lModal := .t.
      :AddStyle( "width: 80ch;max-width: 100%;" )
      :onFormShow := "Srv_FrmInit"
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
            :cValue := "ignacio"
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
         WITH OBJECT WParagraph():New( oGroup )
            :cText := "Your actual Xailer version:"
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

   LOCAL oSection
   LOCAL cKey, cValue

   oSection := Document:AddSection( "results" )

   WITH OBJECT WDiv():New( oSection )
      ECHO "<h1>xaWeb</h1>"
      ECHO "<p>This are the inputs from your form:</p>"
      FOR EACH cKey, cValue IN HB_HKeys( hParams ), HB_HValues( hParams )
         ECHO cKey + ": " + cValue + "<br>"
      NEXT
   END WITH

RETURN oSection

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
// If you do not need to change anything, is better to return an empty json.

METHOD Srv_FrmInit( hParams )

   LOCAL hCargo := Engine:hCargo

   HB_HSet( hCargo, "username", "perico" )

RETURN HB_JsonEncode( hCargo )

//------------------------------------------------------------------------------

