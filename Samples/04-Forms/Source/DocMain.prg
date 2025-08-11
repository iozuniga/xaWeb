#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()
   METHOD Section_Form()
   METHOD Section_Results( hPost )
   METHOD Srv_CheckDate( hParams )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

//   Engine:lDebug := .t.

   IF ::IsDefault()
      ::Section_Form()
   ENDIF

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

METHOD Section_Form() CLASS WDocMain

   LOCAL oForm, oGroup
   LOCAL cJs, cCss

   TEXT INTO cCss
      @media screen and (width > 600px) {
         form {
            width: 600px;
         }
      }
   ENDTEXT

   ::AddCSS( cCss )

   TEXT INTO cJs
      function CheckUserCode( value ) {
         if (value == 0) {
            return { pass: false, error: "El código no puede ser cero" };
         }
         else {
            return { pass: true };
         }
      }

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

   WITH OBJECT oForm := WForm():New( Document )
      :cMethod := "post"
      :cName := "Section_results"
      :cEncType := "multipart/form-data"
      :lAutoComplete := .T.
      :Create()
      WITH OBJECT oGroup := WFieldset():New( oForm )
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
         WITH OBJECT WParagraph():New( oGroup )
            :cText := "Your actual Xailer version:"
            :oStyle:Display := "block"
            :oStyle:Margin := "10px 0px"
            :Create()
         END WITH
         WITH OBJECT WRadioMenu():New( oGroup )
            :cId := "xailer_version"
            :cName := "xailer_version"
            :aItems := { "None", "Personal", "Professional", "Enterprise" }
//            :AddRadio( "None" )
//            :AddRadio( "Personal" )
//            :AddRadio( "Professional" )
//            :AddRadio( "Enterprise" )
            :nSelected := 1
            :lHorizontal := .f.
            :Create()
         END WITH
         WITH OBJECT WFile():New( oGroup )
            :cName := "myFile"
         END WITH
      END WITH
      WITH OBJECT WButton():New( oForm )
         :cType := "submit"
         :cText := "Submit"
         :cId := "button"
         :Create()
      END WITH
   END WITH

RETURN nil

//------------------------------------------------------------------------------

/*
Note: This section is shown before the code on CreateDoc(), before we did not
create a 'default' section on that method
*/

METHOD Section_Results( hPost ) CLASS WDocMain

   LOCAL oSection
   LOCAL cKey, cValue

   oSection := Document:AddSection( "results" )

   WITH OBJECT WDiv():New( oSection )
      ECHO "<h1>xaWeb - Forms</h1>"
      ECHO "<p>This are the inputs from your form:</p>"
      FOR EACH cKey, cValue IN HB_HKeys( hPost ), HB_HValues( hPost )
         IF HB_IsString( cValue )
            ECHO cKey + ": " + cValue + "<br>"
         ELSE // is a a WPostFile object
            ECHO cKey + ": " + cValue:cName + "<br>"
            cValue:Save()
         ENDIF
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
