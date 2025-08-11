// https://test.ozs.es/cgi-bin/xaWeb/simpleform.cgi

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()
   METHOD Frm_MyForm( hPost )
   METHOD Srv_CheckDate( hParams )

END CLASS

//------------------------------------------------------------------------------

METHOD Frm_MyForm( hPost ) CLASS WDocMain

   LOCAL oSection
   LOCAL cKey, cValue

   oSection := Document:AddSection( "results" )

   WITH OBJECT WDiv():New( oSection )
      ECHO "<h1>xaWeb - Forms</h1>"
      ECHO "<p>This are the inputs from your form:</p>"
      FOR EACH cKey, cValue IN HB_HKeys( hPost ), HB_HValues( hPost )
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

METHOD CreateDoc() CLASS WDocMain

   LOCAL oForm, oGroup
   LOCAL cCode, cJs

   WITH OBJECT WMaterializeContext():New( Self )
      :cLanguage := "es"
   END WITH

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      :Create()
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
   END WITH

   ::AddCSS( ".input-field { margin:1em 0em;}" )

   WITH OBJECT oForm := WForm():New( Document )
      :AddClass( "container" )
      :cMethod := "post"
      :cName := "Frm_myform"
      :lAutoComplete := .T.
      :Create()
      WITH OBJECT oGroup := WFieldset():New( oForm )
         :cLegend := "Personal data"
         :cName := "User data"
         :Create()
         WITH OBJECT WNumber():New( oGroup )
            :lOutlined := .t.
            :cId := "usercode"
            :cLabel := "User code:"
            :nMin := 0
            :nMax := 99999
            :nSize := 5
            :OnValidate := "CheckUserCode"
            :cSupport := ""
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :lOutlined := .t.
            :cId := "username"
            :cLabel := "User name:"
            :cAutoComplete := "name"
            :nMaxLength := 50
            :nMinLength := 10
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :lOutlined := .t.
            :cId := "address1"
            :cPlaceHolder := "address"
            :cAutoComplete := "address-line1"
            :cLabel := "Address:"
            :nMaxLength := 50
            :nMinLength := 10
            :Create()
         END WITH
         WITH OBJECT WEmail():New( oGroup )
            :lOutlined := .t.
            :cId := "email"
            :cName := "email"
            :cPlaceHolder := "email"
            :cAutoComplete := "email"
            :cLabel := "Email:"
            :nMaxLength := 50
            :nMinLength := 10
            :Create()
         END WITH
         WITH OBJECT WDatePicker():New( oGroup )
            :lOutlined := .t.
            :cId := "birthday"
            :cName := "birthday"
            :cAutoComplete := "bday"
            :cLabel := "Day of birth:"
            :OnValidate := "Srv_CheckDate"
            :Create()
            :oStyle:Margin_bottom := "1rem"
         END WITH
         WITH OBJECT WCheckbox():New( oGroup )
            :cId := "adv"
            :cName := "adv"
            :cLabel := "You want to receive advertising from us"
            :Create()
         END WITH
         WITH OBJECT WSpan():New( oGroup )
            :cText := "Your actual Xailer version:<br>"
            :oStyle:Display := "block"
            :oStyle:Margin := "10px 0px"
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
            :lHorizontal := .f.
            :Create()
         END WITH
      END WITH
      WITH OBJECT WButton():New( oForm )
         :oStyle:Margin_top := "2em"
         :cType := "submit"
         :cText := "Submit"
         :cId := "button"
         :Create()
      END WITH
   END WITH

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
            ele.value = xa_b64toUnicode(ele.dataset.text);
            ele.hidden = false;
            document.getElementById("btnsource").style.display = "none";
            M.Forms.textareaAutoResize(ele);
         }
      }
   ENDTEXT

   ::AddScript( cJs )

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
      :cText := ""//HB_Base64Encode( cCode )
      :AddDataset("text", HB_Base64Encode( cCode ) )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

