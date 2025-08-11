#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   DATA oCon AS CLASS WMaterializeContext

   METHOD CreateDoc()
   METHOD Section_Results( hPost )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL oDiv AS CLASS WDiv
   LOCAL oFrm AS CLASS WForm
   LOCAL oBtn AS CLASS WButton
   LOCAL cCode, cJs, cResfile

//   Engine:lDebug := .t.

   ::oCon := WMaterializeContext():New( Self )

   // This simple Javascript function just shows the source code on a hidden TextArea

   TEXT INTO cJs
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

   FILE "DocMain.prg"         INTO cCode
   FILE "..\OrderForm.xares"  INTO cResfile

   ::AddScript( cJs )

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      :Create()
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
   END WITH

   IF ::IsDefault()
      WITH OBJECT oDiv := WDiv():New( Self )
         :AddClass( "container" )
         WITH OBJECT WFormManager():New( cResfile )
            IF Empty( :cError )
               oFrm := :DeployForm( "", oDiv ) // Loads first form in resources
               IF !HB_IsObject( oFrm )
                  ECHO "Form Manager Error: (" + :cError + ")" INTO oDiv
               ENDIF
            ELSE
               ECHO "Error on WFormManager constructor: (" + :cError + ")" INTO oDiv
            ENDIF
            :End()
         END WITH
      END WITH
   ENDIF

   IF HB_IsObject( oFrm )
      oFrm:cName := "Section_results"
      oFrm:cMethod := "post"
      oFrm:lAutoComplete := .t.
      oBtn := oFrm:ControlById( "btnOK" )
      IF HB_IsObject( oBtn )
         oBtn:cType := "submit" // Maybe already on the resource file
      ELSE
         LogDebug( "button OK not found on resource file" )
      ENDIF
   ENDIF

   ECHO "<hr>" INTO Self

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
      :AddDataset( "text", HB_Base64Encode( cCode ) )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Section_Results( hPost ) CLASS WDocMain

   LOCAL oSection
   LOCAL cKey, xValue

   oSection := Document:AddSection( "results" )

   WITH OBJECT WDiv():New( oSection )
      :AddClass( "container" )
      ECHO "<h1>xaWeb - Mat-Form from resources</h1>"
      ECHO "<p>This are the inputs from your form:</p>"
      FOR EACH cKey, xValue IN HB_HKeys( hPost ), HB_HValues( hPost )
         IF HB_IsString( xValue )
            ECHO cKey + ": " + xValue + "<br>"
         ELSE
            ECHO cKey + ": " + xValue:cName + "<br>"
            xValue:Save()
         ENDIF
      NEXT
   END WITH

RETURN oSection

//------------------------------------------------------------------------------


