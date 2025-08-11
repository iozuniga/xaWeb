#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oDropdown1
   LOCAL cCode, cJs

   WMaterializeContext():New( Self )

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

   ::AddScript( cJs )

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      :Create()
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
   END WITH

   WITH OBJECT oContainer := WMain():New( Self )
      :AddClass( "container" )
      WITH OBJECT WText():New( SO )
         :cText := "<h4>xaWeb - Materialize Checkbox & Radio-buttons</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/checkboxes.html"
         :Create()
      END WITH


      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/radio-buttons.html"
         :oStyle:Margin_left := "1em"
         :Create()
      END WITH

      ECHO "<h3>Checkboxes</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform"
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WCheckbox():New( SO )
               :cId := "check1"
               :cLabel := "Red"
               :Create()
            END WITH
         END WITH
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WCheckbox():New( SO )
               :cId := "check2"
               :cLabel := "Yellow"
               :lChecked := .T.
               :Create()
            END WITH
         END WITH
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WCheckbox():New( SO )
               :cId := "check3"
               :cLabel := "Green"
               :lDisabled := .T.
               :lChecked := .T.
               :Create()
            END WITH
         END WITH
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WCheckbox():New( SO )
               :cId := "check4"
               :cLabel := "Brown"
               :lDisabled := .T.
               :lChecked := .F.
               :Create()
            END WITH
         END WITH
      END WITH

      ECHO "<h3>Radio buttons</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform2"
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WRadio():New( SO )
               :cName := "group1"
               :cId := "radio1"
               :cLabel := "Red"
               :Create()
            END WITH
         END WITH
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WRadio():New( SO )
               :cName := "group1"
               :cId := "radio2"
               :cLabel := "Yellow"
               :lChecked := .F.
               :Create()
            END WITH
         END WITH
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WRadio():New( SO )
               :cName := "group1"
               :cId := "radio3"
               :cLabel := "Green"
               :lChecked := .T.
               :lWithGap := .T.
               :Create()
            END WITH
         END WITH
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WRadio():New( SO )
               :cName := "group1"
               :cId := "radio4"
               :cLabel := "Brown"
               :lDisabled := .T.
               :Create()
            END WITH
         END WITH
      END WITH


      ECHO "<h3>Radio Menu</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform3"
         WITH OBJECT WRadioMenu():New( SO )
            :cId := "xailer_version"
            :cName := "xailer_version"
            :AddRadio( "None" ):lWithGap := .T.
            :AddRadio( "Personal" )
            :AddRadio( "Professional" )
            :AddRadio( "Enterprise" )
            :nSelected := 1
            :lHorizontal := .T.
            :Create()
         END WITH
      END WITH

      FILE "DocMain.prg" INTO cCode

      ECHO "<hr>"

      WITH OBJECT WButton():New( SO )
         :cText := "This button shows xaWeb source code"
         :Onclick := "showCode"
         :cId := "btnsource"
         :cDisplayType := "filled"
         :Create()
      END WITH
      WITH OBJECT WTextArea():New( SO )
         :oStyle:Margin_top := "20px"
         :oContext:WavesEffect( .T., .T. )
         :AddClass( "materialize-textarea" )
         :nCols := 80
         :cId   := "source"
         :nRows := 40
         :AddDataset( "text", HB_Base64Encode( cCode ) )
         :lReadOnly := .t.
         :lVisible := .f.
         :oStyle:Font_family := "monospace"
         :Create()
      END WITH
   END WITH

RETURN nil

