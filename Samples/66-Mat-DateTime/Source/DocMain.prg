/*
*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oDropdown1
   LOCAL cCode, cJs

   WITH OBJECT WMaterializeContext():New( Self )
      :cLanguage := "es"
   END WITH

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
         :cText := "<h4>xaWeb - Materialize Date & Time Pickers</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/pickers.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "2em"
         :Create()
      END WITH

      ECHO "<h3>Date Picker</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform"
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WDateTime():New( SO )
               :cId := "datetime1"
               :cLabel := "Birthdate: (legacy control)"
               :Create()
            END WITH
            WITH OBJECT WDatePicker():New( SO )
               :cId := "datepicker1"
               :cLabel := "Birthdate:"
               :Create()
            END WITH
         END WITH
      END WITH

      ECHO "<h3>Time Picker</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform2"
         WITH OBJECT WParagraph():New( SO )
            WITH OBJECT WDateTime():New( SO )
               :cType := "time"
               :cId := "time1"
               :cLabel := "Lunch time: (legacy control)"
               :Create()
            END WITH
            WITH OBJECT WTimePicker():New( SO )
               //:lTwelveHour := .T.
               :cId := "timepicker1"
               :cLabel := "Lunch time:"
               :Create()
            END WITH
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

