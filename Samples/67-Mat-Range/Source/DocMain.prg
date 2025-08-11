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
         :cText := "<h4>xaWeb - Materialize Range & Chips & Switches</h4>"
         :Create()
      END WITH

      WITH OBJECT WFlexRow():New( SO )
         :oStyle:gap := "3em"
         WITH OBJECT WLink():New( SO )
            :cText := "Range"
            :cHref := "https://materializeweb.com/range.html"
            :Create()
         END WITH

         WITH OBJECT WLink():New( SO )
            :cText := "Chips"
            :cHref := "https://materializeweb.com/chips.html"
            :Create()
         END WITH

         WITH OBJECT WLink():New( SO )
            :cText := "Switches"
            :cHref := "https://materializeweb.com/switches.html"
            :Create()
         END WITH
      END WITH

      ECHO "<h3>HTML5 Range</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform"
         WITH OBJECT WRange():New( SO )
            :nMin := 0
            :nMax := 100
            :Create()
         END WITH
      END WITH

      ECHO "<h3>Chips</h3>"

      WITH OBJECT WDiv():New( SO )
         WITH OBJECT WChip():New( SO )
            :cText := "Jane Doe"
            :cSrcImage := "https://materializeweb.com/images/yuna.jpg"
            :lClose := .t.
         END WITH
         WITH OBJECT WChip():New( SO )
            :cText := "Tag"
            :lClose := .t.
         END WITH
         WITH OBJECT WChip():New( SO )
            :cIcon := "check"
            :cText := "Filter"
            :lClose := .t.
         END WITH
         WITH OBJECT WChip():New( SO )
            :cText := "Information"
            :lOutlined := .t.
         END WITH
         WITH OBJECT WChip():New( SO )
            :cIcon := "check"
            :cText := "Filter"
            :lClose := .t.
            :lOutlined := .t.
         END WITH
      END WITH

      ECHO "<h3>Switch</h3>"

      WITH OBJECT WDiv():New( SO )
         WITH OBJECT WSwitch():New( SO )
            :cLabelLeft := "Off"
            :cLabelRight := "On"
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

