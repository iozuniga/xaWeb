/*
CLASS ZSelect
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA cLabel          INIT ""
   DATA cPlaceHolder    INIT ""
   DATA lOutlined       INIT .F.
   DATA lMultiple       INIT .F.
   DATA lBrowserDefault INIT .F.
   DATA cValue          INIT "" PERSISTENT
   DATA cForm           INIT ""
   DATA cName           INIT ""
   DATA nSize           INIT NIL
   DATA lAutofocus      INIT .F.
   DATA lDisabled       INIT .F.
   DATA lRequired       INIT .F.
ENDCLASS

CLASS ZSelectItem
PUBLISHED:
   DATA aItems       INIT {} AS CLASS WSelectItem
   DATA cText        INIT ""
   DATA cLabel       INIT ""
   DATA cImage       INIT ""
   DATA cValue       INIT ""
   DATA lDisabled    INIT .F.
   DATA lSelected    INIT .F.
ENDCLASS

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
         :cText := "<h4>xaWeb - Materialize Select</h4>"
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/select.html"
         :Create()
      END WITH

      ECHO "<h3>Select</h3>"

      WITH OBJECT WForm():New( SO )
         :oContext:Col( 12 )
         :cName := "form1"
         WITH OBJECT WDiv():New( SO )
            :AddClass( "row" )
            WITH OBJECT WSelect():New( SO )
               :cId := "select-example1"
               :oContainer:oContext:Col( 12, 6 )
               :lOutlined := .T.
               :cLabel := "Materialize Select"
               :Create()
               WITH OBJECT :AddItem( "0", "Choose your option" )
                  :lDisabled := .T.
                  :lSelected := .T.
               END WITH
               :AddItem( "1", "Option 1" )
               :AddItem( "2", "Option 2" )
               :AddItem( "3", "Option 3" )
            END WITH
            WITH OBJECT WSelect():New( SO )
               :cId := "select-example2"
               :oContainer:oContext:Col( 12, 6 )
               :lMultiple := .T.
               :cLabel := "Materialize Multiple Select"
               :Create()
               WITH OBJECT :AddItem( "00", "Choose your option" )
                  :lDisabled := .T.
                  :lSelected := .T.
               END WITH
               :AddItem( "1", "Option 1" )
               :AddItem( "2", "Option 2" )
               :AddItem( "3", "Option 3" )
            END WITH
            WITH OBJECT WSelect():New( SO )
               :cId := "select-example3"
               :oContainer:oContext:Col( 12, 6 )
               :cLabel := "Optgroups"
               :Create()
               WITH OBJECT :AddGroup( "team 1" )
                  :AddItem( "1", "Option 1" )
                  :AddItem( "2", "Option 2" )
               END WITH
               WITH OBJECT :AddGroup( "team 2" )
                  :AddItem( "3", "Option 3" )
                  :AddItem( "4", "Option 4" )
               END WITH
            END WITH
            WITH OBJECT WSelect():New( SO )
               :cId := "select-example4"
               :oContainer:oContext:Col( 12, 6 )
               :cLabel := "Images in Select"
               :Create()
               WITH OBJECT :AddItem( "00", "Choose your option" )
                  :lDisabled := .T.
                  :lSelected := .T.
               END WITH
               :AddItem( "1", "example 1", "https://materializeweb.com/images/sample-1.jpg" )
               :AddItem( "2", "example 2", "https://materializeweb.com/images/office.jpg" )
               :AddItem( "3", "example 3", "https://materializeweb.com/images/yuna.jpg" )
            END WITH

         WITH OBJECT WDiv():New( SO )
            :oContext:Col( 12, 6 )
            WITH OBJECT WLabel():New( SO )
               :cText := "Browser Select"
               :cFor := "select-exmple5"
            END WITH
            WITH OBJECT WSelect():New( SO )
               :cId := "select-example5"
               :lBrowserDefault := .T.
               :Create()
               WITH OBJECT :AddItem( "00", "Choose your option" )
                  :lDisabled := .T.
                  :lSelected := .T.
               END WITH
               :AddItem( "1", "Option 1" )
               :AddItem( "2", "Option 2" )
               :AddItem( "3", "Option 3" )
            END WITH
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

