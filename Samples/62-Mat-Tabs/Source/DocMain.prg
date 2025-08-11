/*
CLASS ZTabs FROM WDiv
   DATA aItems                INIT {} AS CLASS WTabsItem
   DATA nDuration             INIT 300
   DATA cOnShow               INIT ""
   DATA lSwipeable            INIT .F.
   DATA lFixedWidth           INIT .F.
   DATA nResponsiveThreshold  INIT 0
   METHOD AddItem( cText, cHref )
ENDCLASS

CLASS ZTabsItem FROM WDiv
   DATA cText        INIT ""
   DATA cHRef        INIT ""
   DATA lDisabled    INIT .F.
   DATA lActive      INIT .F.
   DATA oLink        AS CLASS WLink
ENDCLASS
*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oTabs1, oTabs2
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
         :cText := "<h3>xaWeb - Materialize Tabs</h3>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/tabs.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
      END WITH

      ECHO "<h4>Standard</h4>"

      WITH OBJECT oTabs1 := WTabs():New( SO )
         :AddItem( "Test1", "#test1" )
         :AddItem( "Test2", "#test2" ) //:lActive := .T.
         :AddItem( "Test3", "#test3" )
         :AddItem( "Test4", "#test4" )
      END WITH

      ECHO "<p></p>"

      WITH OBJECT WDiv():New( SO )
         :cId := "test1"
         ECHO "test1"
      END WITH

      WITH OBJECT WDiv():New( SO )
         :cId := "test2"
         ECHO "test2"
      END WITH
      WITH OBJECT WDiv():New( SO )
         :cId := "test3"
         ECHO "test3"
      END WITH
      WITH OBJECT WDiv():New( SO )
         :cId := "test4"
         ECHO "test4"
      END WITH

      ECHO "<h4>Swipeable</h4>"

      WITH OBJECT oTabs2 := WTabs():New( SO )
         :lSwipeable := .T.
         :AddItem( "Test1", "#test-swipe-1" )
         :AddItem( "Test2", "#test-swipe-2" )
         :AddItem( "Test3", "#test-swipe-3" )
      END WITH

      WITH OBJECT WDiv():New( SO )
         :oStyle:Height := "400px"
         WITH OBJECT WDiv():New( SO )
            :cId := "test-swipe-1"
            :AddClass( "blue" )
            ECHO "test1"
         END WITH
         WITH OBJECT WDiv():New( SO )
            :cId := "test-swipe-2"
            :AddClass( "red" )
            ECHO "test2"
         END WITH
         WITH OBJECT WDiv():New( SO )
            :cId := "test-swipe-3"
            :AddClass( "green" )
            ECHO "test3"
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

