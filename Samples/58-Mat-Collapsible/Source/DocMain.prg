/*
CLASS ZCollapsible FROM WDiv
   DATA aPanels       INIT {}
   DATA cType           INIT "" VALUES "accordion", "expandable", "popout"
   DATA nInDuration     INIT 300
   DATA nOutDuration    INIT 300
   DATA cOnOpenStart    INIT ""
   DATA cOnOpenEnd      INIT ""
   DATA cOnCloseStart   INIT ""
   DATA cOnCloseEnd     INIT ""

   METHOD AddPanel( cTitle, cIcon, cText )
ENDCLASS

CLASS ZCollapsiblePanel FROM WDiv
   DATA cTitle       INIT ""
   DATA cIcon        INIT ""
   DATA cText        INIT ""
   DATA lActive      INIT .F.

   DATA oHeader      AS CLASS WDiv
   DATA oBody        AS CLASS WDiv
   DATA oIcon        AS CLASS WIconGoogle
   DATA oHeadSpan    AS CLASS WSpan
   DATA oBodySpan    AS CLASS WSpan
ENDCLASS
*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer
   LOCAL cCode, cJs, c1, c2

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

   TEXT INTO c1
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
   ENDTEXT

   TEXT INTO c2
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
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
         :cText := "<h4>xaWeb - Materialize Collapsible</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/collapsible.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
      END WITH

      ECHO "<h4>Collapsible</h4>"

      WITH OBJECT WCollapsible():New( SO )
         :AddPanel( "First", "filter_drama", c1 )
         :AddPanel( "Second", "place", c2 )
         :AddPanel( "Third", "whatshot", c2 )
      END WITH

      ECHO "<h4>Expandable</h4>"

      WITH OBJECT WCollapsible():New( SO )
         :cType := "expandable"
         :AddPanel( "First", "filter_drama", c1 )
         :AddPanel( "Second", "place", c2 )
         :AddPanel( "Third", "whatshot", c2 )
      END WITH

      ECHO "<h4>Popout</h4>"

      WITH OBJECT WCollapsible():New( SO )
         :cType := "popout"
         :AddPanel( "First", "filter_drama", c1 )
         :AddPanel( "Second", "place", c2 )
         :AddPanel( "Third", "whatshot", c2 )
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

