/*
CLASS ZCarousel FROM WDiv
   DATA nDuration    INIT 200
   DATA nDist        INIT -100
   DATA nShift       INIT 0
   DATA nPadding     INIT 0
   DATA nNumVisible  INIT 5
   DATA lFullWidth   INIT .F.
   DATA lIndicators  INIT .F.
   DATA lNoWrap      INIT .F.
   DATA cOnCycleTo   INIT ""
   DATA lCenter      INIT .F.

   METHOD AddImage( chRef, cSource )
   METHOD AddPanel( lFixed, cBkColor, cTxtColor )

   DATA aImages       INIT {}
   DATA aPanels       INIT {}
ENDCLASS
*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer
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
         :cText := "<h4>xaWeb - Materialize Carousel</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/carousel.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
      END WITH

      WITH OBJECT WCarousel():New( SO )
         :lIndicators := .t.
         :AddImage( "#one!", "https://materializeweb.com/images/placeholder/800x400_a.jpg" )
         :AddImage( "#two!", "https://materializeweb.com/images/placeholder/800x400_b.jpg" )
         :AddImage( "#three!", "https://materializeweb.com/images/placeholder/800x400_c.jpg" )
         :AddImage( "#four!", "https://materializeweb.com/images/placeholder/800x400_d.jpg" )
         :nDuration := 100
      END WITH

      WITH OBJECT WCarousel():New( SO )
         :oStyle:Height := "400px"
         :oStyle:Margin_top := "10px"
         :lIndicators := .T.
         :lCenter := .T.
         WITH OBJECT :AddPanel( .T. )
            :lCenter := .T.
            WITH OBJECT WButton():New( SO )
               :cText := "Button"
               :cColor := "white"
               :AddClass( "gray-text darken-text-2" )
               :cWaveEffect := "normal"
            END WITH
         END WITH
         WITH OBJECT :AddPanel( .F., "red", "white" )
            ECHO "<h2>First panel</h2>"
            ECHO "This is your first panel"
         END WITH
         WITH OBJECT :AddPanel( .F., "amber", "white" )
            ECHO "<h2>Second panel</h2>"
            ECHO "This is your second panel"
         END WITH
         WITH OBJECT :AddPanel( .F., "green", "white" )
            ECHO "<h2>Third panel</h2>"
            ECHO "This is your third panel"
         END WITH
         WITH OBJECT :AddPanel( .F., "blue", "white" )
            ECHO "<h2>Fourth panel</h2>"
            ECHO "This is your fourth panel"
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

