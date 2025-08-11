/*
CLASS ZSidenav FROM WDiv
   DATA oHeader            AS CLASS WSidenavHeader
   DATA cEdge              INIT "" VALUES "left", "right", "top", "bottom"
   DATA nInDuration
   DATA nOutDuration
   DATA nDragTargetWidth
   DATA lDraggable
   DATA lPreventScrolling
   DATA lFixed

   DATA OnOpenStart
   DATA OnOpenEnd
   DATA OnCloseStart
   DATA OnCloseEnd

   METHOD AddHeader( cImage )
   METHOD AddItem( cText, cHref, cIcon )
   METHOD AddDivider()
   METHOD AddSubHeader( cText )
   METHOD SlideButton( oParent, cText, lShowOnLarge )
ENDCLASS

CLASS ZSidenavHeader FROM WDiv
   DATA oDivMain      AS CLASS WDiv
   DATA oDivImage     AS CLASS WDiv
   DATA oBackground   AS CLASS WImage

   METHOD AddItem( cText, cHref )
   METHOD AddImage( cSource, cHref )
   METHOD AddIcon( cIcon, cHref )
ENDCLASS

CLASS ZSidenavItem FROM WDiv
   DATA oLink        AS CLASS WLink
   DATA oIcon        AS CLASS WIconGoogle  // Only available at Preprocess event
   DATA oSpan        AS CLASS WSpan        // Only available at Preprocess event
   DATA cIcon        INIT ""
   DATA cHref        INIT ""
   DATA cWaveEffect  INIT "" VALUES "", "normal", "light"
   DATA lCloseNav    INIT .F.
ENDCLASS

*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oSidenav, oBtn
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
         :cText := "<h3>xaWeb - Materialize Sidenav</h3>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/sidenav.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
      END WITH

      WITH OBJECT oSidenav := WSidenav():New( SO )
         :cId := "sidenav"
         :nOutDuration := 100
         WITH OBJECT :AddHeader( "https://materializeweb.com/images/office.jpg" )
            :AddImage( "https://materializeweb.com/images/yuna.jpg" )
            :AddItem( "John Doe" ):AddClass( "white-text name" )
            :AddItem( "jdoe@example.com" ):AddClass( "white-text email" )
         END WITH
         :AddItem( "First Link with Icon", "#", "cloud" )
         :AddItem( "Second Link" ):lCloseNav := .t.
         :AddDivider()
         :AddSubheader( "Subheader" )
         :AddItem( "Third Link with waves" ):cWaveEffect := "normal"
      END WITH

      oBtn := oSidenav:SlideButton( SO, "Sidenav demo" )

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

