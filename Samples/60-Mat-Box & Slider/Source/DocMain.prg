/*
CLASS ZMaterialBox FROM WImage
   DATA cCaption        INIT ""
   DATA nInDuration     INIT 275
   DATA nOutDuration    INIT 200
   DATA cOnOpenStart    INIT ""
   DATA cOnOpenEnd      INIT ""
   DATA cOnCloseStart   INIT ""
   DATA cOnCloseEnd     INIT ""
ENDCLASS

CLASS ZSlider FROM WDiv
PUBLISHED:
   DATA oContainer      AS CLASS WDiv
   DATA aItems          INIT {} AS CLASS WSliderItem
   DATA nDuration       INIT 500
   DATA nHeight         INIT 400
   DATA nInterval       INIT 6000
   DATA lIndicators     INIT .T.
   DATA lPauseOnFocus   INIT .T.
   DATA lPauseOnHover   INIT .T.
   METHOD AddItem()
ENDCLASS

CLASS ZSliderItem FROM WDiv
   DATA cSrc         INIT ""
   DATA cTitle       INIT ""
   DATA cText        INIT ""
   DATA cAlignment   INIT "center" VALUES "center", "left", "right"
   DATA oImage       AS CLASS WImage
   DATA oDiv         AS CLASS WDiv
   DATA oTitle       AS CLASS WText
   DATA oText        AS CLASS WText
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
         :cText := "<h4>xaWeb - Materialize Box & Slider</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/media.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
      END WITH

      WITH OBJECT WMaterialBox():New( SO )
         :cSrc := "https://materializeweb.com/images/placeholder/800x400_d.jpg"
         :nWidth := 250
         :cCaption := "A picture of a way with a group of trees in a park"
         :cAlt := "Sample image for Material Box with caption"
      END WITH

      ECHO "<br>"

      WITH OBJECT WSlider():New( SO )
         WITH OBJECT :AddItem()
            :cSrc   := "https://materializeweb.com/images/placeholder/800x400_a.jpg"
            :cTitle := "This is our big Tagline!"
            :cText  :=  "Here's our small slogan."
         END WITH
         WITH OBJECT :AddItem()
            :cAlignment := "left"
            :cSrc       := "https://materializeweb.com/images/placeholder/800x400_b.jpg"
            :cTitle     := "Left align caption"
            :cText      :=  "Here's our small slogan."
         END WITH
         WITH OBJECT :AddItem()
            :cAlignment := "right"
            :cSrc       := "https://materializeweb.com/images/placeholder/800x400_c.jpg"
            :cTitle     := "Right align caption"
            :cText      :=  "Here's our small slogan."
         END WITH
         WITH OBJECT :AddItem()
            :cAlignment := "center"
            :cSrc       := "https://materializeweb.com/images/placeholder/800x400_d.jpg"
            :cTitle     := "Center align caption"
            :cText      :=  "Here's our small slogan."
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

