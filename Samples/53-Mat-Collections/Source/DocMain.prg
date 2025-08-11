/*
CLASS ZCollection FROM WDiv
   METHOD AddHeader( cText, nSize )
   METHOD AddItem( cText )
ENDCLASS

CLASS ZCollectionItem FROM WDiv
   DATA cSrcImage  INIT ""
   DATA cIconColor INIT ""
   DATA cIcon     INIT ""
   DATA cTitle    INIT ""
   DATA cText     INIT ""
   DATA cHRef     INIT ""
   DATA lHeader   INIT .F.
   DATA lActive   INIT .F.

   DATA oDiv         // Only available at Preprocess event
   DATA oAvatar      // Only available at Preprocess event
   DATA oTitle       // Only available at Preprocess event
   DATA oSubTitle    // Only available at Preprocess event
   DATA oLink        // Only available at Preprocess event
   DATA oIcon        // Only available at Preprocess event
ENDCLASS
*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs, oStyle

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


   WITH OBJECT WDiv():New( Self )
      :AddClass( "container" )
      WITH OBJECT WText():New( SO )
         :cText := "<h3>xaWeb - Materialize Collections</h3>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/collections.html"
      END WITH

      ECHO "<h2>Basic</h2>"

      WITH OBJECT WCollection():New( SO )
         :AddItem( "Alvin" )
         :AddItem( "Alvin" )
         :AddItem( "Alvin" )
         :AddItem( "Alvin" )
         :Create()
      END WITH

      ECHO "<h2>Links</h2>"

      WITH OBJECT WCollection():New( SO )
         :AddItem( "Alvin" ):cHRef := "#"
         WITH OBJECT :AddItem( "Alvin" )
            :cHRef := "#"
            :lActive := .T.
         END WITH
         :AddItem( "Alvin" ):cHRef := "#"
         :AddItem( "Alvin" ):cHRef := "#"
         :Create()
      END WITH

      ECHO "<h2>Headers</h2>"

      WITH OBJECT WCollection():New( SO )
         :AddHeader( "First Names", 4 )
         :AddItem( "Alvin" )
         :AddItem( "Alvin" )
         :AddItem( "Alvin" )
         :AddItem( "Alvin" )
         :Create()
      END WITH

      ECHO "<h2>Secondary content</h2>"

      WITH OBJECT WCollection():New( SO )
         :AddHeader( "First Names", 4 )
         WITH OBJECT :AddItem( "Alvin" )
            :cSecHRef := "#"
            :cSecIcon := "send"
         END WITH
         WITH OBJECT :AddItem( "Alvin" )
            :cSecHRef  := "#"
            :cSecIcon := "send"
         END WITH
         WITH OBJECT :AddItem( "Alvin" )
            :cSecHRef := "#"
            :cSecIcon := "send"
         END WITH
         WITH OBJECT :AddItem( "Alvin" )
            :cSecHRef := "#"
            :cSecIcon := "send"
         END WITH
         :Create()
      END WITH

      ECHO "<h2>Avatar content</h2>"

      WITH OBJECT WCollection():New( SO )
         WITH OBJECT :AddItem( "First line Second line" )
            :cSrcImage := "https://materializeweb.com/images/yuna.jpg"
            :cTitle    := "Title"
            :cSecHRef  := "#!"
            :cSecIcon := "grade"
         END WITH
         WITH OBJECT :AddItem( "First line Second line" )
            :cIcon     := "folder"
            :cTitle    := "Title"
            :cSecHRef  := "#!"
            :cSecIcon := "grade"
         END WITH
         WITH OBJECT :AddItem( "First line Second line" )
            :cIcon      := "insert_chart"
            :cIconColor := "green"
            :cTitle    := "Title"
            :cSecHRef  := "#!"
            :cSecIcon := "grade"
         END WITH
         WITH OBJECT :AddItem( "First line Second line" )
            :cIcon      := "play_arrow"
            :cIconColor := "red"
            :cTitle    := "Title"
            :cSecHRef  := "#!"
            :cSecIcon := "grade"
         END WITH
         :Create()
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

