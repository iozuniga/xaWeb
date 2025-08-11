
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
         :cText := "<h3>xaWeb - Materialize Cards</h3>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/cards.html"
      END WITH

      ECHO "<p></p>"

      WITH OBJECT WDiv():New( SO )
         :oContext:Row()
         :AddStyle( "gap: 20px;" )
         WITH OBJECT WCard():New( SO )
            :oContext:Col( 12, 6, 4 )
            :oTitle:cText := "Card Title"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link" )
            :AddAction( "This is a link" )
         END WITH
         WITH OBJECT WCard():New( SO )
            :oContext:Col( 12, 6, 4 )
            :oTitle:cText := "Card Title"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link" )
            :AddAction( "This is a link" )
         END WITH
         WITH OBJECT WCard():New( SO )
            :oContext:Col( 12, 6, 4 )
            :oTitle:cText := "Card Title"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link" )
            :AddAction( "This is a link" )
         END WITH
      END WITH

      ECHO "<p></p>"

      WITH OBJECT WFlexRow():New( SO )
         :cJustifyContent := "flex-start"
         :AddStyle( "gap: 20px;" )
         WITH OBJECT WCard():New( SO )
            :oTitle:cText := "Card Title with Image"
            :oImage:cSrc := "https://materializeweb.com/images/sample-1.jpg"
            :oImage:oStyle:Max_Width := "100%"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link", "#" )
            :AddAction( "This is a link", "#" )
         END WITH
         WITH OBJECT WCard():New( SO )
            :oTitle:cText := "Card Title with Image & button"
            :oImage:cSrc := "https://materializeweb.com/images/sample-1.jpg"
            :oImage:oStyle:Max_Width := "100%"
            :oButton:cIcon := "add"
            :lTitleOnImage := .t.
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link", "#" )
            :AddAction( "This is a link", "#" )
         END WITH
      END WITH

      ECHO "<p></p>"

      WITH OBJECT WFlexRow():New( SO )
         :cJustifyContent := "flex-start"
         :AddStyle( "gap: 20px;" )
         WITH OBJECT WCard():New( SO )
            :lHorizontal := .t.
            :lStacked := .T.
            :oTitle:cText := "Card Title with Image horizontal"
            :oImage:cSrc := "https://materializeweb.com/images/sample-1.jpg"
            :oImage:oStyle:Max_Width := "100%"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link", "#" )
            :AddAction( "This is a link", "#" )
         END WITH
      END WITH

      ECHO "<p></p>"

      WITH OBJECT WFlexRow():New( SO )
         :cJustifyContent := "flex-start"
         :AddStyle( "gap: 20px;" )
         WITH OBJECT WCard():New( SO )
            :lReveal := .T.
            :oTitle:cText := "Card expanded"
            :cRevealTitle := "Card collapsed"
            :cRevealText  := "Here is some more information about this product that is only revealed once clicked on."
            :oImage:cSrc  := "https://materializeweb.com/images/sample-1.jpg"
            :oText:cText  := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link", "#" )
            :AddAction( "This is a link", "#" )
         END WITH
         WITH OBJECT WCard():New( SO )
            :lReveal := .T.
            :oTitle:cText := "Card expanded"
            :cRevealTitle := "Card collapsed"
            :cRevealText  := "Here is some more information about this product that is only revealed once clicked on."
            :oImage:cSrc  := "https://materializeweb.com/images/sample-1.jpg"
            :oText:cText  := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link", "#" )
            :AddAction( "This is a link", "#" )
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

