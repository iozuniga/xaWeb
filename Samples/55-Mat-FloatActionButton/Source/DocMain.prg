
/*
CLASS ZFloatActionButton FROM WDiv
   DATA cDirection      INIT "" VALUES "top", "right", "bottom", "left"
   DATA cIcon           INIT ""
   DATA cColor          INIT ""
   DATA lLarge          INIT .T.
   DATA lHoverEnabled   INIT NIL
   DATA lToolbarEnabled INIT NIL

   METHOD AddItem( chRef, cIcon, cColor )

PUBLIC:
   DATA oUl           AS CLASS WDiv
   DATA oLink         AS CLASS WLink
   DATA oIcon         AS CLASS WIconGoogle
ENDCLASS

*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oButton
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
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
      :Create()
   END WITH


   WITH OBJECT oContainer := WMain():New( Self )
      :AddClass( "container" )
      WITH OBJECT WText():New( SO )
         :cText := "<h4>xaWeb - Materialize Floating action button and footer</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/floating-action-button.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
      END WITH

      WITH OBJECT oButton := WFloatActionButton():New( SO )
         :cIcon  := "mode_edit"
         :lLarge := .T.
         WITH OBJECT :AddItem( "#", "insert_chart" ):oContext:oTooltip( "Insert chart" )
            :cPosition := "left"
            :cIcon := "insert_chart"
            :ltextAfter := .t.
         END WITH
         :AddItem( "#", "format_quote", "yellow darken-1" ):oContext:oTooltip( "format" ):cPosition := "left"
         :AddItem( "#", "publish", "green" ):oContext:oTooltip( "Plublish" ):cPosition := "left"
         :AddItem( "#", "attach_file", "blue" ):oContext:oTooltip( "Attach file" ):cPosition := "left"
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

      WITH OBJECT WFooter():New( Self )
         WITH OBJECT WDiv():New( SO )
            :AddClass( "container" )
            WITH OBJECT WFlexRow():New( SO )
               WITH OBJECT WDiv():New( SO )
                  :oContext:Col( 12, NIL, 6 )
                  ECHO "<h5>Footer content</h5>"
                  ECHO "<p>You can use rows and columns here to organize your footer content.</p>"
               END WITH
               WITH OBJECT WDiv():New( SO )
                  :oContext:Col( 12, NIL, 4 )
                  :oContext:Offset( NIL, NIL, 8 )
                  ECHO "<h5>Links</h5>"
                  WITH OBJECT WDiv():New( SO )
                     :cTag := "ul"
                     WITH OBJECT WDiv():New( SO )
                        :cTag := "li"
                        WLink():New( SO ):cText := "Link 1"
                     END WITH
                     WITH OBJECT WDiv():New( SO )
                        :cTag := "li"
                        WLink():New( SO ):cText := "Link 2"
                     END WITH
                     WITH OBJECT WDiv():New( SO )
                        :cTag := "li"
                        WLink():New( SO ):cText := "Link 3"
                     END WITH
                     WITH OBJECT WDiv():New( SO )
                        :cTag := "li"
                        WLink():New( SO ):cText := "Link 4"
                     END WITH
                  END WITH
               END WITH
            END WITH
         END WITH
         WITH OBJECT WDiv():New( SO )
            :AddClass( "footer-copyright" )
            WITH OBJECT WDiv():New( SO )
               :AddClass( "container" )
               ECHO "@ 2024 Copyright TEXT"
               WLink():New( SO ):cText := "More links"
            END WITH
         END WITH
      END WITH
   END WITH

RETURN nil

