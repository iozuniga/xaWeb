/*
CLASS ZModal FROM WDiv
   DATA oContent           AS CLASS WDiv
   DATA oFooter            AS CLASS WDiv
   DATA lFixedFooter       INIT .F.
   DATA lBottomSheet       INIT .F.
   DATA aLinks             INIT {} AS CLASS WLink
   DATA aButtons           INIT {} AS CLASS WButton
   DATA cStartingStop      INIT "4%"
   DATA cEndingStop        INIT "10%"
   DATA nInDuration        INIT 250
   DATA nOutDuration       INIT 250
   DATA nOpacity           INIT 0.5
   DATA lDismissible       INIT .T.
   DATA lPreventScrolling  INIT .T.
   DATA cOnOpenStart       INIT ""
   DATA cOnOpenEnd         INIT ""
   DATA cOnCloseStart      INIT ""
   DATA cOnCloseEnd        INIT ""
   METHOD AddLink( oParent, cText, cIcon )
   METHOD AddButton( oParent, cText, cIcon )
ENDCLASS
*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oModal1, oModal2, oModal3
   LOCAL cCode, cJs, c1

   WMaterializeContext():New( Self )

   TEXT INTO c1
      Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum
   ENDTEXT

   c1 := Replicate( "<p>" + c1 + "</p>", 8 )

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
         :cText := "<h4>xaWeb - Materialize Modal</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/modals.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
         :Create()
      END WITH

      WITH OBJECT oModal1 := WModal():New( Self )
         WITH OBJECT :oContent
            ECHO "<h4>Modal Header</h4>"
            ECHO "<p>A bunch of text</p>"
         END WITH
         WITH OBJECT :oFooter
            WITH OBJECT WLink():New( SO )
               :cText := "Agree"
               :AddClass( "modal-close waves-effect btn-flat" )
               :Create()
            END WITH
         END WITH
      END WITH

      WITH OBJECT oModal1:AddLink( SO, "Modal" )
         :AddClass( "waves-effect waves-light" )
      END WITH

      WITH OBJECT oModal1:AddButton( SO, "Modal" )
         :AddClass( "waves-effect waves-light" )
         :cDisplayType := "filled"
      END WITH

      WITH OBJECT oModal2 := WModal():New( Self )
         :lFixedFooter := .T.
         WITH OBJECT :oContent
            ECHO "<h4>Modal Header</h4>"
            ECHO c1
         END WITH
         WITH OBJECT :oFooter
            WITH OBJECT WLink():New( SO )
               :cText := "Agree"
               :AddClass( "modal-close waves-effect btn-flat" )
            END WITH
         END WITH
      END WITH

      WITH OBJECT oModal2:AddLink( SO, "Modal with fixed footer" )
         :AddClass( "waves-effect waves-light" )
      END WITH

      WITH OBJECT oModal3 := WModal():New( Self )
         :lBottomSheet := .T.
         WITH OBJECT :oContent
            ECHO "<h4>Modal Header</h4>"
            ECHO "<p>A bunch of text</p>"
         END WITH
         WITH OBJECT :oFooter
            WITH OBJECT WLink():New( SO )
               :cText := "Agree"
               :AddClass( "modal-close waves-effect btn-flat" )
            END WITH
         END WITH
      END WITH

      WITH OBJECT oModal3:AddLink( SO, "Modal with bottom-sheet" )
         :AddClass( "waves-effect waves-light" )
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

