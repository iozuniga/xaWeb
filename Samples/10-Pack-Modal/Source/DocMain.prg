#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()
   METHOD Act_Click( hParams, hEvent )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL oPackage, oModal
   LOCAL cJs, cCode

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   TEXT INTO cJs
      function js_click(e) {
         alert( 'JS event. Dialog not closed on purpose' );
      }
   ENDTEXT

   ::AddScript( cJs )

   oPackage := WModalMsgBtn():New( Self )
   oModal   := oPackage:ShowModal("titulo", "texto", {"OK", "XW event", "JS event"} )

   WITH OBJECT oModal
      :OnClick( 1, '<script>alert("OK");</script>' )
      :OnClick( 2, "Act_click" )
      :OnClick( 3, "js_click" )
      :OnClose( 3, .F. )
   END WITH

   ECHO "<h1>xaWeb - Packages demo</h1>" INTO Self

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a modal dialog box"
      :OnClick := oModal
      :cId := "button1"
      :Create()
   END WITH

   TEXT INTO cJs
      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.value);
            ele.hidden = false;
            document.getElementById("btnsource").style.display = "none";
         }
      }
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WButton():New( Self )
      :cText := "This button shows xaWeb source code"
      :Onclick := "showCode"
      :cId := "btnsource"
      :Create()
   END WITH

   WITH OBJECT WTextArea():New( Self )
      :oStyle:Margin_top := "20px"
      :oStyle:Font_Family := "monospace"
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Click( hParams, hEvent ) CLASS WDocMain

   ECHO "<p>Method fired from a Dialog button from the ModalPackage</p>" INTO Self

RETURN NIL

//------------------------------------------------------------------------------

