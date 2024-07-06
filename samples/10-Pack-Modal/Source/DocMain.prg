#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()
   METHOD Act_Click( hEvent )

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

   ECHO "<h1>XailerWeb - Packages demo</h1>" INTO Self

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a modal dialog box"
      :OnClick := oModal
      :cId := "button1"
      :Create()
   END WITH

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WTextArea():New( Self )
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Click( hEvent ) CLASS WDocMain

   ECHO "<p>Method fired from a Dialog button from the ModalPackage</p>" INTO Self

RETURN NIL

//------------------------------------------------------------------------------

