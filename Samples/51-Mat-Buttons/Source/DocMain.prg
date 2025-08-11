/*
CLASS ZButton FROM WContainer
   DATA cDisplayType INIT "" VALUES "", "filled", "tonal", "outlined", "text", "floating"
   DATA cWaveEffect  INIT "" VALUES "", "normal", "light"
   DATA cText        INIT ""
   DATA cIcon        INIT ""
   DATA cColor       INIT ""
   DATA cIconAlign   INIT "" VALUES "", "left", "right"
   DATA lRounded     INIT .F.
   DATA lDisabled    INIT .F.
   DATA llarge       INIT .F.
   DATA lSmall       INIT .F.
   DATA lPulse       INIT .F.
   DATA lElevated    INIT .F.
   DATA lSubmit      INIT .F.
   DATA oIcon        AS CLASS WIconGoogle
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
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
      :Create()
   END WITH

   WITH OBJECT WDiv():New( Self )
      :AddClass( "container" )
      WITH OBJECT WText():New( SO )
         :cText := "<h3>xaWeb - Materialize Buttons & Breadcrumb</h3>"
         :Create()
      END WITH

      WITH OBJECT WFlexRow():New( SO )
         :cJustifyContent := "space-between"
         WITH OBJECT WButton():New( SO )
            :cDisplayType := "floating"
            :lPulse := .T.
            :cIcon := "face"
            //:cColor := "grey"
         END WITH
         WITH OBJECT WButton():New( SO )
            :cDisplayType := "floating"
            :lPulse := .T.
            :cIcon := "home"
            :cColor := "green"
         END WITH
         WITH OBJECT WButton():New( SO )
            :cDisplayType := "floating"
            :lPulse := .T.
            :cIcon := "warning"
            :cColor := "red"
         END WITH
      END WITH

      ECHO "<p></p>"

      WITH OBJECT WButton():New( SO )
         :cText := "Home"
         :cIcon := "home"
         :cWaveEffect := "normal"
         :cDisplayType := "filled"
      END WITH
      WITH OBJECT wButton():New( SO )
         :cText := "Help"
         :cIcon := "help"
         :cWaveEffect := "normal"
         :cDisplayType := "tonal"
      END WITH
      WITH OBJECT wButton():New( SO )
         :cText := "Warning"
         :cIcon := "warning"
         :cWaveEffect := "normal"
         :cColor := "orange"
         :cDisplayType := "filled"
      END WITH

      ECHO "<p></p>"

      WITH OBJECT WDiv():New( SO )
         :oContext:Col(12)
         WBreadcrumb():New( SO ):cText := "First"
         WBreadcrumb():New( SO ):cText := "Second"
         WBreadcrumb():New( SO ):cText := "Third"
         WITH OBJECT WBreadcrumb():New( SO )
            :cText := "Fourth"
            :oStyle:Color := "red"
         END WITH
      END WITH

      FILE "DocMain.prg" INTO cCode

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

