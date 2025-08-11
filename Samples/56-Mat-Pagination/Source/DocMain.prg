/*
CLASS ZPagination FROM WDiv
   DATA cWaveEffect  INIT "" VALUES "", "normal", "light"
   DATA cIconLeft    INIT "chevron_left"
   DATA cIconRight   INIT "chevron_right"
   DATA chRefLeft    INIT "#"
   DATA chRefRight   INIT "#"

   METHOD AddItem( chRef, cText, lIcon )
ENDCLASS

CLASS ZPreloader FROM WDiv
   DATA cSize        INIT "" VALUES "", "big", "small"
   DATA cColor       INIT ""
   DATA lCircular    INIT .F.
   DATA lDeterminate INIT .F.
   DATA lFlash       INIT .F.
   DATA nValue       INIT 0
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
         :cText := "<h4>xaWeb - Materialize Pagination and Preloader</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize pagination page"
         :cHref := "https://materializeweb.com/pagination.html#!"
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize preloader page"
         :cHref := "https://materializeweb.com/prealoader.html"
         :oStyle:Margin_left := "2em"
      END WITH

      WITH OBJECT WPagination():New( SO )
         :cWaveEffect := "normal"
         :AddItem( "1" )
         :AddItem( "2" )
         :AddItem( "3" )
         :AddItem( "4" )
         :AddItem( "5" )
         :AddItem( "6" )
         :AddItem( "7" )
         :AddItem( "8" )
         :AddItem( "9" )
         :Create()
      END WITH

      ECHO "<h4>Determinate</h4>"

      WITH OBJECT WPreloader():New( SO )
         :lDeterminate := .T.
         :nValue := 70
      END WITH

      ECHO "<h4>Undeterminate</h4>"

      WITH OBJECT WPreloader():New( SO )
         :lDeterminate := .F.
         :cColor := "red"
      END WITH

      ECHO "<h4>Circular</h4>"

      WITH OBJECT WDiv():New( SO )
         :AddClass( "row" )
         WITH OBJECT WDiv():New( SO )
            :oContext:Col( 12, 4 )
            :AddClass( "center" )
            WITH OBJECT WPreloader():New( SO )
               :lCircular := .T.
               :cSize     := "big"
               :cColor    := "blue"
            END WITH
         END WITH
         WITH OBJECT WDiv():New( SO )
            :oContext:Col( 12, 4 )
            :AddClass( "center" )
            WITH OBJECT WPreloader():New( SO )
               :lCircular := .T.
               :cColor    := "red"
            END WITH
         END WITH
         WITH OBJECT WDiv():New( SO )
            :oContext:Col( 12, 4 )
            :AddClass( "center" )
            WITH OBJECT WPreloader():New( SO )
               :lCircular := .T.
               :cSize     := "small"
               :cColor    := "green"
            END WITH
         END WITH
      END WITH

      ECHO "<h4>Circular flashing colors</h4>"

      WITH OBJECT WDiv():New( SO )
         :AddClass( "row" )
         WITH OBJECT WDiv():New( SO )
            :oContext:Col( 12, 4 )
            :AddClass( "center" )
            WITH OBJECT WPreloader():New( SO )
               :lCircular := .T.
               :lFlash    := .T.
               :cSize     := "big"
            END WITH
         END WITH
         WITH OBJECT WDiv():New( SO )
            :oContext:Col( 12, 4 )
            :AddClass( "center" )
            WITH OBJECT WPreloader():New( SO )
               :lCircular := .T.
               :lFlash    := .T.
            END WITH
         END WITH
         WITH OBJECT WDiv():New( SO )
            :oContext:Col( 12, 4 )
            :AddClass( "center" )
            WITH OBJECT WPreloader():New( SO )
               :lCircular := .T.
               :lFlash    := .T.
               :cSize     := "small"
            END WITH
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

