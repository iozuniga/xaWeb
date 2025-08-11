/*
CLASS ZDropdown FROM WDiv
   DATA aTriggers       INIT {} AS CLASS WLink
   DATA aItems          INIT {} AS CLASS WDropdownItem
   DATA cAlignment      INIT "" VALUES "left", "right"
   DATA lAutoTrigger    INIT .T.
   DATA lConstrainWidth INIT .T.
   DATA lCoverTrigger   INIT .T.
   DATA lCloseOnClick   INIT .T.
   DATA lHover          INIT .F.
   DATA nInDuration     INIT 150
   DATA nOutDuration    INIT 250
   DATA cOnOpenStart    INIT ""
   DATA cOnOpenEnd      INIT ""
   DATA cOnCloseStart   INIT ""
   DATA cOnCloseEnd     INIT ""

   METHOD AddTrigger( oParent, cText, cIcon )
   METHOD AddItem( cText, cIcon, chRef )
   METHOD AddDivider() INLINE ::AddItem( "-" )
ENDCLASS

CLASS ZDropDownItem FROM WDiv
   DATA cIcon     INIT ""
   DATA cText     INIT ""
   DATA cHRef     INIT ""
   DATA oLink     AS CLASS WLink
   DATA oIcon     AS CLASS WIconGoogle
ENDCLASS

*/

#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oDropdown1, oDropdown2
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
         :cText := "<h4>xaWeb - Materialize Dropdown</h4>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/dropdown.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
         :Create()
      END WITH

      WITH OBJECT oDropdown1 := WDropdown():New( SO )
         :AddItem( "One" )
         :AddItem( "Two" )
         :AddDivider()
         :AddItem( "Three" )
         :AddItem( "Four", "view_module", "#" )
         :AddItem( "Five", "cloud", "#" )
      END WITH

      WITH OBJECT oDropdown2 := WDropdown():New( SO )
         :cAlignment := "right"
         :AddItem( "One" )
         :AddItem( "Two" )
         :AddDivider()
         :AddItem( "Three" )
         :AddItem( "Four", "view_module", "#" )
         :AddItem( "Five", "cloud", "#" )
      END WITH

      WITH OBJECT WFlexRow():New(SO)
         WITH OBJECT WDiv():New( SO )
            :oStyle:Width := "50%"
            oDropdown1:AddTrigger( SO, "Left - DropDown!" )
         END WITH
         WITH OBJECT WDiv():New( SO )
            :oStyle:Width := "50%"
            oDropdown2:AddTrigger( SO, "Right - DropDown!" )
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

