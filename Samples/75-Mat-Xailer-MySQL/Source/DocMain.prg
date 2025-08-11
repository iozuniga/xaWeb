#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   DATA oDS    AS CLASS WXailerMariaDbDataSource
   DATA oCon   AS CLASS WMaterializeContext
   DATA oPag   AS CLASS WPagination
   DATA oTab   AS CLASS WTable
   DATA nPage  INIT 1 PERSISTENT // This is crucial
   DATA nRecs  INIT 0

   METHOD CreateDoc()
   METHOD PageChange( hParam, hEvent )
   METHOD SelectRange()

END CLASS

//------------------------------------------------------------------------------
// This method is called when any pagination item is clicked
// oPage:OnPageChange := "PageChange". That's all!!!!!!

METHOD PageChange( hParam, hEvent ) CLASS WDocMain

   LOCAL oItem
   LOCAL cPage := HB_HGetDef( hEvent, "detail-value", "" )

   IF Val( cPage ) > 0
      ::nPage := Val( cPage )
   ELSEIF cPage == "chevron_left"
      ::nPage := Max( 1, ::nPage - 1 )
   ELSEIF cPage == "chevron_right"
      ::nPage := Min( 10, ::nPage + 1 )
   ENDIF

   FOR EACH oItem IN ::oPag:aItems
      oItem:lActive := ( oItem:nPage == ::nPage )
   NEXT

   ::SelectRange()

RETURN nil

//------------------------------------------------------------------------------

METHOD SelectRange() CLASS WDocMain

   LOCAL aData
   LOCAL cQuery
   LOCAL nRecs, nOffset, nChunk

   nRecs   := ::nRecs
   nChunk  := Int( nRecs / 10 )
   nOffset := ( (::nPage -1 ) * nChunk )
   cQuery  := "SELECT * FROM customer LIMIT "+;
                ToString( nOffset ) + "," + ToString( nChunk )

   aData  := ::oDS:QueryArray( cQuery, {} )

   ::oTab:LoadData( aData )

   ::oDs:Disconnect()

RETURN NIL

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL aData, aHeader
   LOCAL cCode, cJs

   Engine:lDebug := .t.

   ::oCon := WMaterializeContext():New( Self )

   // This simple Javascript function just shows the source code on a hidden TextArea

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

   WITH OBJECT ::oDS := WXailerMariaDBDataSource():New()
      :cHost := "xailer.com"
      :cDatabase := "Neptuno"
      :cUser := "neptuno_ro"
      :cPassword := "ao5S!19g5"
      :Create()
      :lConnected := .T.
      ::nRecs := :QueryValue( "SELECT Count(*) FROM customer", 0 )
      aData   := :QueryArray( "select * from customer limit 1", @aHeader )
   END WITH

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
      ECHO "<h4>xaWeb - Materialize & Xailer - MariaDB Table pagination</h4>"

      WITH OBJECT ::oPag := WPagination():New( SO )
         :OnPageChange := "PageChange"
         :oStyle:List_style_type := "none"
         :oStyle:Padding := "0px"
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
         :AddItem( "10" )
         :aItems[ ::nPage ]:lActive := .T.
         :Create()
      END WITH

      WITH OBJECT ::oTab := WTable():New( SO )
         :cId := "table1"
         :oStyle:Width := "100%"
         :cHeaderBkColor := ::oCon:PrimaryColor()
         :cHeaderColor := ::oCon:PrimaryTextColor()
         :SetHeader( aHeader, .T. )
         :cMaxHeight := "600px"
         :lResponsive := .T.
         :lShowID := .t.
         :lShowSelected := .t.
         :lCanSort := .t.
         :lCanFilter := .f.
         :Create()
      END WITH
   END WITH

   IF ::IsDefault()
      ::SelectRange()
   ENDIF

   FILE "DocMain.prg" INTO cCode

   ECHO "<hr>" INTO Self

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
      :AddDataset( "text", HB_Base64Encode( cCode ) )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

