/*
This example simulates the partial download of data. Since DBFs are used, the
filter functionality offered by DBF files has been used to achieve the same effect.
In SQL database environments the example would have been done using the SQL
LIMIT and OFFSET clauses (or equivalent).
*/

#include "xa-materialize.ch"

REQUEST DBFCDX

CLASS WDocMain FROM WDoc

   DATA oCon   AS CLASS WMaterializeContext
   DATA oPag   AS CLASS WPagination
   DATA oTab   AS CLASS WTable
   DATA nPage  INIT 1 PERSISTENT // This is crucial

   METHOD CreateDoc()
   METHOD PageChange( hParam, hEvent )
   METHOD Filter()

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

   ::Filter()

RETURN nil

//------------------------------------------------------------------------------

METHOD Filter() CLASS WDocMain

   LOCAL nRecs, nMin, nMax, nChunk

   nRecs  := Customer->( RecCount() )
   nChunk := Int( nRecs / 10 )
   nMin   := ( (::nPage -1 ) * nChunk ) + 1
   nMax   := nMin + nChunk

   Customer->( DBSetFilter( { || FIELD->ID >= nMin .AND. FIELD->ID <= nMax } ) )
   Customer->(DBGoTop())

   ::oTab:LoadDbf( "customer" )

RETURN NIL

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs, cCss, cPath

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

#ifdef _LINUX_
   cPath := HB_DirBase() + "data" + hb_OsPathSeparator()
#else
   cPath := HB_DirBase() + "..\..\resource\"
#endif

   USE ( cPath + "customer.dbf" ) ALIAS Customer READONLY VIA "DBFCDX"
   SET ORDER TO TAG "ID"

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
      ECHO "<h4>xaWeb - Materialize - Table pagination</h4>"

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
         :LoadDbf( "customer",  {"Id","First","Last","Street", "City"}, .T., .F. ) // Just load header
         :aHeaders[ 1 ]:cHeader := "ID"
         :cMaxHeight := "600px"
         :lResponsive := .T.
         :lShowID := .t.
         :lShowSelected := .t.
         :lCanSort := .t.
         :lCanFilter := .t.
         :Create()
      END WITH
   END WITH

   IF !::IsAction()  // On First call, there is no Action
      ::Filter()
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

