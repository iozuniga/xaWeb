///*
 * Proyect: XailerWeb framework
 * File: ZTable.prg
 * Description: HTML table class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Copyright https://dcode.domenade.com/tutorials/how-to-easily-sort-html-tables-with-css-and-javascript
 * Copyright https://www.youtube.com/watch?v=czZ1PvNW5hk
 * Note: Specific attributes inside table tags are deprecated. Not implemented
 */

#include "xailerweb.ch"
#include "error.ch"

STATIC lCustom := .F., lSort := .F., lRowClick := .F., lFilter := .F.

CLASS ZTable FROM WControl
EXPORTED:
   DATA oContainer         AS CLASS WDiv
   DATA aHeaders           INIT {}    // { oCol, ... } sTableCol
   DATA aRows              INIT {}    // { oRow, ... } sTableRow
   DATA aColGroup          INIT {}    // { oColGroup, ...}  sTableColGroup
   DATA cCaption           INIT ""
   DATA cHeaderBkColor     INIT ""    // CSS format
   DATA cHeaderColor       INIT ""    // CSS format
   DATA cFooterBkColor     INIT ""    // CSS format
   DATA cFooterColor       INIT ""    // CSS format
   DATA cMaxHeight         INIT ""
   DATA cBtnDelId          INIT ""
   DATA cBtnEditId         INIT ""
   DATA lResponsive        INIT .F.
   DATA lCanSort           INIT .F.
   DATA lShowID            INIT .F.
   DATA lCanFilter         INIT .F. PERSISTENT
   DATA lShowSelected      INIT .F.

   METHOD New( oParent ) CONSTRUCTOR

   EVENT OnRowClick( oSender )

   // This two methods are only fired on Default or action mode
   EVENT OnStartRow( oSender, oRow, nType ) // nType: 1 header, 2 body, 3 footer
   EVENT OnStartCol( oSender, oCol )

PUBLIC:
   DATA oHeader                       // sTableZone
   DATA oFooter                       // sTableZone
   DATA oBody                         // sTableZone
   DATA oEdit                         // sTableTask
   DATA oAppend                       // sTableTask

   METHOD AddRow()
   METHOD nHeader( nValue ) SETGET
   METHOD nFooter( nValue ) SETGET
   METHOD AddColGroup( nSpan )
   METHOD LoadData( aData, lRecno, lReverse )
   METHOD Append( cDocPage )
   METHOD Edit( cDocPage )
   METHOD LoadFromService( cService, cSourceID )
   METHOD EditFromService( cService, cSourceID )
   METHOD DeleteFromService( cService, cSourceID )

RESERVED:
   DATA lRowClick           INIT .F. READONLY

   METHOD CustomCss()
   METHOD ResponsiveCss()
   METHOD SortCss()
   METHOD SortJs()
   METHOD RowClickJs()
   METHOD FilterJs()

   METHOD CreateZones()
   METHOD HtmlTagEnd()
   METHOD PreProcess()

PROTECTED:
   DATA cTag            INIT "table"

   METHOD InsertControl() VIRTUAL

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZTable

   ::oContainer := WDiv():New( oParent, nil )
   ::Super:New( ::oContainer, Self )

   ::AddClass( "xw-table__table" )
   ::oContainer:AddClass( "xw-table" )

RETURN Self

//------------------------------------------------------------------------------

 METHOD AddRow() CLASS ZTable

   LOCAL oRow := sTableRow():New( Self )

   AAdd( ::aRows, oRow )

RETURN oRow

//------------------------------------------------------------------------------

METHOD CreateZones() CLASS ZTable

   IF !HB_IsObject( ::oHeader )
      ::oHeader := sTableZone():New( Self )
      ::oHeader:nType := 1
   ENDIF

   IF !HB_IsObject( ::oFooter )
      ::oFooter := sTableZone():New( Self )
      ::oFooter:nType := 2
   ENDIF

   IF !HB_IsObject( ::oBody )
      ::oBody := sTableZone():New( Self )
      ::oBody:nType := 3
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD nHeader( nValue ) CLASS ZTable

   ::CreateZones()

   IF PCount() > 0
      ::oHeader:nRows := nValue
   ENDIF

RETURN ::oHeader:nRows

//------------------------------------------------------------------------------

METHOD nFooter( nValue ) CLASS ZTable

   ::CreateZones()

   IF PCount() > 0
      ::oFooter:nRows := nValue
   ENDIF

RETURN ::oFooter:nRows

//------------------------------------------------------------------------------

METHOD AddColGroup( nSpan ) CLASS ZTable

   LOCAL oColGroup

   oColGroup := sTableColGroup():New( Self, nSpan )

   AAdd( ::aColGroup, oColGroup )

RETURN oColGroup

//------------------------------------------------------------------------------

METHOD LoadData( aData, lRecno, lReverse ) CLASS ZTable

   LOCAL oRow
   LOCAL aRow, xCol, lHeader

   DEFAULT lRecno TO .f., lReverse TO .f.

   IF lRecno
      ::lRowClick := .T.
   ENDIF

   FOR EACH aRow IN aData
      lHeader := ( Len( ::aRows ) == 0 )
      oRow := ::AddRow()

      FOR EACH xCol IN aRow
         IF lReverse
            lHeader := xCol:__enumIsFirst()
         ENDIF
         oRow:AddCol( xCol, lHeader, 0 )
      NEXT
      lHeader :=  .F.
   NEXT

RETURN NIL

//------------------------------------------------------------------------------

METHOD LoadFromService( cService, cSourceID ) CLASS ZTable

   LOCAL oFetch

   DEFAULT cSourceID TO ""

   IF Empty( ::cId )
      ::cId := ::RandomId()
   ENDIF

   WITH OBJECT oFetch := WFetch():New()
      :cUrl      := Document:Service( cService )
      :cTargetId := ::cId
      :cSourceID := cSourceID
      :cCallBack := "xw_loadTable"
      :cContentType := "application/json"
   END WITH

RETURN oFetch

//------------------------------------------------------------------------------

METHOD Append( cDocPage ) CLASS ZTable

   ::oAppend := sTableTask():New( Self, cDocPage, .T. )

RETURN ::oAppend

//------------------------------------------------------------------------------

METHOD Edit( cDocPage ) CLASS ZTable

   ::oEdit := sTableTask():New( Self, cDocPage )

RETURN ::oEdit

//------------------------------------------------------------------------------

METHOD EditFromService( cService, cSourceID ) CLASS ZTable

   LOCAL oFetch

   DEFAULT cSourceID TO ""

   IF Empty( ::cId )
      ::cId := ::RandomId()
   ENDIF

   WITH OBJECT oFetch := WFetch():New()
      :cUrl      := Document:Service( cService )
      :cTargetId := ::cId
      :cSourceID := cSourceID
      :cJsFunction := "TableSelectedRow"
      :cCallBack := "xw_postEditTable"
      :cContentType := "application/json"
   END WITH

RETURN oFetch

//------------------------------------------------------------------------------

METHOD DeleteFromService( cService, cSourceID ) CLASS ZTable

   LOCAL oFetch

   DEFAULT cSourceID TO ""

   IF Empty( ::cId )
      ::cId := ::RandomId()
   ENDIF

   WITH OBJECT oFetch := WFetch():New()
      :cUrl      := Document:Service( cService )
      :cTargetId := ::cId
      :cSourceID := cSourceID
      :cJsFunction := "TableSelectedRow"
      :cCallBack := "xw_delRowTable"
      :cContentType := "application/json"
   END WITH

RETURN oFetch

//------------------------------------------------------------------------------

METHOD CustomCss()  CLASS ZTable

   LOCAL cCss

   TEXT INTO cCss
   .table-filterable tr:nth-child(even) {
      background-color: initial;
      }

   .table-filterable tbody tr {
      border-bottom: 1px solid #333333;
      }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

METHOD ResponsiveCss() CLASS ZTable

   LOCAL cCss

   TEXT INTO cCss
   @media (max-width: 650px) {
      #table1 th {
      display: none;
      }

      #table1 tfoot {
      display: none;
      }

      #table1 td {
      display: grid;
      gap: 0.5rem;
      grid-template-columns: 15ch auto;
      padding: 0.5rem 1rem;
      }

      #table1 td:first-child {
      padding-top: 2rem;
      }

      #table1 td:last-child {
      padding-bottom: 2rem;
      }

      #table1 td::before {
      font-weight: 700;
      text-transformation: capitalize;
      }
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

METHOD SortCss() CLASS ZTable

   LOCAL cCss

   TEXT INTO cCss
   .table-sortable th {
	      cursor: pointer;
      }

  .table-sortable .th-sort-asc::after {
   	content: "\25b4";
   }

   .table-sortable .th-sort-desc::after {
	   content: "\25be";
   }

   .table-sortable .th-sort-asc::after,
   .table-sortable .th-sort-desc::after {
      margin-left: 5px;
   }

   .table-sortable .th-sort-asc,
   .table-sortable .th-sort-desc {
	   background: rgba(0, 0, 0, 0.1);
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

METHOD SortJs() CLASS ZTable

   LOCAL cJs

   TEXT INTO cJs
   xw_sortTableListen();
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD RowClickJs()

   LOCAL cJs

   TEXT INTO cJs
   xw_rowClickOnTableListen();
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD FilterJs() CLASS ZTable

   LOCAL cJs

   TEXT INTO cJs
   xw_filterTableByColumn();
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTable

   LOCAL oHeader AS CLASS sTableCol
   LOCAL oBtn
   LOCAL cId, cCss, cJs, cTmp

   IF Empty( ::cId )
      cId := ::RandomId()
   ELSE
      cId := ::cId
   ENDIF

   cJs := ""

   IF ::oEdit != NIL
      ::oEdit:Preprocess()
   ENDIF

   IF ::oAppend != NIL
      ::oAppend:Preprocess()
   ENDIF

   IF ::lShowSelected
      Document:AddStyle( "#" + cId + " tr.row-selected", "background-color: rgba(0, 0, 0, 0.1);" )
   ENDIF

   IF ::lCanSort
      ::AddClass( "table-sortable" )
      IF !lSort
         cCss := ::SortCss()
         cJs  += ::SortJs() + hb_eol()
         Document:AddCss( cCss )
         lSort := .T.
      ENDIF
   ENDIF

   IF ::lCanFilter
      ::AddClass( "table-filterable" )
      IF !lFilter
         cJs  += ::FilterJs() + hb_eol()
         lFilter := .T.
      ENDIF
   ENDIF

   IF ::lRowClick .and. ::IsEvent( "OnRowClick" )
      ::AddClass( "table-recno" )
      ::AddDataset( "id", ::aHeaders[ 1 ]:xValue )
      Document:AddCss( "#" + cId + " tr {" + hb_eol() + "cursor: pointer };" )
      IF !lRowClick
         cJs  += ::RowClickJs() + hb_eol()
         lRowClick := .T.
      ENDIF
      IF !::lShowID
         Document:AddStyle( "#" + cId + " tr td:nth-child(1)", "display: none;" )
         Document:AddStyle( "#" + cId + " th:nth-child(1)", "display: none;" )
      ENDIF
   ENDIF

   IF !lCustom
      Document:AddCss( ::CustomCss() )
      lCustom := .T.
   ENDIF

   IF ::lResponsive
      cCss := ::ResponsiveCss()
      cCss := Left( cCss, RAt( "}", cCss ) - 1 ) + hb_eol()
      FOR EACH oHeader IN ::aHeaders
         cCss += '#' + cId + ' td:nth-of-type('+ ;
                 ToString( oHeader:__enumIndex() ) +;
                 ')::before {' + hb_eol() + 'content: "' + ;
                 ToString( oHeader:xValue ) + ;
                 ':";}' + hb_eol()
      NEXT
      cCss += hb_eol() + "}"
      Document:AddCss( cCss )
   ENDIF

   IF !Empty( ::cHeaderBkColor )
      Document:AddStyle( "#" + cId + " th", "background-color:" + ::cHeaderBkColor + ";" )
   ENDIF

   IF !Empty( ::cHeaderColor )
      Document:AddStyle( "#" + cId + " th", "color:" + ::cHeaderColor + ";" )
   ENDIF

   IF !Empty( ::cFooterBkColor )
      Document:AddStyle( "#" + cId + " tfoot", "background-color:" + ::cFooterBkColor + ";" )
   ENDIF

   IF !Empty( ::cFooterColor )
      Document:AddStyle( "#" + cId + " tfoot", "color:" + ::cFooterColor + ";" )
   ENDIF

   IF !Empty( ::cMaxHeight )
      ::oContainer:AddStyle( 'height:' + ::cMaxHeight + ';overflow-y:auto' +;
                             ';scroll-snap-type:y mandatory' )

      Document:AddStyle( "#" + cId + " thead", "position:sticky;inset-block-start: 0;" )
      Document:AddStyle( "#" + cId + " tfoot", "position:sticky;inset-block-end: 0;" )
      Document:AddStyle( "#" + cId + " tr", "scroll-snap-align: start;" )
      Document:AddStyle( "#" + cId + " th", "background-color: " + ::cHeaderBkColor + ";" )
      Document:AddStyle( "#" + cId + " td", "border: none;" ) // forced
      Document:AddStyle( "#" + cId + " th", "border: none;" ) // forced
      IF ::IsStyle( "Width" )
         ::oContainer:AddStyle( 'width:' + ::GetStyle( "Width" ) )
      ENDIF
      ::oStyle:cWidth := "100%"
   ENDIF

   IF !Empty( cJs )
      Document:AddScript( cJs )
   ENDIF

   FOR EACH oHeader IN ::aHeaders
      WITH OBJECT oHeader
         :AddDataset( "field", :cField )
         :AddClass( "field-info" )
      END WITH
   NEXT

   IF !Empty( ::cBtnEditId )
      oBtn := Document:SearchControl( ::cBtnEditId )
      ::AddDataset( "btnEdit", ::cBtnEditId )
      IF oBtn != NIL
         oBtn:lDisabled := .T.
      ENDIF
   ENDIF

   IF !Empty( ::cBtnDelId )
      oBtn := Document:SearchControl( ::cBtnDelId )
      ::AddDataset( "btnDel", ::cBtnDelId )
      IF oBtn != NIL
         oBtn:lDisabled := .T.
      ENDIF
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZTable

   LOCAL oCol, oRow
   LOCAL cHtml := ""
   LOCAL nFirstRow, nLastRow, nRow

   cHtml := ::Super:HtmlTagEnd()

   Document:nIndent ++

   IF !Empty( ::cCaption )
      cHtml += HTML_SPACES + '<caption>' + ::cCaption + '</caption>' + hb_eol()
   ENDIF

   FOR EACH oCol IN ::aColGroup
      cHtml += oCol:Html()
   NEXT

   nFirstRow := 1
   nLastRow  := Len( ::aRows )

   IF HB_IsObject( ::oHeader ) .AND. ::oHeader:nRows > 0
      cHtml += ::oHeader:Html()
      nLastRow := nFirstRow + ::oHeader:nRows - 1
      Document:nIndent ++
      FOR nRow := nFirstRow TO nLastRow
         ::OnStartRow( ::aRows[ nRow ], 0 )
         WITH OBJECT ::aRows[ nRow ]
            cHtml += :Html()
         END WITH
      NEXT
      IF ::lCanFilter
         cHtml += HTML_SPACES + '<tr class="table-search-row">' + hb_eol()
         Document:nIndent ++
         FOR EACH oCol IN ::aRows[ nFirstRow ]:aCols
            cHtml += HTML_SPACES + ;
                  '<td><input type="text" class="table-search-input" placeholder="' +;
                   oCol:xValue + '" style="width: 100%;"></td>' + hb_eol()
         NEXT
         Document:nIndent --
         cHtml += HTML_SPACES + '</tr>' + hb_eol()
      ENDIF
      Document:nIndent --
      cHtml += HTML_SPACES + '</thead>' + hb_eol()
      nFirstRow := nLastRow  + 1
      nLastRow  := Len( ::aRows ) - ::oFooter:nRows
   ENDIF

   IF HB_IsObject( ::oBody )
      cHtml += ::oBody:Html()
      Document:nIndent ++
   ENDIF

   FOR nRow := nFirstRow TO nLastRow
      ::OnStartRow( ::aRows[ nRow ], 1 )
      cHtml += ::aRows[ nRow ]:Html()
   NEXT

   IF HB_IsObject( ::oBody )
      Document:nIndent --
      cHtml += HTML_SPACES + '</tbody>' + hb_eol()
   ENDIF

   IF HB_IsObject( ::oFooter ) .AND. ::oFooter:nRows > 0
      cHtml +=  ::oFooter:Html()
      nFirstRow := nLastRow + 1
      nLastRow  := Len( ::aRows )
      Document:nIndent ++
      FOR nRow := nFirstRow TO nLastRow
         ::OnStartRow( ::aRows[ nRow ], 2 )
         cHtml += ::aRows[ nRow ]:Html()
      NEXT
      Document:nIndent --
      cHtml += HTML_SPACES + '</tfoot>' + hb_eol()
   ENDIF

   Document:nIndent --

   cHtml += HTML_SPACES + '</table>' + hb_eol()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS sTableColGroup FROM WBasic
EXPORT:
   DATA nSpan    INIT 0
   METHOD New( oParent, nSpan ) CONSTRUCTOR
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag      INIT "colgroup"
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, nSpan ) CLASS sTableColGroup

   ::Super:New( oParent, nil )

   IF nSpan != NIL
      ::nSpan := nSpan
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS sTableColGroup

   LOCAL cHtml := ""

   IF ::nSpan == 0
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := "HTML 'groupcol' must have a span value greater than zero."
         :Operation   := "WTable:Html()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ELSE
      cHtml += ' span="' + ToString( ::nSpan ) + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS sTableRow FROM WBasic STATIC
EXPORTED:
   DATA aCols  INIT {} // sTableCol

RESERVED:
   METHOD HtmlTagEnd()
   METHOD AddCol( xValue, lHeader, nColSpan )

PROTECTED:
   DATA cTag   INIT "tr"

   METHOD InsertControl() VIRTUAL
ENDCLASS

//------------------------------------------------------------------------------

METHOD AddCol( xValue, lHeader, nColSpan ) CLASS sTableRow

   LOCAL oCol

   DEFAULT lHeader TO .F., nColSpan TO 0

   oCol := sTableCol():New( Self, xValue, lHeader, nColSpan )

   AAdd( ::aCols, oCol )

RETURN oCol

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS sTableRow

   LOCAL aCols
   LOCAL oCol, oParent
   LOCAL cHtml := ""

   cHtml += ::Super:HtmlTagEnd()

   Document:nIndent ++

   oParent:= ::oParent

   FOR EACH oCol IN ::aCols
      oParent:OnStartCol( oCol )
      cHtml += oCol:Html()
   NEXT

   Document:nIndent --
   cHtml += HTML_SPACES + '</' + ::cTag + '>' + hb_eol()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS sTableCol FROM WBasic STATIC

RESERVED:
   DATA xValue    INIT NIL
   DATA cField    INIT ""
   DATA nColSpan  INIT 0
   DATA nRowSpan  INIT 0
   DATA lHeader   INIT .F.

   METHOD New( oRow, xValue, lHeader, nColSpan ) CONSTRUCTOR
   METHOD Html()
   METHOD HtmlTagIni()
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag      INIT ""

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oRow, xValue, lHeader, nColSpan ) CLASS sTableCol

   ::Super:New( oRow, nil )

   ::xValue   := xValue
   ::lHeader  := lHeader
   ::nColSpan := nColSpan

   IF lHeader
      ::cField := xValue
      AAdd( oRow:oParent:aHeaders, Self )
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD Html() CLASS sTableCol

   IF Empty( ::xValue ) .AND. ( ::nColSpan == 0 ) .AND. ( ::nRowSpan == 0 )
      RETURN ""
   ENDIF

RETURN ::Super:Html()

//------------------------------------------------------------------------------

METHOD HtmlTagIni() CLASS sTableCol

   IF ::lHeader
      ::cTag := "th"
   ELSE
      ::cTag := "td"
   ENDIF

RETURN ::Super:HtmlTagIni()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS sTableCol

   LOCAL cHtml := ""

   IF ::nColSpan > 0
      cHtml += ' colspan="' + ToString( ::nColSpan ) + '"'
   ENDIF

   IF ::nRowSpan > 0
      cHtml += ' rowspan="' + ToString( ::nRowSpan ) + '"'
   ENDIF

   ::cText := ToString( ::xValue )

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS sTableZone FROM WBasic
EXPORT:
   DATA nRows    INIT 0
   DATA nType    INIT 1 // 1 Header, 2 Footer, 3 Body

   METHOD HtmlTagIni()

PROTECTED:
   DATA cTag      INIT ""
ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagIni() CLASS sTableZone

   SWITCH ::nType
   CASE 1
      ::cTag := "thead"
      EXIT
   CASE 2
      ::cTag := "tfoot"
      EXIT
   CASE 3
      ::cTag := "tbody"
      EXIT
   END SWITCH

RETURN ::Super:HtmlTagIni()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS sTableTask STATIC
EXPORT:
   DATA oParent
   DATA oForm
   DATA cDocPage  INIT ""
   DATA lAppend   INIT .F.

   METHOD New( cPage ) CONSTRUCTOR
   METHOD Preprocess()
   METHOD Html()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cPage, lAppend ) CLASS sTableTask

   DEFAULT lAppend TO .F.

   ::oParent  := oParent
   ::cDocPage := cPage
   ::lAppend  := lAppend

RETURN Self

//------------------------------------------------------------------------------

METHOD Html() CLASS sTableTask

   LOCAL cJs

   cJs := 'xw_showPage("' + ::cDocPage + '", true, "' + ::oForm:cId + ;
          '", "' +  ::oParent:cId + '", ' + ;
          IIF( ::lAppend, 'true', 'false' ) + ');'

RETURN cJs

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS sTableTask

   LOCAL oPage

   oPage := Document:GetPage( ::cDocPage )

   IF !HB_IsObject( oPage )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "DocPage not loaded: '" + ::cDocPage + "'."
         :Operation   := "WTable:Edit( cDocPage )"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ELSE
      ::oForm := oPage:FirstForm()
      IF !HB_IsObject( ::oForm )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := "Form object not found on page: '" + ::cDocPage + "'."
            :Operation   := "WTable:Edit( cDocPage )"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ELSE
      ENDIF
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------
