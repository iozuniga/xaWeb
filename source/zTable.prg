/*
 * Proyecto: xaWeb framework
 * Fichero: ZTable.prg
 * Descripción: HTML table class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Referred to https://dcode.domenade.com/tutorials/how-to-easily-sort-html-tables-with-css-and-javascript
 * Referred to https://www.youtube.com/watch?v=czZ1PvNW5hk
 * Note: Specific attributes inside table tags are deprecated. Not implemented
 */

#include "xaWeb.ch"
#include "error.ch"
#include "dbstruct.ch"

STATIC lCustom := .F., lSort := .F., lRowClick := .F., lFilter := .F.

CLASS ZTable FROM WControl
PUBLISHED:
   DATA oContainer         AS CLASS WDiv
   DATA aHeaders           INIT {}    AS CLASS WTableCell
   DATA aRows              INIT {}    AS CLASS WTableRow
   DATA aColGroup          INIT {}    AS CLASS WTableColGroup
   DATA cCaption           INIT ""
   DATA cHeaderBkColor     INIT ""    // CSS format
   DATA cHeaderColor       INIT ""    // CSS format
   DATA cFooterBkColor     INIT ""    // CSS format
   DATA cFooterColor       INIT ""    // CSS format
   DATA cMaxHeight         INIT ""
   DATA lResponsive        INIT .F.
   DATA lCanSort           INIT .F.
   DATA lShowID            INIT .F.
   DATA lCanFilter         INIT .F. PERSISTENT
   DATA lShowSelected      INIT .F.

   EVENT OnRowClick( jsEvent )

   // This two methods are only fired on Default or action mode
   EVENT OnStartRow( oSender, oRow AS CLASS WTableRow, nType ) // nType: 1 header, 2 body, 3 footer
   EVENT OnStartCell( oSender, oCell AS CLASS WTableCell )

   DATA oHeader            AS CLASS WTableZone
   DATA oFooter            AS CLASS WTableZone
   DATA oBody              AS CLASS WTableZone
   DATA oEditTask          AS CLASS WTableTask
   DATA oAppendTask        AS CLASS WTableTask

   METHOD AddRow()  // --> WTableRow
   METHOD nHeader( nValue ) SETGET
   METHOD nFooter( nValue ) SETGET
   METHOD AddColGroup( nSpan )  // --> WTableColGroup
   METHOD SetHeader( aData, lRecno )  // --> WTableRow
   METHOD LoadData( aData, lRecno, lReverse )  // --> nil
   METHOD LoadDbf( cAlias, aFields, lRecno, lData )  // --> nil
   METHOD Append( cDocSection )  // --> WTableTask
   METHOD Edit( cDocSection )   // --> WTableTask
   METHOD LoadFromService( cService, oSource )  // --> WFetch
   METHOD EditFromService( cService, oSource )  // --> WFetch
   METHOD DeleteFromService( cService, oSource ) // --> WFetch

   METHOD oEditControl( oValue ) SETGET AS CLASS WControl
   METHOD oDelControl( oValue ) SETGET AS CLASS WControl

RESERVED:
   DATA foEditControl
   DATA foDelcontrol
   DATA lRowClick          INIT .F. READONLY

   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()

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
   DATA cTag         INIT "table"
   DATA cCache
   DATA lCloseTag    INIT .F.

   METHOD InsertControl() VIRTUAL

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZTable

   ::oContainer := WDiv():New( oParent, Self, .T. )
   ::Super:New( ::oContainer, oParent, lAuto )

   ::AddClass( "xa-table__table" )
   ::oContainer:AddClass( "xa-table" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZTable

   ::aHeaders   := {}
   ::aRows      := {}
   ::aColGroup  := {}
   ::oHeader    := NIL
   ::oFooter    := NIL
   ::oBody      := NIL
   ::oContainer := NIL

   IF HB_IsObject( ::oEditTask )
      ::oEditTask:End()
      ::oEditTask := NIL
   ENDIF

   IF HB_IsObject( ::oAppendTask )
      ::oAppendTask:End()
      ::oAppendTask := NIL
   ENDIF

RETURN ::Super:End()

//------------------------------------------------------------------------------

 METHOD AddRow() CLASS ZTable

   LOCAL oRow := ZTableRow():New( Self )

   AAdd( ::aRows, oRow )

RETURN oRow

//------------------------------------------------------------------------------

METHOD CreateZones() CLASS ZTable

   IF !HB_IsObject( ::oHeader )
      ::oHeader := ZTableZone():New( Self )
      ::oHeader:nType := 1
   ENDIF

   IF !HB_IsObject( ::oFooter )
      ::oFooter := ZTableZone():New( Self )
      ::oFooter:nType := 2
   ENDIF

   IF !HB_IsObject( ::oBody )
      ::oBody := ZTableZone():New( Self )
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

   oColGroup := ZTableColGroup():New( Self, nSpan )

   AAdd( ::aColGroup, oColGroup )

RETURN oColGroup

//------------------------------------------------------------------------------

METHOD SetHeader( aData, lRecno ) CLASS ZTable

   LOCAL oHeader
   LOCAL cField

   DEFAULT lRecno TO .F.

   IF lRecno
      ::lRowClick := .T.
   ENDIF

   oHeader := ::AddRow()

   FOR EACH cField IN aData
      oHeader:AddCell( cField, .T. )
   NEXT

   ::nHeader ++

RETURN oHeader

//------------------------------------------------------------------------------

METHOD LoadData( aData, lRecno, lReverse ) CLASS ZTable

   LOCAL oRow
   LOCAL aRow, xCell, lHeader

   DEFAULT lRecno TO .f., lReverse TO .f.

   IF lRecno
      ::lRowClick := .T.
   ENDIF

   IF Empty( aData )
      RETURN nil
   ENDIF

   lHeader := ( Len( ::aRows ) == 0 )

   IF HB_IsArray( aData[ 1 ] )
      FOR EACH aRow IN aData
         oRow := ::AddRow()
         FOR EACH xCell IN aRow
            IF lReverse
               lHeader := xCell:__enumIsFirst()
            ENDIF
            oRow:AddCell( xCell, lHeader, 0 )
         NEXT
         lHeader :=  .F.
      NEXT
   ELSE  // Its a Header or Footer. Every element is a string
      oRow := ::AddRow()
      FOR EACH xCell IN aData
         oRow:AddCell( xCell, lHeader, 0 )
      NEXT
      IF lHeader
         ::nHeader ++
      ELSE
         ::nFooter ++
      ENDIF
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------

METHOD LoadDbf( cAlias, aFields, lRecno, lData ) CLASS ZTable

   LOCAL oRow, oCell
   LOCAL aRow, aField
   LOCAL nFor

   DEFAULT lRecno TO .T., lData TO .T.

   IF lRecno
      ::lRowClick := .T.
   ENDIF

   IF Len( ::aRows ) == 0
      IF Empty( aFields )
         aFields := {}
         FOR EACH aField IN (cAlias)->( DBStruct() )
            AAdd( aFields, ( { aField[ DBS_NAME ], aField:__EnumIndex() } ) )
         NEXT
      ELSE
         FOR nFor := 1 TO Len( aFields )
            aFields[ nFor ] := { aFields[ nFor ], (cAlias)->( FieldPos( aFields[ nFor ] ) ) }
         NEXT
      ENDIF

      oRow := ::AddRow()

      FOR EACH aField IN aFields
         oRow:AddCell( aField[ 1 ], .T. )
      NEXT

      ::nHeader := 1
   ELSE
      aFields := {}
      FOR EACH oCell IN ::aHeaders
         AAdd( aFields, { oCell:cField, (cAlias)->( FieldPos( oCell:cField ) ) } )
      NEXT
   ENDIF

   (cAlias)->( DbGotop() )

   IF lData
      DO WHILE !(cAlias)->( Eof() )
         oRow := ::AddRow()
         FOR EACH aField IN aFields
            oRow:AddCell( (cAlias)->( Fieldget( aField[ 2 ] ) ) )
         NEXT
         (cAlias)->( DbSkip() )
      ENDDO
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------

METHOD LoadFromService( cService, oSource ) CLASS ZTable

   LOCAL oFetch
   LOCAL cId

   IF HB_IsObject( oSource )
      cId := ::ValidId()
      WITH OBJECT oFetch := WFetch():New()
         :cUrl      := Document:Service( cService )
         :cTargetId := cId
         :cSourceID := oSource:cId
         :cCallBack := "xa_loadTable"
         :cContentType := "application/json"
      END WITH
   ENDIF

RETURN oFetch

//------------------------------------------------------------------------------

METHOD Append( cDocSection ) CLASS ZTable

   ::oAppendTask := ZTableTask():New( Self, cDocSection, .T. )

RETURN ::oAppendTask

//------------------------------------------------------------------------------

METHOD Edit( cDocSection ) CLASS ZTable

   ::oEditTask := ZTableTask():New( Self, cDocSection )

RETURN ::oEditTask

//------------------------------------------------------------------------------

METHOD EditFromService( cService, oSource ) CLASS ZTable

   LOCAL oFetch
   LOCAL cId

   IF HB_IsObject( oSource )
      cId := ::ValidId()
      WITH OBJECT oFetch := WFetch():New()
         :cUrl      := Document:Service( cService )
         :cTargetId := cId
         :cSourceID := oSource:ValidId()
         :cJsFunction := "TableSelectedRow"
         :cCallBack := "xa_postEditTable"
         :cContentType := "application/json"
      END WITH
   ENDIF

RETURN oFetch

//------------------------------------------------------------------------------

METHOD DeleteFromService( cService, oSource ) CLASS ZTable

   LOCAL oFetch
   LOCAL cId

   IF HB_IsObject( oSource )
      cId := ::ValidId()
      WITH OBJECT oFetch := WFetch():New()
         :cUrl      := Document:Service( cService )
         :cTargetId := cId
         :cSourceID := oSource:ValidId()
         :cJsFunction := "TableSelectedRow"
         :cCallBack := "xa_delRowTable"
         :cContentType := "application/json"
      END WITH
   ENDIF

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
   xa_sortTableListen();
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD RowClickJs()

   LOCAL cJs

   TEXT INTO cJs
   xa_rowClickOnTableListen();
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD FilterJs() CLASS ZTable

   LOCAL cJs

   TEXT INTO cJs
   xa_filterTableByColumn();
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTable

   LOCAL oHeader AS CLASS ZTableCell
   LOCAL oBtn
   LOCAL cId, cCss, cJs, cTmp

   cId := ::ValidId()
   cJs := ""

   IF Len( ::aHeaders ) == 0
      RETURN ::Super:PreProcess()
   ENDIF

   IF ::lShowSelected
      IF !::lRowClick
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := "Property lShowSelected requires that data is loaded with recno information."
            :Operation   := "WTable:lShowSelected"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
      Document:AddStyle( "#" + cId + " tr.row-selected", "background-color: var(--primary-hover-color);" )
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

   IF ::lRowClick
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
      //Document:AddStyle( "#" + cId + " th", "background-color: " + ::cHeaderBkColor + ";" )
      Document:AddStyle( "#" + cId + " td", "border: none;" ) // forced
      Document:AddStyle( "#" + cId + " th", "border: none;" ) // forced
      IF ::IsStyle( "Width" )
         ::oContainer:AddStyle( 'width:' + ::GetStyle( "Width" ) )
      ENDIF
      ::oStyle:Width := "100%"
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

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
// oControl should be a Link or Button or Specialy suppported control

 METHOD oEditControl( oControl ) CLASS ZTable

   IF PCount() > 0
      ::foEditControl := oControl
      ::AddDataset( "btnEdit", oControl:ValidId() )
      oControl:lDisabled := .T.
   ENDIF

RETURN ::FoEditControl

//------------------------------------------------------------------------------
// oControl should be a Link or Button or Specialy suppported control

 METHOD oDelControl( oControl ) CLASS ZTable

   IF PCount() > 0
      ::foDelControl := oControl
      ::AddDataset( "btnDel", oControl:ValidId() )
      oControl:lDisabled := .T.
   ENDIF

RETURN ::FoDelControl

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZTable

   LOCAL oCell, oRow
   LOCAL cHtml := "", cColor
   LOCAL nFirstRow, nLastRow, nRow

   cHtml := ::Super:HtmlTagEnd()

   Document:nIndent ++

   IF !Empty( ::cCaption )
      cHtml += HTML_SPACES + '<caption>' + ::cCaption + '</caption>' + hb_eol()
   ENDIF

   FOR EACH oCell IN ::aColGroup
      cHtml += oCell:Html()
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
         IF Document:oContext != NIL
            cColor := Document:oContext:BodyColor()
         ELSE
            cColor := "white"
         ENDIF
         cHtml += HTML_SPACES + '<tr class="table-search-row" style="background-color:' + cColor + ';">' + hb_eol()
         Document:nIndent ++
         FOR EACH oCell IN ::aRows[ nFirstRow ]:aCells
            cHtml += HTML_SPACES + ;
                  '<td><input type="text" class="table-search-input" autocomplete="new-name" placeholder="' +;
                   oCell:xValue + '" style="width: 100%;height:1.7em;"></td>' + hb_eol()
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

CLASS ZTableColGroup FROM WBasic
PUBLISHED:
   DATA nSpan    INIT 0

RESERVED:
   METHOD New( oParent, nSpan ) CONSTRUCTOR
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag      INIT "colgroup"
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, nSpan ) CLASS ZTableColGroup

   ::Super:New( oParent, oParent:oOwner, .T. )

   IF nSpan != NIL
      ::nSpan := nSpan
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZTableColGroup

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

CLASS ZTableRow FROM WBasic
PUBLISHED:
   DATA aCells  INIT {} AS CLASS WTableCell

   METHOD AddCell( xValue, lHeader, nColSpan )

RESERVED:
   METHOD HtmlTagEnd()

PROTECTED:
   DATA cTag   INIT "tr"

   METHOD InsertControl() VIRTUAL
ENDCLASS

//------------------------------------------------------------------------------

METHOD AddCell( xValue, lHeader, nColSpan ) CLASS ZTableRow

   LOCAL oCell

   DEFAULT lHeader TO .F., nColSpan TO 0

   oCell := ZTableCell():New( Self, xValue, lHeader, nColSpan )

   AAdd( ::aCells, oCell )

RETURN oCell

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZTableRow

   LOCAL aCells
   LOCAL oCell, oParent
   LOCAL cHtml := ""

   cHtml += ::Super:HtmlTagEnd()

   Document:nIndent ++

   oParent:= ::oParent

   FOR EACH oCell IN ::aCells
      oParent:OnStartCell( oCell )
      cHtml += oCell:Html()
   NEXT

   Document:nIndent --
   cHtml += HTML_SPACES + '</' + ::cTag + '>' + hb_eol()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZTableCell FROM WBasic

PUBLISHED:
   DATA xValue    INIT NIL
   DATA cField    INIT ""
   DATA nColSpan  INIT 0
   DATA nRowSpan  INIT 0
   DATA lHeader   INIT .F.
   METHOD cHeader( cValue ) SETGET

RESERVED:
   METHOD New( oRow, xValue, lHeader, nColSpan ) CONSTRUCTOR
   METHOD HtmlTagIni()
   METHOD HtmlTagBody()
   METHOD Html()

PROTECTED:
   DATA cTag      INIT ""

ENDCLASS

//------------------------------------------------------------------------------

METHOD Html() CLASS ZTableCell

   IF ::xValue == NIL
      RETURN ""
   ENDIF

RETURN ::Super:Html()

//------------------------------------------------------------------------------

METHOD New( oRow, xValue, lHeader, nColSpan ) CLASS ZTableCell

   ::Super:New( oRow, oRow:oOwner, .T. )

   ::xValue   := xValue
   ::lHeader  := lHeader
   ::nColSpan := nColSpan

   IF lHeader
      ::cField := xValue
      AAdd( oRow:oParent:aHeaders, Self )
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD HtmlTagIni() CLASS ZTableCell

   IF ::lHeader
      ::cTag := "th"
   ELSE
      ::cTag := "td"
   ENDIF

RETURN ::Super:HtmlTagIni()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZTableCell

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

METHOD cHeader( cValue ) CLASS ZTableCell

   IF PCount() > 0 .AND. ::lHeader
      ::xValue := cValue
   ENDIF

RETURN ::xValue

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZTableZone FROM WBasic
PUBLISHED:
   DATA nRows    INIT 0
   DATA nType    INIT 1 // 1 Header, 2 Footer, 3 Body

RESERVED:
   METHOD HtmlTagIni()

PROTECTED:
   DATA cTag      INIT ""
ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagIni() CLASS ZTableZone

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

CLASS ZTableTask
RESERVED:
   DATA oParent
   DATA oForm
   DATA cDocSection  INIT ""
   DATA cType        INIT "service" READONLY
   DATA lAppend   INIT .F.

   METHOD New( oParent, cSection, lAppend ) CONSTRUCTOR
   METHOD End()
   METHOD Preprocess()
   METHOD Html()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cSection, lAppend ) CLASS ZTableTask

   DEFAULT lAppend TO .F.

   ::oParent  := oParent
   ::cDocSection := cSection
   ::lAppend  := lAppend

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZTableTask

   ::oParent := NIL
   ::oForm   := NIL

RETURN nil

//------------------------------------------------------------------------------

METHOD Html() CLASS ZTableTask

   LOCAL cJs

   ::Preprocess()

   cJs := 'xa_showSection("' + ::cDocSection + '", true, "' + ::oForm:cId + ;
          '", "' +  ::oParent:cId + '", ' + ;
          IIF( ::lAppend, 'true', 'false' ) + ');'

RETURN cJs

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS ZTableTask

   LOCAL oSection

   oSection := Document:GetSection( ::cDocSection )

   IF !HB_IsObject( oSection )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "DocSection not loaded: '" + ::cDocSection + "'."
         :Operation   := "WTable:Edit( cDocSection )"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ELSE
      ::oForm := oSection:FirstForm()
      IF !HB_IsObject( ::oForm )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := "Form object not found on Section: '" + ::cDocSection + "'."
            :Operation   := "WTable:Edit( cDocSection )"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ELSE
      ENDIF
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------
