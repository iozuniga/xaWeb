/*
 * Proyecto: xaWeb framework
 * Fichero: zBasicSelect.prg
 * Descripción: HTMLSelect class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZBasicSelect FROM WControl
PUBLISHED:
   DATA cValue       INIT "" PERSISTENT
   DATA cForm        INIT ""
   DATA cName        INIT ""
   DATA nSize        INIT NIL
   DATA lAutofocus   INIT .F.
   DATA lDisabled    INIT .F.
   DATA lMultiple    INIT .F.
   DATA lRequired    INIT .F.

   METHOD AddItem( cValue, cLabel )  // --> WSelectItem
   METHOD AddGroup( cLabel )  INLINE ::AddItem( nil, cLabel )  // --> WSelectItem
   METHOD aItems( aValues ) SETGET AS CLASS WSelectItem

PROTECTED:
   DATA cTag         INIT "select"
   DATA faItems      INIT {}

RESERVED:
   METHOD End()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD End() CLASS ZBasicSelect

   ::faItems := {}

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD aItems( aValues ) CLASS ZBasicSelect

   LOCAL xVal, aItems

   IF HB_IsString( aValues )
      IF At( WIN_EOL, aValues ) > 0
         aValues := HB_ATokens( aValues, WIN_EOL )
      ELSEIF At( hb_eol(), aValues ) > 0
         aValues := HB_ATokens( aValues, hb_eol() )
      ELSEIF At( ",", aValues ) > 0
         aValues := HB_ATokens( aValues, "," )
      ELSEIF At( ";", aValues ) > 0
         aValues := HB_ATokens( aValues, ";" )
      ELSE
         aValues := { aValues }
      ENDIF
   ENDIF

   IF PCount() > 0
      aItems := {}
      IF Len( aValues ) > 0
         IF HB_IsArray( aValues[ 1 ] )
            FOR EACH xVal IN aValues
               AAdd( aItems, WSelectItem():New( Self, xVal[ 1 ], xVal[ 2 ], .T. ) )
            NEXT
         ELSE
            FOR EACH xVal IN aValues
               AAdd( aItems, WSelectItem():New( Self, ToString( xVal:__EnumIndex() ),;
                     xVal, .T. ) )
            NEXT
         ENDIF
      ENDIF
      ::faItems := aItems
   ENDIF

RETURN ::faItems

//------------------------------------------------------------------------------

METHOD AddItem( cValue, cLabel ) CLASS ZBasicSelect

   LOCAL oItem

   oItem := WSelectItem():New( Self, cValue, cLabel, .F. )

   AAdd( ::faItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicSelect

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cForm + '"'
   ENDIF

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   IF ::lAutofocus
      cHtml += ' autofocus'
   ENDIF

   IF ::lDisabled
      cHtml += ' disabled'
   ENDIF

   IF ::lMultiple
      cHtml += ' multiple'
   ENDIF

   IF ::lRequired
      cHtml += ' required'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZBasicSelectItem FROM WControl
PUBLISHED:
   DATA aItems       INIT {} READONLY AS CLASS WSelectItem
   DATA cText        INIT ""
   DATA cLabel       INIT ""
   DATA cValue       INIT ""
   DATA lDisabled    INIT .F.
   DATA lSelected    INIT .F.

   METHOD AddItem( cValue, cLabel )  // --> WBasicSelectitem

RESERVED:
   METHOD New( oParent, cValue, cLabel, lAuto )  // --> Self

PROTECTED:
   DATA cTag         INIT "option"

   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cValue, cText, lAuto ) CLASS ZBasicSelectItem

   ::Super:New( oParent, oParent, lAuto )

   ::cValue := cValue

   IF ::cValue == NIl
      ::cTag := "optgroup"
      ::cLabel := cText
   ELSE
      ::cText := cText
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD AddItem( cValue, cLabel ) CLASS ZBasicSelectItem

   LOCAL oItem

   oItem := WSelectItem():New( Self, cValue, cLabel )

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicSelectItem

   LOCAL cHtml := "", cValue := ::cValue
   LOCAL lSelected := .f.

   IF !Empty( cValue )
      cHtml += ' value="' + cValue + '"'
      lSelected := ( cValue == ::oParent:cValue )
   ENDIF

   IF !Empty( ::cLabel )
      cHtml += ' label="' + ::cLabel + '"'
   ENDIF

   IF ::lDisabled
      cHtml += ' disabled'
   ENDIF

   IF ::lSelected .OR. lSelected
      cHtml += ' selected'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
