/*
 * Proyecto: xaWeb framework
 * Fichero: zList.prg
 * Descripción: HTML List class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZList FROM WControl
PUBLISHED:
   DATA lOrdered     INIT .F.

   METHOD AddItem( cText, cUrl, cIcon )  // --> WListItem

RESERVED:
   METHOD Preprocess()

PROTECTED:
   DATA cTag         INIT "ul"

ENDCLASS

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS ZList

   IF ::lOrdered
      ::cTag := "ol"
   ENDIF

RETURN ::Super:Preprocess()

//------------------------------------------------------------------------------

METHOD AddItem( cText, cUrl, cIcon ) CLASS ZList

   IF PCount() == 0
      RETURN ZListItem():Empty( Self )
   ENDIF

RETURN ZListItem():New( Self, cText, cUrl, cIcon )

//------------------------------------------------------------------------------

CLASS ZListItem FROM WControl
PUBLISHED:
   DATA oHRef        AS CLASS WTask
   DATA oIcon        AS CLASS WIconGoogle
   DATA oLink        AS CLASS WLink
   DATA cIcon        INIT ""
   DATA cText        INIT ""
   DATA cColor       INIT ""

   METHOD cHRef( cValue ) SETGET

PROTECTED:
   DATA cTag         INIT "li"

RESERVED:
   METHOD New( oParent, cText, xRef, cIcon ) CONSTRUCTOR
   METHOD Empty( oParent ) CONSTRUCTOR
   METHOD PreProcess()
   METHOD HtmlTagBody()

END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cText, xRef, cIcon ) CLASS ZListItem

   DEFAULT cText TO "", xRef TO "", cIcon TO ""

   ::Super:New( oParent )

   ::cText := cText
   ::chRef := xRef
   ::cIcon := cIcon

   ::oLink := WLink():New( Self, Self, .T. )
   ::oIcon := WIconGoogle():New( Self, Self, .T. )

RETURN Self

//------------------------------------------------------------------------------

METHOD Empty( oParent ) CLASS ZListItem

   ::Super:New( oParent )

RETURN Self

//------------------------------------------------------------------------------

METHOD cHRef( cValue ) CLASS ZListItem

   IF PCount() > 0
      IF HB_IsObject( cValue )
         ::oHRef := cValue
      ELSE
         ::oHRef := WTask():Url( cValue )
      ENDIF
   ENDIF

RETURN IIF( ::oHRef != NIL, ::oHRef:Html(), "" )

//------------------------------------------------------------------------------

 METHOD PreProcess() CLASS ZListItem

   LOCAL cRef := ::cHRef

   IF HB_IsObject( ::oLink )
      IF !Empty( cRef )
         ::oLink:cHRef := cRef
         ::oLink:cText := ::cText
         ::cText := ""
      ELSE
         ::RemoveControl( ::oLink )
         ::oLink := NIL
      ENDIF
   ENDIF

   IF HB_IsObject( ::oIcon )
      IF !Empty( ::cIcon )
         IF HB_IsObject( ::oLink )
            ::RemoveControl( ::oIcon )
            ::oLink:InsertControl( ::oIcon )
            ::oIcon:oParent := ::oLink
         ENDIF
         WITH OBJECT ::oIcon
            :cText := ::cIcon
            :cColor := ::cColor
         END WITH
      ELSE
         ::RemoveControl( ::oIcon )
         ::oIcon := nil
      ENDIF
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZListItem

   LOCAL cHtml := ""

   IF !Empty( ::cHRef )
      cHtml += ' href="' + ::cHRef + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

