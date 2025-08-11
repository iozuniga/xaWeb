/*
 * Proyecto: xaWeb framework
 * Fichero: ZFieldset.prg
 * Descripción: HTML fieldset class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZFieldset FROM WControl
PUBLISHED:
   DATA cForm     INIT ""
   DATA cName     INIT ""
   DATA lDisabled INIT .F.
   DATA oLegend   INIT nil

   METHOD cLegend( cValue ) SETGET  // --> cText

PROTECTED:
   DATA cTag      INIT "fieldset"

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZFieldset

   ::Super:New( oParent, oOwner, lAuto )

   ::AddClass( "xa-fieldset" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZFieldset

   ::oLegend := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD cLegend( cValue ) CLASS ZFieldset

   IF PCount() > 0
      IF !Empty( cValue )
         WITH OBJECT ::oLegend := sLegend():New( Self, Self, .T. )
            :cTag  := "legend"
            :cText := cValue
            :Create()
         END WITH
      ELSE
         ::oLegend := NIL
      ENDIF
   ELSE
      IF ::oLegend != NIL
         cValue := ::oLegend:cText
      ELSE
         cValue := ""
      ENDIF
   ENDIF

RETURN cValue

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZFieldset

   LOCAL cHtml := ""

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cFor + '"'
   ENDIF

   IF ::lDisabled
      cHtml += ' disabled'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS sLegend FROM WBasic
   DATA cTag   INIT "legend"
   DATA cText  INIT ""
ENDCLASS

//------------------------------------------------------------------------------


