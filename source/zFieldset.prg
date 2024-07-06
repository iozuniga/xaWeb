/*
 * Proyect: XailerWeb framework
 * File: ZFieldset.prg
 * Description: HTML fieldset class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "XailerWeb.ch"

CLASS ZFieldset FROM WContainer
EXPORT:
   DATA cForm     INIT ""
   DATA cName     INIT ""
   DATA lDisabled INIT .F.

PUBLIC:
   DATA oLegend   INIT nil

   METHOD cLegend( cValue ) SETGET
   METHOD New( oParent )

PROTECTED:
   DATA cTag      INIT "fieldset"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZFieldset

   ::Super:New( oParent )

   ::AddClass( "xw-fieldset" )

RETURN Self

//------------------------------------------------------------------------------

METHOD cLegend( cValue ) CLASS ZFieldset

   IF PCount() > 0
      IF !Empty( cValue )
         WITH OBJECT ::oLegend := sLegend():New( Self )
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
PUBLIC:
   DATA cTag   INIT "legend"
   DATA cText  INIT ""
ENDCLASS

//------------------------------------------------------------------------------


