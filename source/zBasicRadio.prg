/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicRadio.prg
 * Descripción: Input-Radio basic HTML class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZBasicRadio FROM WInput
PUBLISHED:
   DATA cValue          INIT "" PERSISTENT // used only for form data
   DATA lChecked        INIT .F.

PROTECTED:
   DATA cType           INIT "radio"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicRadio

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF ::lChecked
      cHtml += ' checked'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------