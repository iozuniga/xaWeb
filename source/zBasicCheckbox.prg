/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicCheckbox.prg
 * Descripción: Base class for checkbox HTML controls
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZBasicCheckbox FROM WInput
PUBLISHED:
   DATA cValue          INIT "" PERSISTENT
   DATA lChecked        INIT .F.

PROTECTED:
   DATA cType           INIT "checkbox"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicCheckbox

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
