/*
 * Proyecto: xaWeb framework
 * Fichero: ZFile.prg
 * Descripción: Input-File HTML element class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZFile FROM WInput

PUBLISHED:
   DATA cAccept         INIT "" // https://www.w3schools.com/tags/att_input_accept.asp
   DATA lMultiple       INIT .F.

PROTECTED:
   DATA cType        INIT "file"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZFile

   LOCAL cHtml := ""

   IF !Empty( ::cAccept )
      cHtml += ' accept="' + ::cAccept + '"'
   ENDIF

   IF ::lMultiple
      cHtml += ' multiple'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
