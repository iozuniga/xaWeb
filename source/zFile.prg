/*
 * Proyect: XailerWeb framework
 * File: ZFile.prg
 * Description: Input-File HTML element class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZFile FROM WInput

EXPORTED:
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
