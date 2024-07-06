/*
 * Proyect: XailerWeb framework
 * File: ZLink.prg
 * Description: HTML Link class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 *
 * Nota: Es un Container, para poder incluir imagenes
 */

#include "XailerWeb.ch"

CLASS ZLink FROM WContainer
EXPORT:
   DATA cHref     INIT "#"
   DATA cTarget   INIT "" VALUES "_self", "_blank", "_parent", "_top"
   DATA cText     INIT ""
   DATA cTitle    INIT ""

PROTECTED:
   DATA cTag      INIT "a"

RESERVED:
   METHOD HtmlTagBody()
ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZLink

   LOCAL cHtml := ""

   IF !Empty( ::cHref )
      cHtml += ' href="' + ::cHref + '"'
   ENDIF

   IF !Empty( ::cTarget )
      cHtml += ' target="' + ::cTarget + '"'
   ENDIF

   IF !Empty( ::cTitle )
      cHtml += ' title="' + ::cTitle + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

