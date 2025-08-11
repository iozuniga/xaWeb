/*
 * Proyecto: xaWeb framework
 * Fichero: ZInputImage.prg
 * Descripción: Input-Image HTML class (used as a submit button)
 *              Do not confuse with <img> tag
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZInputImage FROM WInput
PUBLISHED:
   DATA cAlt         INIT ""
   DATA cSrc         INIT ""
   DATA nHeight      INIT NIL
   DATA nWidth       INIT NIL

PROTECTED:
   DATA cType        INIT "image"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZInputImage

   LOCAL cHtml := ""

   IF !Empty( ::cAlt )
      cHtml += ' alt="' + ::cAlt + '"'
   ENDIF

   IF !Empty( ::cSrc )
      cHtml += ' src="' + ::cSrc + '"'
   ENDIF

   IF HB_IsNumeric( ::nHeight )
      cHtml += ' height="' + ToString( ::nHeight ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nWidth )
      cHtml += ' width="' + ToString( ::nWidth ) + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
