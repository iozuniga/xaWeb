/*
 * Proyect: XailerWeb framework
 * File: ZInputImage.prg
 * Description: Input-Image HTML class (used as a submit button)
 *              Do not confuse with <img> tag
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZInputImage FROM WInput
EXPORTED:
   DATA cAlt         INIT "" // https://www.w3schools.com/tags/att_input_accept.asp
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
