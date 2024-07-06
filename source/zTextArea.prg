/*
 * Proyect: XailerWeb framework
 * File: ZTextArea.prg
 * Description: Basic text HTML class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 *
 * Important note:
 * No PERSISTENT clause admited on this control since its easy to bypass the
 * 4 kb limit of cookies. Its value should be retrieved from a form using
 * submit POST method
 */

#include "xailerweb.ch"

CLASS ZTextArea FROM WControl
EXPORTED:
   DATA cText        INIT ""
   DATA cValue       INIT "" //PERSISTENT
   DATA cPlaceHolder INIT ""
   DATA cWrap        INIT "" VALUES "hard", "soft"

   DATA nRows        INIT NIL
   DATA nCols        INIT NIL
   DATA nMaxLength   INIT NIL

   DATA lReadOnly    INIT .F.

PROTECTED:
   DATA cTag         INIT "textarea"

RESERVED:
   METHOD HtmlTagBody()
ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZTextArea

   LOCAL cHtml := ""

   IF !Empty( ::cPlaceHolder )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   IF HB_IsNumeric( ::nMaxLength )
      cHtml += ' maxlength="' + ToString( ::nMaxLength ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nRows )
      cHtml += ' rows="' + ToString( ::nRows ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nCols )
      cHtml += ' cols="' + ToString( ::nCols ) + '"'
   ENDIF

   IF !Empty( ::cWrap )
      cHtml += ' wrap="' + ::cWrap + '"'
   ENDIF

   IF ::lReadOnly
      cHtml += ' readonly'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
