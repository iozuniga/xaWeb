/*
 * Proyecto: xaWeb framework
 * Fichero: ZTextArea.prg
 * Descripción: Basic text HTML class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 *
 * Important note:
 * No PERSISTENT clause admited on this control since its easy to bypass the
 * 4 kb limit of cookies. Its value should be retrieved from a form using
 * submit POST method
 */

#include "xaWeb.ch"

CLASS ZTextArea FROM WControl
PUBLISHED:
   DATA cText        INIT ""
   DATA cValue       INIT "" //PERSISTENT
   DATA cPlaceHolder INIT ""
   DATA cWrap        INIT "" VALUES "hard", "soft"
   DATA cForm        INIT ""
   DATA cName        INIT ""

   DATA nRows        INIT NIL
   DATA nCols        INIT NIL
   DATA nMaxLength   INIT NIL

   DATA lReadOnly    INIT .F.

PROTECTED:
   DATA cTag          INIT "textarea"
   DATA lEndTagForced INIT .T.

RESERVED:
   METHOD HtmlTagBody()
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTextArea

   IF !Empty( ::cPlaceHolder ) .AND. ::NeedTranslate()
      ::cPlaceHolder := Translator():Translate( ::cPlaceHolder )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZTextArea

   LOCAL cHtml := ""

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

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

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cForm + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
