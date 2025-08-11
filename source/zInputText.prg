
 * Proyecto: xaWeb framework
 * Fichero: ZInputText.prg
 * Descripción: Input-text HTML controls base class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This class is internal. Should not be instanciated directly
 */

#include "xaWeb.ch"

CLASS ZInputText FROM WInput
PUBLISHED:
   DATA cPlaceHolder    INIT ""

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZInputText

   LOCAL cTmp := ::cPlaceHolder

   IF !Empty( cTmp ) .AND. ::NeedTranslate()
      ::cPlaceHolder := Translator():Translate( cTmp )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZInputText

   LOCAL cHtml := ""
   IF !( ::cPlaceHolder == "" )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

