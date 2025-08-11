/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicEdit.prg
 * Descripción: class for Input type basic text HTML elements
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZBasicEdit FROM WInputText
PUBLISHED:
   DATA cType           INIT "text" VALUES "text", "password", "tel", "url", "search", "hidden"
   DATA cText           INIT ""
   DATA cValue          INIT "" PERSISTENT
   DATA cPattern        INIT ""
   DATA cPicture        INIT ""
   DATA nSize           INIT NIL
   DATA nMaxLength      INIT NIL
   DATA nMinLength      INIT NIL

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZBasicEdit

   IF !Empty( ::cPicture )
      InputMask():SetMask( Self, ::cPicture )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicEdit

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF !Empty( ::cPattern )
      cHtml += ' pattern="' + ::cPattern + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMaxLength )
      cHtml += ' maxlength="' + ToString( ::nMaxLength ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMinLength )
      cHtml += ' minlength="' + ToString( ::nMinLength ) + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

