/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicEmail.prg
 * Descripción: Input-Email basic HTML control class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZBasicEmail FROM WInputText
PUBLISHED:
   DATA cValue          INIT "" PERSISTENT
   DATA cText           INIT ""
   DATA nMaxLength      INIT NIL
   DATA nMinLength      INIT NIL
   DATA nSize           INIT NIL
   DATA lMultiple       INIT .F.

PROTECTED:
   DATA cType        INIT "email"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicEmail

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   IF ::lMultiple
      cHtml += ' multiple'
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