/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicNumber.prg
 * Descripción: Input-HTML type basic number class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZBasicNumber FROM WInputText
PUBLISHED:
   DATA cText           INIT ""
   DATA nValue          INIT NIL PERSISTENT
   DATA nMax            INIT NIL
   DATA nMin            INIT NIL
   DATA nStep           INIT NIL
   DATA nSize           INIT NIL

PROTECTED:
   DATA cType        INIT "number"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicNumber

   LOCAL cHtml := ""

   IF HB_IsNumeric( ::nValue )
      cHtml += ' value="' + ToString( ::nValue ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMax )
      cHtml += ' max="' + ToString( ::nMax ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMin )
      cHtml += ' min="' + ToString( ::nMin ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nStep )
      cHtml += ' step="' + ToString( ::nStep ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
