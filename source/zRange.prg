/*
 * Proyect: XailerWeb framework
 * File: ZRange.prg
 * Description: Input-Range HTML class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZRange FROM WInput
EXPORTED:
   DATA nValue    INIT NIL PROPERTY
   DATA nMax      INIT NIL
   DATA nMin      INIT NIL
   DATA nStep     INIT NIL

PROTECTED:
   DATA cType        INIT "range"

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZRange

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

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
