/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicDateTime.prg
 * Descripción: Input-DateTime HTML base controls
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZBasicDateTime FROM WInput
PUBLISHED:
   DATA xValue          INIT NIL PERSISTENT
   DATA cType           INIT "date" VALUES "date", "datetime-local", "month", "time", "week", "hidden"
   DATA nMax            INIT NIL
   DATA nMin            INIT NIL
   DATA nStep           INIT NIL

RESERVED:
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasicDateTime

   LOCAL cHtml := ""

   IF HB_IsDate( ::xValue )
      cHtml += ' value="' +  HB_DToC( ::xValue, "YYYY-MM-DD" ) + '"'
   ELSEIF HB_IsDateTime( ::xValue )
      cHtml += ' value="' +  HB_TToC( ::xValue, "YYYY-MM-DD", "HH:MM:SS" ) + '"'
   ELSEIF HB_IsNumeric( ::xValue )
      cHtml += ' value="' + ToString( ::xValue ) + '"'
   ENDIF

   IF HB_IsDate( ::nMax )
      cHtml += ' max="' +  HB_DToC( ::nMax, "YYYY-MM-DD" ) + '"'
   ELSEIF HB_IsDateTime( ::nMax )
      cHtml += ' max="' +  HB_TToC( ::nMax, "YYYY-MM-DD", "HH:MM:SS" ) + '"'
   ELSEIF HB_IsNumeric( ::nMax )
      cHtml += ' max="' + ToString( ::nMax ) + '"'
   ENDIF

   IF HB_IsDate( ::nMin )
      cHtml += ' min="' +  HB_DToC( ::nMin, "YYYY-MM-DD" ) + '"'
   ELSEIF HB_IsDateTime( ::nMin )
      cHtml += ' min="' +  HB_TToC( ::nMin, "YYYY-MM-DD", "HH:MM:SS" ) + '"'
   ELSEIF HB_IsNumeric( ::nMin )
      cHtml += ' min="' + ToString( ::nMin ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nStep )
      cHtml += ' step="' + ToString( ::nStep ) + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
