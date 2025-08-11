/*
 * Proyecto: xaWeb framework
 * Fichero: ZImage.prg
 * Descripción: HTML Image class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZImage FROM WControl
PUBLISHED:
   DATA cAlt            INIT ""
   DATA cCrossOrigin    INIT "" VALUES "anonymous", "use-credentials"
   DATA cLoading        INIT "" VALUES "eager", "lazy"
   DATA cLongDesc       INIT ""
   DATA cReferrerPolicy INIT "" VALUES "no-referrer", "no-referrer-when-downgrade",;
                                       "origin", "origin-when-cross-origin",;
                                       "unsafe-url"
   DATA cSizes          INIT ""
   DATA cSrc            INIT ""
   DATA cUserMap        INIT ""

   DATA nHeight         INIT NIL
   DATA nWidth          INIT NIL

   DATA lIsMap          INIT .F.

PROTECTED:
   DATA cTag      INIT "img"

RESERVED:
   METHOD HtmlTagBody()
ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZImage

   LOCAL cHtml := ""

   IF !Empty( ::cAlt )
      cHtml += ' alt="' + ::cAlt + '"'
   ENDIF

   IF !Empty( ::cCrossOrigin )
      cHtml += ' crossorigin="' + ::cCrossOrigin + '"'
   ENDIF

   IF !Empty( ::cLoading )
      cHtml += ' loading="' + ::cLoading + '"'
   ENDIF

   IF !Empty( ::cLongDesc )
      cHtml += ' longdesc="' + ::cLongDesc + '"'
   ENDIF

   IF !Empty( ::cReferrerPolicy )
      cHtml += ' referrerpolicy="' + ::cReferrerPolicy + '"'
   ENDIF

   IF !Empty( ::cSizes )
      cHtml += ' sizes="' + ::cSizes + '"'
   ENDIF

   IF !Empty( ::cSrc )
      cHtml += ' src="' + ::cSrc + '"'
   ENDIF

   IF !Empty( ::cUserMap )
      cHtml += ' usermap="' + ::cUserMap + '"'
   ENDIF

   IF HB_IsNumeric( ::nHeight )
      cHtml += ' height="' + ToString( ::nHeight ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nWidth )
      cHtml += ' width="' + ToString( ::nWidth ) + '"'
   ENDIF

   IF ::lIsMap
      cHtml += ' ismap'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------


