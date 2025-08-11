/*
 * Proyecto: xaWeb framework
 * Fichero: ZIconGoogle.prg
 * Descripción: HTML Icon Google class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

#define URL_ICONS    "https://fonts.googleapis.com/icon?family=Material+Icons"

STATIC lLoaded := .F.

CLASS ZIconGoogle FROM WControl
PUBLISHED:
   DATA cText
   DATA cColor
   DATA cTextColor
   DATA nSize     INIT NIL VALUES 18, 24, 36, 48
   DATA lDark     INIT NIL
   DATA lLight    INIT NIL
   DATA lDisabled INIT NIL

RESERVED:
   METHOD End()
   METHOD Preprocess()

PROTECTED:
   DATA cTag      INIT "i"

ENDCLASS

//------------------------------------------------------------------------------

METHOD End() CLASS ZIconGoogle

   lLoaded := .F.

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZIconGoogle

   LOCAL cClass, cCss

   cClass := "material-icons"

   TEXT INTO cCss
   .material-icons.md-18 { font-size: 18px; }
   .material-icons.md-24 { font-size: 24px; }
   .material-icons.md-36 { font-size: 36px; }
   .material-icons.md-48 { font-size: 48px; }
   .material-icons.md-dark { color: rgba(0, 0, 0, 0.54); }
   .material-icons.md-dark.md-inactive { color: rgba(0, 0, 0, 0.26); }
   .material-icons.md-light { color: rgba(255, 255, 255, 1); }
   .material-icons.md-light.md-inactive { color: rgba(255, 255, 255, 0.3); }
   ENDTEXT

   IF !Empty( ::nSize )
      cClass += " md-" + ToString( ::nSize )
   ENDIF

   IF !Empty( ::lDark )
      cClass += " md-dark"
   ENDIF

   IF !Empty( ::lLight )
      cClass += " md-light"
   ENDIF

   IF !Empty( ::lDisabled )
      cClass += " md-inactive"
   ENDIF

   IF !lLoaded .OR. Document:lRender2
      Document:AddCSS( URL_ICONS )
      Document:AddCSS( cCss, "IconGoogle" )
      lLoaded := .T.
   ENDIF

   IF !Empty( ::cColor )
      cClass += " " + ::cColor
   ENDIF

   IF !Empty( ::cTextColor )
      cClass += " " + ::cTextColor + "-text"
   ENDIF

   ::AddClass( cClass )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

