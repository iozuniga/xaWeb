/*
 * Proyecto: xaWeb framework
 * Fichero: ZCmpButtonIcon.prg
 * Descripción: button + icon component
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Referred to https://www.youtube.com/@dcode-software
 */

#include "xaWeb.ch"
#include "error.ch"

#define URL_IONICONS_ESM "https://unpkg.com/ionicons@7.1.0/dist/ionicons/ionicons.esm.js"
#define URL_IONICONS_NOR "https://unpkg.com/ionicons@7.1.0/dist/ionicons/ionicons.js"

STATIC lScript := .F.

CLASS ZCmpButtonIcon FROM WButton
PUBLISHED:
   DATA cIcon     INIT ""
   DATA cVariant  INIT "" VALUES "", "success", "error"

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag      INIT "button-icon"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCmpButtonIcon

   LOCAL cJs

   IF !lScript .or. Document:lRender2
      TEXT INTO cJs TABS 2
      import { CmpButtonIcon } from "/js/xa_CmpButtonIcon.js"
      customElements.define("button-icon", CmpButtonIcon );
      ENDTEXT

      WITH OBJECT Document:AddScript( cJs )
         :cType := "module"
         :lBottom := .F.
      END WITH

      WITH OBJECT Document:AddScript( URL_IONICONS_ESM )
         :cType := "module"
         :lBottom := .T.
      END WITH

      WITH OBJECT Document:AddScript( URL_IONICONS_NOR )
         :lNoModule := .T.
      END WITH

      lScript := .T.
   ENDIF

RETURN ::Super:New( oParent, oOwner, lAuto )

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZCmpButtonIcon

   LOCAL cHtml := ""

   IF !Empty( ::cIcon )
      cHtml += ' data-icon="' + ::cIcon + '"'
   ENDIF

   IF !Empty( ::cVariant )
      cHtml += ' data-variant="' + ::cVariant + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
