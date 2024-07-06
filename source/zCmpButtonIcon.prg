/*
 * Proyect: XailerWeb framework
 * File: ZCmpButtonIcon.prg
 * Description: button + icon component
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Copyleft https://www.youtube.com/@dcode-software
 */

#include "xailerweb.ch"
#include "error.ch"

STATIC lScript := .F.

CLASS ZCmpButtonIcon FROM WButton
EXPORTED:
   DATA cIcon     INIT ""
   DATA cVariant  INIT "" VALUES "", "success", "error"

   METHOD New( oParent ) CONSTRUCTOR

RESERVED:
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag      INIT "button-icon"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZCmpButtonIcon

   LOCAL cJs

   IF !lScript
      TEXT INTO cJs TABS 2
      import { CmpButtonIcon } from "/js/xw_CmpButtonIcon.js"
      customElements.define("button-icon", CmpButtonIcon );
      ENDTEXT

      WITH OBJECT Document:AddScript( cJs )
         :cType := "module"
         :lBottom := .F.
      END WITH

      WITH OBJECT Document:AddScript( "https://unpkg.com/ionicons@7.1.0/dist/ionicons/ionicons.esm.js" )
         :cType := "module"
      END WITH
      lScript := .T.
   ENDIF

RETURN ::Super:New( oParent )

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
