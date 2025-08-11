/*
 * Proyecto: xaWeb framework
 * Fichero: ZCmpNumericKeypad.prg
 * Descripción: Numeric keypad component
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

STATIC lScript := .F.

CLASS ZCmpNumericKeypad FROM WDiv
PUBLISHED:
   DATA cEditId      INIT ""
   DATA lDecimalDot  INIT .T.

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag      INIT "numeric-keypad"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCmpNumericKeypad

   LOCAL cJs

   IF !lScript .OR. Document:lRender2
      TEXT INTO cJs TABS 2
      import { CmpNumericKeypad } from "/js/xa_CmpNumericKeypad.js"
      customElements.define("numeric-keypad", CmpNumericKeypad );
      ENDTEXT

      WITH OBJECT Document:AddScript( "https://unpkg.com/ionicons@7.1.0/dist/ionicons/ionicons.esm.js" )
         :cType := "module"
      END WITH

      WITH OBJECT Document:AddScript( cJs )
         :cType := "module"
         :lBottom := .F.
      END WITH

      lScript := .T.
   ENDIF

RETURN ::Super:New( oParent, oOwner, lAuto )

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZCmpNumericKeypad

   LOCAL cHtml := ""

   IF !Empty( ::cEditId )
      cHtml += ' data-edit="' + ::cEditId + '"'
   ENDIF

   IF ::lDecimalDot
      cHtml += ' data-dot=true'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

