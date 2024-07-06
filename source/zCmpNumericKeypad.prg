/*
 * Proyect: XailerWeb framework
 * File: ZCmpNumericKeypad.prg
 * Description: Numeric keypad component
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

STATIC lScript := .F.

CLASS ZCmpNumericKeypad FROM WDiv
EXPORTED:
   DATA cEditId      INIT ""
   DATA lDecimalDot  INIT .T.

   METHOD New( oParent ) CONSTRUCTOR

RESERVED:
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag      INIT "numeric-keypad"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZCmpNumericKeypad

   LOCAL cJs

   IF !lScript
      TEXT INTO cJs TABS 2
      import { CmpNumericKeypad } from "/js/xw_CmpNumericKeypad.js"
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

RETURN ::Super:New( oParent )

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

