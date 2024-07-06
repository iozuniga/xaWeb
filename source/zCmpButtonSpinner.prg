/*
 * Proyect: XailerWeb framework
 * File: ZCmpButtonSpinner.prg
 * Description: button + spinner component
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Copyleft https://www.youtube.com/@dcode-software
 */

#include "xailerweb.ch"
#include "error.ch"

STATIC lScript := .F.

CLASS ZCmpButtonSpinner FROM WButton
EXPORTED:
   METHOD New( oParent ) CONSTRUCTOR

PROTECTED:
   DATA cTag      INIT "button-spinner"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZCmpButtonSpinner

   LOCAL cJs

   IF !lScript
      TEXT INTO cJs TABS 2
      import { CmpButtonSpinner } from "/js/xw_CmpButtonSpinner.js"
      customElements.define("button-spinner", CmpButtonSpinner );
      ENDTEXT

      WITH OBJECT Document:AddScript( cJs )
         :cType := "module"
         :lBottom := .F.
      END WITH

      lScript := .T.
   ENDIF

RETURN ::Super:New( oParent )

//------------------------------------------------------------------------------
