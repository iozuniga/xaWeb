/*
 * Proyecto: xaWeb framework
 * Fichero: ZCmpButtonSpinner.prg
 * Descripción: button + spinner component
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Referred to https://www.youtube.com/@dcode-software
 */

#include "xaWeb.ch"
#include "error.ch"

STATIC lScript := .F.

CLASS ZCmpButtonSpinner FROM WButton
RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR

PROTECTED:
   DATA cTag      INIT "button-spinner"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCmpButtonSpinner

   LOCAL cJs

   IF !lScript .OR. Document:lRender2
      TEXT INTO cJs TABS 2
      import { CmpButtonSpinner } from "/js/xa_CmpButtonSpinner.js"
      customElements.define("button-spinner", CmpButtonSpinner );
      ENDTEXT

      WITH OBJECT Document:AddScript( cJs )
         :cType := "module"
         :lBottom := .F.
      END WITH

      lScript := .T.
   ENDIF

RETURN ::Super:New( oParent, oOwner, lAuto )

//------------------------------------------------------------------------------
