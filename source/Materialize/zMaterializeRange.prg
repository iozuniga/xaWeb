/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeRange.prg
 * Descripción: class for Materialize range
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatRange

STATIC lDeploy := .F.

CLASS ZRange FROM WInput
PUBLISHED:
   DATA oContainer   AS CLASS WDiv
   DATA nValue       INIT NIL PROPERTY
   DATA nMax         INIT NIL
   DATA nMin         INIT NIL
   DATA nStep        INIT NIL

PROTECTED:
   DATA cType        INIT "range"

RESERVED:
   METHOD New( oParent, oOwner, lAuto )
   METHOD End() INLINE (::oContainer := NIL, ::Super:End() )

   METHOD HtmlTagBody()
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZRange

   ::oContainer := WDiv():New( oParent, Self, .t. )
   ::Super:New( ::oContainer, Self )

   ::oContainer:AddClass( "range-field" )

RETURN Self

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

METHOD PreProcess() CLASS ZRange

   LOCAL cCss

   IF !lDeploy .OR. Document:lRender2 // bug en materialize 2.1.1
      TEXT INTO cCss TABS 3
         input[type=range] {
            background-color: var(--md-sys-color-primary);
         }
      ENDTEXT
      Document:AddCss( cCss )
      lDeploy := .T.
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------


