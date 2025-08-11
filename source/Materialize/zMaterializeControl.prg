/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeControl.prg
 * Descripción: Base class for HTML Materialize controls
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This class is internal. Should not be instanciated directly
 *       This is part of the Materialize library. This class should not be
 *       overwritten by the end user. In case its done: method oContext()
 *       should be copied to new version for Xailer Intellisense to work
 */

#include "xaWeb.ch"

ANNOUNCE ZMatControl

CLASS ZControl FROM WContainer
PUBLISHED:
   DATA oBadge       AS CLASS WSpan // Must be created with AddBadge()
   DATA cBadge       INIT ""
   DATA cText        INIT ""
   DATA lNoPadding   INIT .F.
   DATA lBadgeNew    INIT .F.

   METHOD oContext( oValue )  SETGET AS CLASS WMaterializeHelper
   METHOD AddBadge() INLINE ::oBadge := WSpan():New( Self )  // --> WSpan

RESERVED:
   DATA cTag         INIT ""

   METHOD End()   INLINE ( ::oBadge := NIL, ::Super:End() )
   METHOD Preprocess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZControl

   IF !Empty( ::cBadge )
      IF !HB_IsObject( ::oBadge )
         ::oBadge := WSpan():New( Self )
      ENDIF
      WITH OBJECT ::oBadge
         :cText := ::cBadge
         :AddClass( IIF( ::lBadgeNew, "new badge", "badge" ) )
      END WITH
   ENDIF

   IF ::lNoPadding
      ::AddClass( "no-padding" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD oContext( oValue ) CLASS ZControl

RETURN ::Super:oContext( oValue )

//------------------------------------------------------------------------------





