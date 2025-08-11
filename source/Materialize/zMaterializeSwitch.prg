/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeCheckbox.prg
 * Descripción: class for Materialize switch
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatSwitch

CLASS ZSwitch FROM WBasicCheckbox
PUBLISHED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabelLeft      AS CLASS WLabel
   DATA oLabelRight     AS CLASS WSpan
   DATA cLabelLeft      INIT ""
   DATA cLabelRight     INIT ""

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZSwitch

   ::oContainer := WDiv():New( oParent, Self, .t. )
   ::oLabelLeft := WLabel():New( ::oContainer, Self, .t. )
   ::Super:New( ::oLabelLeft, Self, lAuto )
   ::oLabelRight  := WSpan():New( ::oLabelLeft, Self, .t. )

   ::oContainer:AddClass( "switch" )
   ::oLabelRight:AddClass( "lever" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZSwitch

   ::oContainer  := NIL
   ::oLabelLeft  := NIL
   ::oLabelRight := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZSwitch

   IF !Empty( ::cLabelLeft )
      ::oLabelLeft:cText := ::cLabelLeft
   ENDIF

   IF !Empty( ::cLabelRight )
      ::oContainer:cText := ::cLabelRight
      ::oContainer:lTextAfter := .T.
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
