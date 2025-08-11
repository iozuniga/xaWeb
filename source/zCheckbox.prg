/*
 * Proyecto: xaWeb framework
 * Fichero: ZCheckbox.prg
 * Descripción: Base class for checkbox HTML controls
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZCheckbox FROM WBasicCheckbox
PUBLISHED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA cLabel          INIT ""

   METHOD lRightCheck( lValue )  SETGET  // --> lValue

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCheckbox

   ::oContainer := WDiv():New( oParent, Self, .T. )
   ::Super:New( ::oContainer, oParent, lAuto )
   ::oLabel := WLabel():New( ::oContainer, Self, .T. )

   ::AddClass( "xa-input__input" )
   ::oContainer:AddClass( "xa-input" )
   ::oLabel:AddClass( "xa-input__label" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZCheckbox

   ::oContainer := NIL
   ::oLabel     := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD lRightCheck( lValue )  CLASS ZCheckBox

   LOCAL nAt1, nAt2

   nAt1 := AScan( ::oContainer:aControls, {|oCtl| oCtl == Self } )
   nAt2 := AScan( ::oContainer:aControls, {|oCtl| oCtl == ::oLabel } )

   IF PCount() > 0
      IF ( lValue .AND. ( nAt1 < nAt2 ) ) .OR. ( !lValue .AND. ( nAt1 > nAt2 ) )
         ::oContainer:SwapControls( nAt1, nAt2 )
      ENDIF
   ELSE
      lValue := ( nAt1 > nAt2 )
   ENDIF

RETURN lValue

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCheckbox

   LOCAL cId

   cId := ::ValidId()

   IF !Empty( ::cLabel )
      ::oLabel:cText := ::cLabel
   ENDIF

   IF Empty( ::oLabel:cText )
      ::oContainer:RemoveControl( ::oLabel )
      ::oLabel := NIL
   ELSE
      WITH OBJECT ::oLabel
         :cFor := cId
      END WITH
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
