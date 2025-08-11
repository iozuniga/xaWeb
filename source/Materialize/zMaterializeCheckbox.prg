/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeCheckbox.prg
 * Descripción: class for Materialize input-checkbox
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
  * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatCheckbox.
 *       This is done automatically when using the xa-materialize.ch file.
 */

#include "xaWeb.ch"

ANNOUNCE ZMatCheckbox

CLASS ZCheckbox FROM WBasicCheckbox
PUBLISHED:
   DATA oContainer      AS CLASS WLabel
   DATA oLabel          AS CLASS WSpan
   DATA cLabel          INIT ""
   DATA lFilledIn       INIT .F.

   METHOD lRightCheck( lValue )  SETGET  // --> lValue

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()

   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCheckbox

   ::oContainer := WLabel():New( oParent, Self, .T. )
   ::Super:New( ::oContainer, Self )
   ::oLabel  := WSpan():New( Self, Self, .T. )

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

   IF !Empty( ::cLabel )
      ::oLabel:cText := ::cLabel
   ENDIF

   IF ::lFilledIn
      ::Addclass( "filled-in" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
