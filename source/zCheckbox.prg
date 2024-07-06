/*
 * Proyect: XailerWeb framework
 * File: ZCheckbox.prg
 * Description: Base class for checkbox HTML controls
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZCheckbox FROM WInput
EXPORTED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA oError          AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA cValue          INIT "" PERSISTENT
   DATA lChecked        INIT .F.

   METHOD New( oParent ) CONSTRUCTOR
   METHOD lRightCheck( lValue )  SETGET

PROTECTED:
   DATA cType           INIT "checkbox"

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZCheckbox

   ::oContainer := WDiv():New( oParent, NIL )
   ::Super:New( ::oContainer, Self )
   ::oLabel := WLabel():New( ::oContainer, NIL )
   ::oError := WDiv():New( ::oContainer, NIL )

   ::AddClass( "xw-input__input" )
   ::oContainer:AddClass( "xw-input" )
   ::oLabel:AddClass( "xw-input__label" )
   ::oError:AddClass( "xw-input__error" )

RETURN Self

//------------------------------------------------------------------------------

METHOD lRightCheck( lValue )  CLASS ZCheckBox

   LOCAL nAt1, nAt2

   nAt1 := AScan( ::oContainer:aControls, {|oCtl| oCtl == Self } )
   nAt2 := AScan( ::oContainer:aControls, {|oCtl| oCtl == ::oLabel } )

   IF PCount() > 0
      IF ( lValue .AND. ( nAt1 < nAt2 ) ) .OR. !lValue .AND. ( nAt1 > nAt2 )
         ::oContainer:SwapControls( nAt1, nAt2 )
      ENDIF
   ELSE
      lValue := ( nAt1 > nAt2 )
   ENDIF

RETURN lValue

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCheckbox

   LOCAL cId

   IF Empty( ::cId )
      cId := ::RandomId()
   ELSE
      cId := ::cId
   ENDIF

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

METHOD HtmlTagBody() CLASS ZCheckbox

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF ::lChecked
      cHtml += ' checked'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
