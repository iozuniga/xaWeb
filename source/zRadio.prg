/*
 * Proyect: XailerWeb framework
 * File: ZRadio.prg
 * Description: Input-Radio HTML class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZRadio FROM WInput
EXPORTED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA oError          AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA cValue          INIT "" PERSISTENT // used only for form data
   DATA lChecked        INIT .F.

   METHOD New( oParent ) CONSTRUCTOR
   METHOD lRightCheck( lValue )  SETGET

PROTECTED:
   DATA cType           INIT "radio"

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZRadio

   ::oContainer := WDiv():New( oParent, nil )
   ::Super:New( ::oContainer, Self )
   ::oLabel := WLabel():New( ::oContainer, nil )
   ::oError := WDiv():New( ::oContainer, nil )

   ::AddClass( "xw-input__input" )
   ::oContainer:AddClass( "xw-input" )
   ::oLabel:AddClass( "xw-input__label" )
   ::oError:AddClass( "xw-input__error" )

   ::oLabel:cFor := "auto"

RETURN Self

//------------------------------------------------------------------------------

METHOD lRightCheck( lValue )  CLASS ZRadio

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

METHOD PreProcess() CLASS ZRadio

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

METHOD HtmlTagBody() CLASS ZRadio

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