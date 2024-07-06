/*
 * Proyect: XailerWeb framework
 * File: ZRadioMenu.prg
 * Description: HTML Div class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZRadioMenu FROM WContainer

EXPORTED:
   DATA oError          AS CLASS WDiv

   DATA aInputs         INIT {}
   DATA cName           INIT ""
   DATA cMargin         INIT ""
   DATA nSelected       INIT 0
   DATA lHorizontal     INIT .F.

   METHOD New( oParent )         CONSTRUCTOR
   METHOD AddRadio( cLabel, cValue, cId )

   EVENT OnValidate( element )

PUBLIC:
   DATA cTag INIT "div"

RESERVED:
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZRadioMenu

   ::Super:New( oParent )
   ::oError := WDiv():New( Self, nil )
   ::oError:AddClass( "xw-input__error" )
   ::AddClass( "xw-input" )

RETURN Self

//------------------------------------------------------------------------------

METHOD AddRadio( cLabel, cValue, cId ) CLASS ZRadioMenu

   LOCAL oRadio
   LOCAL nId := 1

   DEFAULT cLabel TO "", cValue TO "", cId TO ""

   WITH OBJECT oRadio := WRadio():New( Self )
      ::RemoveComponent( oRadio )
      :cLabel := cLabel
      :cValue := cValue
      IF !Empty( cId )
         :cId := cId + "-" + ToString( nId++ )
      ENDIF
      :oContainer:DelClass( "xw-input" )
      :oError:DelClass( "xw-input__error" )
   END WITH

   AAdd( ::aInputs, oRadio )

RETURN oRadio

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZRadioMenu

   LOCAL oRad
   LOCAL cId, cValid
   LOCAL n

   IF Empty( ::cId )
      cId := ::RandomId()
   ELSE
      cId := ::cId
   ENDIF

   IF Empty( ::cName )
      ::cName := cId
   ENDIF

   AAdd( ::aControls, ::aControls[ 1 ] )
   HB_ADel( ::aControls, 1, .T. )

   IF ::IsEvent( "OnValidate" )
      cValid := ::EventValue( "OnValidate" )
      ::EventDelete( "OnValidate" )
   ENDIF

   FOR EACH oRad IN ::aInputs
      n := oRad:__EnumIndex()
      oRad:cName := ::cName
      IF !Empty( cValid )
         oRad:OnValidate := cValid
      ENDIF
      IF Empty( oRad:cId )
         oRad:cId := ::cName + "-" + ToString( n )
      ENDIF
      IF Empty( oRad:cValue )
         oRad:cValue := oRad:cId
      ENDIF
      oRad:lChecked := ( n == ::nSelected )
      IF ::lHorizontal
         ::AddStyle( "display:flex;" )
         IF !Empty( ::cMargin ) .AND. !oRad:__enumIsLast
            oRad:oLabel:AddStyle( "margin-right:" + ::cMargin + ";" )
         ENDIF
      ELSEIF !Empty( ::cMargin )
         oRad:AddStyle( "margin-top: " + ::cMargin +;
                        ";margin-bottom: " + ::cMargin + ";" )
      ENDIF
   NEXT

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------




