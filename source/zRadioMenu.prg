/*
 * Proyecto: xaWeb framework
 * Fichero: ZRadioMenu.prg
 * Descripción: HTML Div class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZRadioMenu FROM WControl

PUBLISHED:
   DATA cName           INIT ""
   DATA nSelected       INIT 0
   DATA lHorizontal     INIT .F.

   METHOD AddRadio( cLabel OPTIONAL, cValue OPTIONAL, cId OPTIONAL )  // --> WRadio
   METHOD aItems( aValues ) SETGET AS CLASS WRadio

   EVENT OnValidate( JsEvent )

PROTECTED:
   DATA faItems      INIT {}
   DATA cTag         INIT "div"

RESERVED:
   METHOD New( oParent, oOwner, lAuto )         CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZRadioMenu

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "xa-input" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZRadioMenu

   ::faItems := {}

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD AddRadio( cLabel, cValue, cId ) CLASS ZRadioMenu

   LOCAL oRadio
   LOCAL nId := 1

   DEFAULT cLabel TO "", cValue TO "", cId TO ""

   WITH OBJECT oRadio := WRadio():New( Self, Self, .T. )
      :cLabel := cLabel
      :cValue := cValue
      IF !Empty( cId )
         :cId := cId + "-" + ToString( nId++ )
      ENDIF
      :oContainer:DelClass( "xa-input" )
      :Create()
   END WITH

   AAdd( ::faItems, oRadio )

RETURN oRadio

//------------------------------------------------------------------------------

METHOD aItems( aValues ) CLASS ZRadioMenu

   LOCAL cVal

   IF HB_IsString( aValues )
      IF At( WIN_EOL, aValues ) > 0
         aValues := HB_ATokens( aValues, WIN_EOL )
      ELSEIF At( hb_eol(), aValues ) > 0
         aValues := HB_ATokens( aValues, hb_eol() )
      ELSEIF At( ",", aValues ) > 0
         aValues := HB_ATokens( aValues, "," )
      ELSEIF At( ";", aValues ) > 0
         aValues := HB_ATokens( aValues, ";" )
      ELSE
         aValues := { aValues }
      ENDIF
   ENDIF

   IF PCount() > 0
      AEval( ::faItems, {|v| v:End() } )
      IF Len( aValues ) > 0
         FOR EACH cVal IN aValues
            ::AddRadio( cVal, ToString( cVal:__EnumIndex() ) )
         NEXT
      ENDIF
   ENDIF

RETURN ::faItems

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZRadioMenu

   LOCAL oRad
   LOCAL cId, cValid
   LOCAL n

   cId := ::ValidId()

   IF Empty( ::cName )
      ::cName := cId
   ENDIF

   IF ::IsEvent( "OnValidate" )
      cValid := ::EventValue( "OnValidate" )
      ::EventDelete( "OnValidate" )
   ENDIF

   IF ::lHorizontal
      ::AddStyle( "display:flex;" )
      IF !::IsStyle( "gap" )
         ::oStyle:Gap := "1em"
      ENDIF
   ELSE
      ::AddStyle( "display:flex;flex-direction:column;" )
      IF !::IsStyle( "gap" )
         ::oStyle:Gap := "0.5em"
      ENDIF
   ENDIF

   FOR EACH oRad IN ::faItems
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
   NEXT

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
