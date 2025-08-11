/*
 * Proyecto: xaWeb framework
 * Fichero: ZFlexRow.prg
 * Descripción: HTML basic flex row class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZFlexRow FROM WControl
PUBLISHED:
   DATA cJustifyContent INIT "" VALUES "center", "flex-start", "flex-end", "space-between", "space-around", "space-evenly", "initial", "inherit"
   DATA cAlignContent   INIT "" VALUES "stretch", "center", "flex-start", "flex-end", "space-between", "space-around", "space-evenly", "initial", "inherit"
   DATA cAlignItems     INIT "" VALUES "normal", "stretch", "center", "flex-start", "flex-end", "start", "end", "baseline", "initial", "inherit"
   DATA cGap            INIT ""

PROTECTED:
   DATA cTag            INIT "div"

RESERVED:
   METHOD Preprocess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS ZFlexRow

   LOCAL cStyle:= "display: flex;"

   IF !Empty( ::cJustifyContent )
      cStyle += " justify-content: " + ::cJustifyContent + ";"
   ENDIF

   IF !Empty( ::cAlignContent )
      cStyle += " align-content: " + ::cAlignContent + ";"
   ENDIF

   IF !Empty( ::cAlignItems )
      cStyle += " align-items: " + ::cAlignItems + ";"
   ENDIF

   IF !Empty( ::cGap )
      cStyle += " gap: " + ::cGap + ";"
   ENDIF

   ::AddStyle( cStyle )

   ::AddClass( "xa-flexrow row" )

RETURN ::Super:Preprocess()

//------------------------------------------------------------------------------

