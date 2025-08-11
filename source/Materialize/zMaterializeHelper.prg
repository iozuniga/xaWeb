/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeHelper.prg
 * Descripción: Class helper for handling Materialize context
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Referred to https://materializeweb.com/
 * Referred to https://github.com/materializecss/materialize/
 */

#include "xaWeb.ch"

#xtranslate NTF( <v>, <t>, <f> ) => IIF( <v> == NIL, "", IIF( <v>, <t>, <f> ) )

CLASS ZMaterializeHelper FROM WContextHelper
PUBLISHED:
   DATA cBkPalette INIT "" VALUES ;
     "amber lighten|darken|accent-?",;
     "blue lighten|darken|accent-?",;
     "blue-grey lighten|darken|accent-?",;
     "brown lighten|darken|accent-?",;
     "cyan lighten|darken|accent-?",;
     "teal lighten|darken|accent-?",;
     "deep-orange lighten|darken|accent-?",;
     "deep-purple lighten|darken|accent-?", ;
     "green lighten|darken|accent-?",;
     "grey lighten|darken|accent-?",;
     "indigo lighten|darken|accent-?",;
     "light-blue lighten|darken|accent-?",;
     "light-green lighten|darken|accent-?",;
     "lime lighten|darken|accent-?",;
     "orange lighten|darken|accent-?",;
     "pink lighten|darken|accent-?",;
     "purple lighten|darken|accent-?",;
     "red lighten|darken|accent-?",;
     "yellow lighten|darken|accent-?", "*"

   METHOD oTooltip( cText ) SETGET AS CLASS WTooltip

   METHOD Container() INLINE ::SetValue( "container", {""}, {""} )
   METHOD Row() INLINE ::SetValue( "row", {""}, {""} )
   METHOD Col( small, medium, large ) INLINE ::SetValue( "col", { small, medium, large }, {"s?","m?","l?"} )
   METHOD Offset( small, medium, large ) INLINE ::SetValue( "_offset", { small, medium, large }, {"offset-s?","offset-m?","offset-l?"} )
   METHOD Gap( value ) INLINE ::SetValue( "_gap", { value }, {"g-?"} )
   METHOD VerticalAlign() INLINE ::SetValue( "valign-wrapper", {""}, {""} )
   METHOD TextAlingleft() INLINE ::SetValue( "left-align", {""}, {""} )
   METHOD TextAlingCenter() INLINE ::SetValue( "center-align", {""}, {""} )
   METHOD TextAlingRight() INLINE ::SetValue( "right-align", {""}, {""} )
   METHOD TextColor( cColor ) INLINE ::SetValue( "_text-color", { cColor }, { "?-text" } )
   METHOD Hide() INLINE ::SetValue( "hide", {""}, {""} )
   METHOD HideOnSmallOnly() INLINE ::SetValue( "hide-on-small-only", {""}, {""} )
   METHOD HideOnMedOnly() INLINE ::SetValue( "hide-on-med-only", {""}, {""} )
   METHOD HideOnMedAndDown() INLINE ::SetValue( "hide-on-med-and-down", {""}, {""} )
   METHOD HideOnMedAndUp() INLINE ::SetValue( "hide-on-med-and-up", {""}, {""} )
   METHOD HideOnLargeOnly() INLINE ::SetValue( "hide-on-large-only", {""}, {""} )
   METHOD ShowOnSmall() INLINE ::SetValue( "show-on-small", {""}, {""} )
   METHOD ShowOnMedium() INLINE ::SetValue( "show-on-medium", {""}, {""} )
   METHOD ShowOnLarge() INLINE ::SetValue( "show-on-large", {""}, {""} )
   METHOD ShowOnMediumAndUp() INLINE ::SetValue( "show-on-medium-and-up", {""}, {""} )
   METHOD ShowOnMediumAndDown() INLINE ::SetValue( "show-on-medium-and-down", {""}, {""} )
   METHOD Padding( value ) INLINE ::SetValue( "_padding", { value }, {"p-?"} )
   METHOD PaddingTop( value ) INLINE ::SetValue( "_padding-top", { value }, {"pt-?"} )
   METHOD PaddingRight( value ) INLINE ::SetValue( "_padding-right", { value }, {"pr-?"} )
   METHOD PaddingBottom( value ) INLINE ::SetValue( "_padding-bottom", { value }, {"pb-?"} )
   METHOD PaddingLeft( value ) INLINE ::SetValue( "_padding-left", { value }, {"pl-?"} )
   METHOD Margin( value ) INLINE ::SetValue( "_Margin", { value }, {"m-?"} )
   METHOD MarginTop( value ) INLINE ::SetValue( "_Margin-top", { value }, {"mt-?"} )
   METHOD MarginRight( value ) INLINE ::SetValue( "_Margin-right", { value }, {"mr-?"} )
   METHOD MarginBottom( value ) INLINE ::SetValue( "_Margin-bottom", { value }, {"mb-?"} )
   METHOD MarginLeft( value ) INLINE ::SetValue( "_Margin-left", { value }, {"ml-?"} )
   METHOD Truncate() INLINE ::SetValue( "truncate", {""}, {""} )
   METHOD Hoverable() INLINE ::SetValue( "hoverable", {""}, {""} )  // ??
   METHOD ResponsiveImage() INLINE ::SetValue( "responsive-img", {""}, {""} )
   METHOD Circle() INLINE ::SetValue( "circle", {""}, {""} )
   METHOD VideoContainer() INLINE ::SetValue( "video-container", {""}, {""} )
   METHOD ResponsiveVideo() INLINE ::SetValue( "responsive-video", {""}, {""} )
   METHOD ZDepth( value ) INLINE ::SetValue( "z-depth-", { value }, {"?"} )
   METHOD Striped() INLINE ::SetValue( "striped", {""}, {""} ) // tables
   METHOD Highlight() INLINE ::SetValue( "highlight", {""}, {""} ) // tables
   METHOD Centered() INLINE ::SetValue( "centered", {""}, {""} ) // tables
   METHOD ResposiveTable() INLINE ::SetValue( "responsive-table", {""}, {""} ) // tables
   METHOD ScaleTransition( lShow ) INLINE ::SetValue( "scale-transition", { lShow }, {NTF(lShow, "scale-in", "scale-out")} )
   METHOD FlowText() INLINE ::SetValue( "flow-text", {""}, {""} )
   METHOD WavesEffect( lCircle, lLight ) INLINE ::SetValue( "waves-effect", {""}, {NTF( lCircle, "waves-circle", "") + NTF( lLight, " waves-light", "")} )

RESERVED:
   METHOD SetBkPalette( cColor )
ENDCLASS

//------------------------------------------------------------------------------

METHOD oTooltip( cText ) CLASS ZMaterializeHelper

   LOCAL oTooltip := WTooltip():New( ::oActiveControl )

   IF !Empty( cText )
      oTooltip:cText := cText
   ENDIF

RETURN oTooltip

//------------------------------------------------------------------------------

METHOD SetBkPalette( cColor ) CLASS ZMaterializeHelper

RETURN ::SetValue( "_bk-pallete", {""}, {cColor} )