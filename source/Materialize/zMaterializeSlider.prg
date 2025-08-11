/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeSlider.prg
 * Descripción: class for Materialbox images
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatSlider

CLASS ZSlider FROM WDiv
PUBLISHED:
   DATA oContainer      AS CLASS WDiv
   DATA aItems          INIT {} AS CLASS WSliderItem
   DATA nDuration       INIT 500
   DATA nHeight         INIT 400
   DATA nInterval       INIT 6000
   DATA lIndicators     INIT .T.
   DATA lPauseOnFocus   INIT .T.
   DATA lPauseOnHover   INIT .T.

   METHOD AddItem()  // --> WSliderItem

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZSlider

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "slider" )

   ::oContainer := WDiv():New( Self )
   ::oContainer:AddClass( "slides" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZSlider

   ::oContainer := NIL
   ::aItems     := {}

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZSlider

   LOCAL hOpt := { => }
   LOCAL cJs, cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF ::nDuration != 500
      HB_HSet( hOpt, "duration", ::nDuration )
   ENDIF
   IF ::nHeight != 500
      HB_HSet( hOpt, "height", ::nHeight )
   ENDIF
   IF ::nInterval != 6000
      HB_HSet( hOpt, "interval", ::nInterval )
   ENDIF
   IF !::lIndicators
      HB_HSet( hOpt, "indicators", ::lIndicators )
   ENDIF
   IF !::lPauseOnFocus
      HB_HSet( hOpt, "pauseOnFocus", ::lPauseOnfocus )
   ENDIF
   IF !::lPauseOnHover
      HB_HSet( hOpt, "pauseOnHover", ::lPauseOnHover )
   ENDIF

   cJs := " M.Slider.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD AddItem() CLASS ZSlider

   LOCAL oItem

   oItem := WSliderItem():New( ::oContainer, Self )

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZSliderItem FROM WControl
PUBLISHED:
   DATA cSrc         INIT ""
   DATA cTitle       INIT ""
   DATA cText        INIT ""
   DATA cAlignment   INIT "center" VALUES "center", "left", "right"
   DATA oImage       AS CLASS WImage
   DATA oDiv         AS CLASS WDiv
   DATA oTitle       AS CLASS WText
   DATA oText        AS CLASS WText

RESERVED:
   METHOD New( oParent AS CLASS WSlider )
   METHOD End()
   METHOD PreProcess()

PROTECTED:
   DATA cTag      INIT "li"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZSliderItem

   ::Super:New( oParent, oParent, .T. )

   ::oImage := WImage():New( Self, Self, .t. )

   WITH OBJECT ::oDiv := WDiv():New( Self, Self, .t. )
      :AddClass( "caption" )
      WITH OBJECT ::oTitle := WText():New( SO, Self, .t. )
         :cTag  := "h3"
      END WITH
      WITH OBJECT ::oText := WText():New( SO, Self, .t. )
         :cTag  := "h5"
         ::AddClass( "light" )
      END WITH
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZSliderItem

   ::oImage := NIL
   ::oDiv   := NIL
   ::oTitle := NIL
   ::oText  := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZSliderItem

   ::oImage:cSrc  := ::cSrc
   ::oTitle:cText := ::cTitle
   ::oText:cText  := ::cText
   ::oDiv:AddClass( ::cAlignment + "-align" )

   ::cText := ""

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
