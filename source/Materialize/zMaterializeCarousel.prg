/*
 * Proyecto: xaWeb framework
 * Fichero: zMaterializeCarousel.prg
 * Descripción: class for Materialize carousel
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

ANNOUNCE ZMatCarousel

CLASS ZCarousel FROM WDiv
PUBLISHED:
   DATA nDuration    INIT 200
   DATA nDist        INIT -100
   DATA nShift       INIT 0
   DATA nPadding     INIT 0
   DATA nNumVisible  INIT 5
   DATA lFullWidth   INIT .F.
   DATA lIndicators  INIT .F.
   DATA lNoWrap      INIT .F.
   DATA cOnCycleTo   INIT ""
   DATA lCenter      INIT .F.

   METHOD AddImage( chRef, cSource )  // --> WCarouselImage
   METHOD AddPanel( lFixed, cBkColor, cTxtColor )  // --> WCarouselPanel

   DATA aImages       INIT {} AS CLASS WCarouselImage
   DATA aPanels       INIT {} AS CLASS WCarouselPanel

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCarousel

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "carousel carousel-slider" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZCarousel

   ::aImages := {}
   ::aPanels := {}

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD AddImage( cHref, cSource ) CLASS ZCarousel

   LOCAL oItem

   IF Len( ::aPanels ) > 0
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Carousel already with panels. AddImage() not allowed"
         :Operation   := "WCarousel:AddImage() error"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

   oItem := WCarouselImage():New( Self, cHref, cSource )

   AAdd( ::aImages, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD AddPanel( lFixed, cBkColor, cTxtColor ) CLASS ZCarousel

   LOCAL oItem

   IF Len( ::aImages ) > 0
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Carousel already with images. AddPanel() not allowed"
         :Operation   := "WCarousel:AddPanel() error"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

   oItem := WCarouselPanel():New( Self, lFixed, cBkColor, cTxtColor )

   AAdd( ::aPanels, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCarousel

   LOCAL hOpt := { => }
   LOCAL cJs, cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF ::nDuration != 200
      HB_HSet( hOpt, "duration", ::nDuration )
   ENDIF
   IF ::nDist != -100
      HB_HSet( hOpt, "dist", ::nDuration )
   ENDIF
   IF ::nShift != 0
      HB_HSet( hOpt, "shift", ::nShift )
   ENDIF
   IF ::nPadding != 0
      HB_HSet( hOpt, "padding", ::nPadding )
   ENDIF
   IF ::nNumVisible != 5
      HB_HSet( hOpt, "numVisible", ::nNumVisible )
   ENDIF
   IF ::lFullWidth
      HB_HSet( hOpt, "fullWidth", ::lFullWidth )
   ENDIF
   IF ::lIndicators
      HB_HSet( hOpt, "indicators", ::lIndicators )
   ENDIF
   IF ::lNoWrap
      HB_HSet( hOpt, "noWrap", ::lNoWrap )
   ENDIF
   IF !Empty( ::cOnCycleTo )
      HB_HSet( hOpt, "onCycleTo", ::cOnCycleTo )
   ENDIF

   cJs := " M.Carousel.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()

   Document:oContext:AddCodeOnLoad( cJs )

   IF ::lCenter
      ::AddClass( "center" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZCarouselImage FROM WLink
PUBLISHED:
   DATA cSource   INIT ""
   DATA oImage   AS CLASS WImage

RESERVED:
   METHOD New( oParent AS CLASS WCarousel, chRef OPTIONAL, cSource OPTIONAL ) CONSTRUCTOR
   METHOD End() INLINE ( ::oImage := nil, ::Super:End() )
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cHref, cSource ) CLASS ZCarouselImage

   ::Super:New( oParent, oParent, .T. )

   DEFAULT cHref TO "#", cSource TO ""

   ::cHref   := cHref
   ::cSource := cSource

   ::AddClass( "carousel-item" )

   ::oImage := WImage():New( Self, Self, .T. )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCarouselImage

   IF !Empty( ::cSource )
      ::oImage:cSrc := ::cSource
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZCarouselPanel FROM WDiv
PUBLISHED:
   DATA lFixed       INIT .F.
   DATA lCenter      INIT .F.
   DATA cBkColor     INIT ""
   DATA cTxtColor    INIT ""

RESERVED:
   METHOD New( oParent AS CLASS WCarousel, lFixed OPTIONAL, cBkColor OPTIONAL, cTxtColor OPTIONAL ) CONSTRUCTOR
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, lFixed, cBkColor, cTxtColor ) CLASS ZCarouselPanel

   ::Super:New( oParent, oParent, .T. )

   DEFAULT lFixed TO .F., cBkColor TO "", cTxtColor TO ""

   ::lFixed    := lFixed
   ::cBkColor  := cBkColor
   ::cTxtColor := cTxtColor

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCarouselPanel

   IF ::lFixed
      ::AddClass( "carousel-fixed-item" )
      IF ::oParent:lIndicators
         ::AddClass( "with-indicators" )
      ENDIF
   ELSE
      ::AddClass( "carousel-item" )
   ENDIF

   IF ::lCenter
      ::AddClass( "center" )
   ENDIF

   IF !Empty( ::cBkColor )
      ::AddClass( ::cBkcolor )
   ENDIF

   IF !Empty( ::cTxtColor )
      ::AddClass( ::cTxtColor + "-text" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

