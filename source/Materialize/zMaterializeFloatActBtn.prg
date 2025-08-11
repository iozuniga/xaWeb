/*
 * Proyecto: xaWeb framework
 * Fichero: zMaterializeFloatActBtn.prg
 * Descripción: class for Materialize floating action buttons
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatFloatActBtn

CLASS ZFloatActionButton FROM WDiv
PUBLISHED:
   DATA cDirection      INIT "" VALUES "top", "right", "bottom", "left"
   DATA cIcon           INIT ""
   DATA cColor          INIT ""
   DATA lLarge          INIT .T.
   DATA lHoverEnabled   INIT NIL
   DATA lToolbarEnabled INIT NIL

   DATA oUl           AS CLASS WDiv
   DATA oLink         AS CLASS WLink
   DATA oIcon         AS CLASS WIconGoogle
   DATA aItems        INIT {} AS CLASS WFloatActionButtonItem

   METHOD AddItem( chRef, cIcon, cColor )  // --> WFloatActionButtonItem

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End() INLINE ( ::oLink := NIL, ::oIcon := NIL, ::aItems := {}, ::Super:End() )
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZFloatActionButton

   ::Super:New( oParent )
   ::AddClass( "fixed-action-btn" )

   ::oLink := WLink():New( Self, Self, .t. )
   ::oIcon := WIconGoogle():New( ::oLink, Self, Self, .t. )
   ::oUl   := WDiv():New( Self, Self, .t. )

   ::oUl:cTag := "ul"

   ::oLink:AddClass( "btn-floating" )

RETURN Self

//------------------------------------------------------------------------------

METHOD AddItem( cHref, cIcon, cColor ) CLASS ZFloatActionButton

   LOCAL oItem

   oItem := WFloatActionButtonItem():New( ::oUl, cHref, cIcon, cColor )

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZFloatActionButton

   LOCAL hOpt := { => }
   LOCAL cJs, cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF !Empty( ::cDirection )
      HB_HSet( hOpt, "edge", ::cEdge )
   ENDIF

   IF ::lHoverEnabled != nil
      HB_HSet( hOpt, "hoverEnabled", ::lHoverEnabled )
   ENDIF

   IF ::lToolbarEnabled != nil
      HB_HSet( hOpt, "toolbarEnabled", ::lToolbarEnabled )
   ENDIF

   cJs := " M.FloatingActionButton.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()

   Document:oContext:AddCodeOnLoad( cJs )

   IF ::lLarge
      ::oLink:AddClass( "btn-large" )
   ENDIF

   WITH OBJECT ::oIcon
      :cText  := ::cIcon
      :cColor := ::cColor
   END WITH

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZFloatActionButtonItem FROM WControl
PUBLISHED:
   DATA cHref     INIT ""
   DATA cIcon     INIT ""
   DATA cColor    INIT ""

   DATA oLink        AS CLASS WLink
   DATA oIcon        AS CLASS WIconGoogle

RESERVED:
   METHOD New( oParent AS CLASS wFloatActionButton, chRef, cIcon, cColor ) CONSTRUCTOR
   METHOD PreProcess()

PROTECTED:
   DATA cTag         INIT "li"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cHref, cIcon, cColor ) CLASS ZFloatActionButtonItem

   ::Super:New( oParent, oParent, .t. )

   DEFAULT cHref TO "#", cIcon TO "", cColor TO ""

   ::cHref  := chref
   ::cIcon  := cIcon
   ::cColor := cColor

   WITH OBJECT ::oLink := WLink():New( Self, Self, .t. )
      :AddClass( "btn-floating" )
      :cHref := ::cHref
   END WITH

   WITH OBJECT ::oIcon := WIconGoogle():New( ::oLink, Self, .t. )
      :cText := ::cIcon
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZFloatActionButtonItem

   LOCAL lEmptyRef := Empty( ::cHRef ) .OR. ::cHRef == "#"

   IF !lEmptyRef
      ::oLink:cHref := ::cHref
      ::cHref := ""
   ENDIF

   IF !Empty( ::cColor )
      ::oLink:AddClass( ::cColor )
   ENDIF

   IF !Empty( ::cIcon )
      ::oIcon:cText := ::cIcon
      ::cIcon := ""
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

