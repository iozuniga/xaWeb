/*
 * Proyecto: xaWeb framework
 * Fichero: zMaterializeCollapsible.prg
 * Descripción: class for Materialize collapsible
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatCollapsible

CLASS ZCollapsible FROM WControl
PUBLISHED:
   DATA cType           INIT "" VALUES "accordion", "expandable", "popout"
   DATA nInDuration     INIT 300
   DATA nOutDuration    INIT 300
   DATA cOnOpenStart    INIT ""
   DATA cOnOpenEnd      INIT ""
   DATA cOnCloseStart   INIT ""
   DATA cOnCloseEnd     INIT ""
   DATA aPanels       INIT {}  AS CLASS WCollapsiblePanel

   METHOD AddPanel( cTitle, cIcon, cText )  // --> WCollapsiblePanel

RESERVED:
   DATA cTag          INIT "ul"

   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()      INLINE ( ::aPanels := {}, ::Super:End() )
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCollapsible

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "collapsible" )

RETURN Self

//------------------------------------------------------------------------------

METHOD AddPanel( cTitle, cIcon, cText ) CLASS ZCollapsible

   LOCAL oItem

   oItem := WCollapsiblePanel():New( Self, cTitle, cIcon, cText )

   AAdd( ::aPanels, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCollapsible

   LOCAL hOpt := { => }
   LOCAL cJs, cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF ::nInDuration != 300
      HB_HSet( hOpt, "inDuration", ::nInDuration )
   ENDIF
   IF ::nOutDuration != 300
      HB_HSet( hOpt, "outDuration", ::nOutDuration )
   ENDIF
   IF !Empty( ::cOnOpenStart )
      HB_HSet( hOpt, "onOpenStart", ::cOnOpenStart )
   ENDIF
   IF !Empty( ::cOnOpenEnd )
      HB_HSet( hOpt, "onOpenEnd", ::cOnOpenEnd )
   ENDIF
   IF !Empty( ::cOnCloseStart )
      HB_HSet( hOpt, "onCloseStart", ::cOnCloseStart )
   ENDIF
   IF !Empty( ::cOnCloseEnd )
      HB_HSet( hOpt, "onCloseEnd", ::cOnCloseEnd )
   ENDIF
   IF !Empty( ::cType ) .AND. ::cType != "accordion"
      ::AddClass( ::cType )
      HB_HSet( hOpt, "accordion", FALSE )
   ENDIF

   cJs := " M.Collapsible.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()

   Document:oContext:AddCodeOnLoad( cJs )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZCollapsiblePanel FROM WControl
PUBLISHED:
   DATA cTitle       INIT ""
   DATA cIcon        INIT ""
   DATA cText        INIT ""
   DATA lActive      INIT .F.

   DATA oHeader      AS CLASS WDiv
   DATA oBody        AS CLASS WDiv
   DATA oIcon        AS CLASS WIconGoogle
   DATA oHeadSpan    AS CLASS WSpan
   DATA oBodySpan    AS CLASS WSpan

RESERVED:
   METHOD New( oParent AS CLASS WCollapsible, cTitle OPTIONAL, cIcon OPTIONAL, cText OPTIONAL ) CONSTRUCTOR
   METHOD PreProcess()

RESERVED:
   DATA cTag        INIT "li"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cTitle, cIcon, cText ) CLASS ZCollapsiblePanel

   ::Super:New( oParent, oParent, .T. )

   DEFAULT cTitle TO "Title", cIcon TO "", cText TO ""

   ::cTitle := cTitle
   ::cIcon  := cIcon
   ::cText  := cText

   WITH OBJECT ::oHeader := WDiv():New( Self )
      :AddClass( "collapsible-header" )
      WITH OBJECT ::oIcon := WIconGoogle():New( SO, Self )
         :cText := cIcon
      END WITH
      WITH OBJECT ::oHeadSpan := WSpan():New( SO, Self )
         :cText := cTitle
      END WITH
   END WITH

   WITH OBJECT ::oBody := WDiv():New( Self )
      :AddClass( "collapsible-body" )
      WITH OBJECT ::oBodySpan := WSpan():New( SO, Self )
         :cText := cText
      END WITH
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCollapsiblePanel

   ::oHeadSpan:cText := ::cTitle
   ::oBodySpan:cText := ::cText

   ::cText := ""

   IF Empty( ::cIcon ) .AND. Empty( ::oIcon:cText )
      ::oHeader:RemoveControl( ::oIcon )
   ELSEIF !Empty( ::cIcon )
      ::oIcon:cText := ::cIcon
   ENDIF

   IF Empty( ::cText ) .AND. Empty( ::oBodySpan:cText )
      ::oBody:RemoveControl( ::oBodySpan )
   ENDIF

   IF ::lActive
      ::AddClass( "active" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

