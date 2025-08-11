/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeNavbar.prg
 * Descripción: class for Materialize Navbar
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

ANNOUNCE ZMatNavbar

CLASS ZNavbar FROM WControl
PUBLISHED:
   DATA oContainer   AS CLASS WDiv
   DATA oContent     AS CLASS WDiv
   DATA oLogo        AS CLASS WNavbarLogo
   DATA oMenu        AS CLASS WLink
   DATA oMenuIcon    AS CLASS WIconGoogle
   DATA oList        AS CLASS WList
   DATA oSidenav     AS CLASS WSidenav
   DATA aItems       INIT {}  AS CLASS WNavbarItem
   DATA cItemsAlign  INIT "right" VALUES "left", "right"
   DATA lExtended    INIT .F.
   DATA lCollapseBtn INIT .F.
   DATA lFillSidenav INIT .T.

   METHOD AddItem( cText, xRef, cIcon )  // --> WNavbarItem

RESERVED:
   DATA cTag      INIT "nav"

   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZNavbar

   ::Super:New( oParent, oOwner, lAuto )

   WITH OBJECT ::oContainer := WDiv():New( Self, Self, .t. )
      :AddClass( "nav-wrapper" )
      ::oLogo := WNavbarLogo():New( SO, Self, .t. )
      WITH OBJECT ::oMenu := WLink():New( SO, Self, .t. )
         :AddClass( "sidenav-trigger" )
         WITH OBJECT ::oMenuIcon := WIconGoogle():New( SO, Self, .t. )
            :cText := "menu"
         END WITH
      END WITH
      ::oList := WList():New( SO, Self, .t. )
      ::oList:oContext:HideOnMedAndDown()
   END WITH

   WITH OBJECT ::oContent := WDiv():New( Self, Self, .t. )
      :AddClass( "nav-content" )
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZNavbar

   ::oContainer := NIL
   ::oContent   := NIL
   ::oLogo      := NIL
   ::oMenu      := NIL
   ::oMenuIcon  := NIL
   ::oSidenav   := NIL
   ::aItems     := {}

RETURN ::Super:End()


//------------------------------------------------------------------------------

METHOD AddItem( cText, xRef, cIcon ) CLASS ZNavbar

   LOCAL oItem

   oItem := WNavbarItem():New( ::oList, cText, xRef, cIcon )

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZNavbar

   LOCAL oItem

   IF ::oLogo:Empty()
      ::oContainer:RemoveControl( ::oLogo )
   ENDIF

   IF !::oContent:IsChildren()
      ::RemoveControl( ::oContent )
   ENDIF

   IF ::lExtended
      ::AddClass( "nav-extended" )
   ENDIF

   ::oList:AddClass( ::cItemsAlign )

   IF ::lCollapseBtn
      IF ::oSidenav == NIL
         WITH OBJECT ::oSidenav := WSidenav():New( ::oParent, ::oParent, .T. )
            :ValidId()
         END WITH
      ELSE
         IF ::oSidenav:lProcessed
            WITH OBJECT ErrorNew()
               :Subsystem   := ERROR_SUBSYSTEM
               :Severity    := ES_WARNING
               :Description := "WSidenav controls should be created after its dependant WNavbar"
               :Operation   := "WNavbar:Preproces()"
               Eval( ErrorBlock(), :__WithObject(), 3 )
            END WITH
         ENDIF
      ENDIF
      IF ::lFillSidenav
         WITH OBJECT ::oSidenav
            FOR EACH oItem IN ::aItems
               WITH OBJECT ::oSidenav:AddNavbarItem( oItem )
                  oItem:oSyncro := :__WithObject()
                  oItem:oLink:AddDataset( "syncro", :oLink:ValidId() )
               END WITH
            NEXT
         END WITH
      ENDIF
      ::oMenu:AddDataset( "target", ::oSidenav:cId )
   ELSE
      ::oContainer:RemoveControl( ::oMenu )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZNavbarLogo FROM WLink
PUBLISHED:
   DATA oIcon        AS CLASS WIconGoogle
   DATA oImage       AS CLASS WImage
   DATA oSpan        AS CLASS WSpan
   DATA cAlignment   INIT "" VALUES "left", "right", "center"
   DATA cIcon        INIT ""
   DATA cSrc         INIT ""

RESERVED:
   METHOD New( oParent AS CLASS WNavbar ) CONSTRUCTOR
   METHOD Empty() INLINE Empty( ::cText ) .AND. Empty( ::cIcon ) .AND. Empty( ::cSrc )
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZNavbarLogo

   ::Super:New( oParent, oParent, .t. )

   ::oIcon  := WIconGoogle():New( Self, Self, .t. )

   WITH OBJECT ::oImage := WImage():New( Self, Self, .t. )
      :oContext:HideOnMedAndDown()
   END WITH

   WITH OBJECT ::oSpan := WSpan():New( Self, Self, .t. )
      :oStyle:Vertical_align := "top"
      :oStyle:White_space := "nowrap"
   END WITH

   ::AddClass( "brand-logo" )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZNavbarLogo

   LOCAL lMore := .F.

   IF !Empty( ::cAlignment )
      ::AddClass( ::cAlignment )
   ENDIF

   IF !Empty( ::cIcon )
      ::oIcon:cText := ::cIcon
      ::cIcon := ""
      lMore := .t.
   ELSE
      ::RemoveControl( ::oIcon )
      ::oIcon := NIL
   ENDIF

   IF !Empty( ::cSrc )
      ::oImage:cSrc := ::cSrc
      ::cSrc := ""
      IF Empty( ::oImage:cAlt )
         ::oImage:cAlt := ::cText
      ENDIF
      lMore := .t.
   ELSE
      ::RemoveControl( ::oImage )
      ::oImage := NIl
   ENDIF

   IF !Empty( ::cText )
      IF lMore
         ::oSpan:cText := ::cText
         ::cText := ""
      ELSE
         ::RemoveControl( ::oSpan )
      ENDIF
   ELSE
      ::RemoveControl( ::oSpan )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZNavbarItem FROM WControl
PUBLISHED:
   DATA oLink        AS CLASS WLink
   DATA oIcon        AS CLASS WIconGoogle
   DATA oDropdown    AS CLASS WDropdown
   DATA oSyncro      AS CLASS WControl
   DATA cText        INIT ""
   DATA cIcon        INIT ""
   DATA chRef        INIT ""
   DATA lIconRight   INIT .T.
   DATA lActive      INIT .F.

   METHOD lDisabled( lValue ) SETGET

RESERVED:
   DATA FlDisabled   INIT .F.

   METHOD New( oList AS CLASS WList, cText OPTIONAL, xRef OPTIONAL, cIcon OPTIONAL ) CONSTRUCTOR
   METHOD PreProcess()

PROTECTED:
   DATA cTag         INIT "li"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oList, cText, xRef, cIcon ) CLASS ZNavbarItem

   DEFAULT cText TO "", xRef TO "#", cIcon TO ""

   ::Super:New( oList, oList, .t. )

   ::cText := cText
   ::cIcon := cIcon

   IF HB_IsObject( xRef ) .AND. xRef:IsKindOf( "WDropdown" )
      ::oDropdown := xRef
   ELSE
      ::chRef := xRef
   ENDIF

   ::oLink := WLink():Create( Self, Self, .t. )
   ::oIcon := WIconGoogle():New( ::oLink, Self, .t. )

   ::oLink:chRef := ::cHref

RETURN Self

//------------------------------------------------------------------------------

 METHOD lDisabled( lValue ) CLASS ZNavbarItem

   IF PCount() > 0
      ::FlDisabled := lValue
      ::oLink:lDisabled := lValue
      IF HB_IsObject( ::oSyncro )
         ::oSyncro:lDisabled := lValue
      ENDIF
   ENDIF

RETURN ::FlDisabled

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZNavbarItem

   WITH OBJECT ::oLink
      IF HB_IsObject( ::oDropdown )
         ::oDropdown:RunTrigger( ::oLink )
         :cHRef := "#!"
      ELSE
         :chRef  := ::chRef
      ENDIF
      :cText  := ::cText
      ::cHref := ""
      ::cText := ""
      IF ::lActive
         :AddClass( "active" )
      ENDIF
      :Create()
   END WITH

   IF !Empty( ::cIcon )
      WITH OBJECT ::oIcon
         :cText := ::cIcon
         IF !Empty( ::oLink:cText )
            IF ::lIconRight
               :AddClass( "right" )
               :oStyle:Margin_left := "8px"
            ELSE
               :AddClass( "left" )
               :oStyle:Margin_right := "8px"
            ENDIF
         ENDIF
         ::cIcon := ""
      END WITH
   ELSE
      ::oLink:RemoveControl( ::oIcon )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

