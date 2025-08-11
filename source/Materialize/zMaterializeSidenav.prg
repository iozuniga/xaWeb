/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeSidenav.prg
 * Descripción: class for Materialize Sidenav
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatSidenav

CLASS ZSidenav FROM WControl
PUBLISHED:
   DATA oHeader            AS CLASS WSidenavHeader
   DATA aItems    INIT {}  AS CLASS WSidenavItem
   DATA cEdge              INIT "" VALUES "left", "right", "top", "bottom"
   DATA nInDuration
   DATA nOutDuration
   DATA nDragTargetWidth
   DATA lDraggable
   DATA lPreventScrolling
   DATA lFixed

   DATA cOnOpenStart
   DATA cOnOpenEnd
   DATA cOnCloseStart
   DATA cOnCloseEnd

   METHOD AddHeader( cImage )  // --> WSidenavHeader
   METHOD AddItem( cText, cHref, cIcon, lDisabled )  // --> WSidenavItem
   METHOD AddDivider()  // --> WDiv
   METHOD AddDropdown( cText, cIcon, oDropDown )
   METHOD AddSubHeader( cText )  // --> WSidenavItem
   METHOD SidenavTrigger( oControl AS CLASS WControl )  // --> NIL
   METHOD SlideButton( oParent, cText, lShowOnLarge )  // --> WButton

PROTECTED:
   DATA cTag      INIT "ul"

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()
   METHOD AddNavbarItem( oNavbarItem )  // --> WSidenavItem

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZSidenav

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "sidenav" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZSidenav

   ::oHeader := NIL
   ::aItems  := {}

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD AddHeader( cImage ) CLASS ZSidenav

   ::oHeader := WSidenavHeader():New( Self, cImage )

RETURN ::oHeader

//------------------------------------------------------------------------------

METHOD AddItem( cText, cHref, cIcon, lDisabled ) CLASS ZSidenav

   LOCAL oItem

   DEFAULT cHref TO "#", cIcon TO "", lDisabled TO .F.

   WITH OBJECT oItem := WSidenavItem():New( Self )
      :cText := cText
      :cHref := cHref
      :cIcon := cIcon
      :lDisabled := lDisabled
   END WITH

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD AddNavbarItem( oNavbarItem ) CLASS ZSidenav

   LOCAL oItem

   WITH OBJECT oItem := WSidenavItem():New( Self )
      :SetNavBarItem( oNavbarItem )
   END WITH

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD AddDivider() CLASS ZSidenav

   LOCAL oDivider

   WITH OBJECT oDivider := WDiv():New( Self, Self, .t. )
      :AddClass( "divider" )
   END WITH

RETURN oDivider

//------------------------------------------------------------------------------

METHOD AddDropdown( cText, cIcon, oDropDown )

   LOCAL oItem

   DEFAULT cText TO "", cIcon TO ""

   WITH OBJECT oItem := WSidenavItem():New( Self )
      :cText := cText
      :cIcon := cIcon
      :oDropDown := oDropDown
   END WITH

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD AddSubHeader( cText ) CLASS ZSidenav

   LOCAL oItem

   oItem := ::AddItem( cText )

   oItem:AddClass( "subheader" )

RETURN oItem

//------------------------------------------------------------------------------

METHOD SlideButton( oParent, cText, lShowOnLarge ) CLASS ZSidenav

   LOCAL oBtn

   DEFAULT lShowOnLarge TO .F.

   WITH OBJECT oBtn := WButton():New( oParent, Self, .t. )
      :cText := cText
      :cDisplayType := "filled"
      :AddDataset( "target", ::ValidId() )
      :AddClass( "sidenav-trigger btn" )
      IF lShowOnLarge
         :AddClass( "show-on-large" )
      ENDIF
   END WITH

RETURN oBtn

//------------------------------------------------------------------------------

METHOD SidenavTrigger( oControl AS CLASS WControl ) CLASS ZSidenav

   WITH OBJECT oControl
      :AddDataset( "target", ::ValidId() )
      :AddClass( "sidenav-trigger btn" )
   END WITH

RETURN NIL

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZSidenav

   LOCAL hOpt := { => }
   LOCAL cJs, cCss, cId

   cId := ::ValidId()

   IF !Empty( ::cEdge )
      HB_HSet( hOpt, "edge", ::cEdge )
   ENDIF
   IF ::lDraggable != nil
      HB_HSet( hOpt, "draggable", ::lDraggable )
   ENDIF
   IF !Empty( ::nDragTargetWidth )
      HB_HSet( hOpt, "dragTargetWidth", ::nDragTargetWidth )
   ENDIF
   IF !Empty( ::nInDuration )
      HB_HSet( hOpt, "inDuration", ::nInDuration )
   ENDIF
   IF !Empty( ::nOutDuration )
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
   IF ::lPreventScrolling != nil
      HB_HSet( hOpt, "preventScrolling", ::lPreventScrolling )
   ENDIF

   cJs := " M.Sidenav.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

   IF !Empty( ::lFixed )
      TEXT INTO cCss TABS 3
         header, main, footer {
            padding-left: 300px;
         }

         @media only screen and (max-width : 992px) {
            header, main, footer {
            padding-left: 0;
            }
         }
      ENDTEXT

      ::AddClass( "sidenav-fixed" )
      Document:AddCss( cCss )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZSidenavHeader FROM WControl
PUBLISHED:
   DATA oDivMain      AS CLASS WDiv
   DATA oDivImage     AS CLASS WDiv
   DATA oBackground   AS CLASS WImage

   METHOD AddItem( cText, cHref OPTIONAL )  // --> WLink
   METHOD AddImage( cSrc, cHref OPTIONAL )  // --> WLink
   METHOD AddIcon( cIcon, cHref OPTIONAL )  // --> WLink

RESERVED:
   DATA aItems        INIT {}
   DATA cTag          INIT "li"

   METHOD New( oParent AS CLASS WSidenav, cImage ) CONSTRUCTOR
   METHOD End()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cImage ) CLASS ZSidenavHeader

   ::Super:New( oParent )

   WITH OBJECT ::oDivMain := WDiv():New( Self, Self, .t. )
      :AddClass( "user-view" )
      WITH OBJECT ::oDivImage := WDiv():New( ::oDivMain, Self, .t. )
         :AddClass( "background" )
         WITH OBJECT ::oBackground := WImage():New( ::oDivImage, Self, .t. )
            :cSrc := cImage
         END WITH
      END WITH
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZSidenavHeader

   ::oDivImage   := NIL
   ::oDivMain    := NIL
   ::oBackground := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD AddItem( cText, cHref ) CLASS ZSidenavHeader

   LOCAL oItem

   DEFAULT cHref TO ""

   WITH OBJECT oItem := WLink():New( ::oDivMain, Self, .t. )
      :cHref := cHref
      WITH OBJECT WSpan():New( oItem, Self, .t. )
         :cText := cText
      END WITH
   END WITH

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD AddImage( cSrc, cHref ) CLASS ZSidenavHeader

   LOCAL oItem

   DEFAULT cHref TO ""

   WITH OBJECT oItem := WLink():New( ::oDivMain, Self, .t. )
      :cHref := cHref
      WITH OBJECT WImage():New( oItem, Self, .t. )
         :cSrc := cSrc
         :AddClass( "circle" )
      END WITH
      :Create()
   END WITH

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------

METHOD AddIcon( cIcon, cHref ) CLASS ZSidenavHeader

   LOCAL oItem

   DEFAULT cHref TO ""

   WITH OBJECT oItem := WLink():New( ::oDivMain, Self, .t. )
      :cHref := cHref
      WITH OBJECT WIconGoogle():New( oItem, Self, .t. )
         :cText := cIcon
         :cHref := cHref
      END WITH
   END WITH

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZSidenavItem FROM WControl
PUBLISHED:
   DATA oLink        AS CLASS WLink
   DATA oIcon        AS CLASS WIconGoogle  // Only available at Preprocess event
   DATA oSpan        AS CLASS WSpan        // Only available at Preprocess event
   DATA oList        AS CLASS WList        // Only available at Preprocess event (dropdown list)
   DATA oDropdown    AS CLASS WDropdown
   DATA cHRef        INIT ""
   DATA cText        INIT ""
   DATA cIcon        INIT ""
   DATA cWaveEffect  INIT "" VALUES "", "normal", "light"
   DATA lCloseNav    INIT .T.

   METHOD lDisabled( lValue ) SETGET

RESERVED:
   DATA oNavbarItem  AS CLASS WNavbarItem
   DATA flDisabled   INIT .F.
   DATA cTag         INIT "li"

   METHOD New( oParent AS CLASS WSidenav ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()
   METHOD SetNavbarItem( oItem )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZSidenavItem

   ::Super:New( oParent, oParent, .t. )
   ::oLink := WLink():New( Self, Self, .t. )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZSidenavItem

   ::oLink       := NIl
   ::oIcon       := NIl
   ::oSpan       := NIL
   ::oList       := NIL
   ::oDropdown   := NIL
   ::oNavbarItem := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

 METHOD lDisabled( lValue ) CLASS ZSidenavItem

   IF PCount() > 0
      ::FlDisabled := lValue
      ::oLink:lDisabled := lValue
   ENDIF

RETURN ::FlDisabled

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZSidenavItem

   LOCAL oItem AS CLASS WDropDownItem
   LOCAL cClass, cId, cJs, cIcon

   IF HB_IsObject( ::oDropdown )
      WITH OBJECT ::oList := WList():New( Self, Self, .T. )
         cId := :ValidId()
         :AddClass( "collapsible collapsible-accordion" )
         WITH OBJECT :AddItem()
            WITH OBJECT WLink():New( SO, SO, .t. )
               :AddClass( "collapsible-header waves-effect" )
               :cText := ::cText
               :chRef := "#"
               :lTextAfter := .t.
            END WITH
            WITH OBJECT WDiv():New( SO, SO, .T. )
               :AddClass( "collapsible-body" )
               WITH OBJECT WList():New( SO, SO, .T. )
                  FOR EACH oItem IN ::oDropdown:aItems
                     IF HB_IsObject( oItem:oLink )
                        WITH OBJECT :AddItem()
                           WITH OBJECT WLink():New( SO, SO, .T. )
                              :cText := oItem:GetText()
                              :chRef := oItem:GetHRef()
                              cIcon := oItem:GetIconName()
                              IF !Empty( cIcon )
                                 :lTextAfter := .t.
                                 WITH OBJECT WSpan():New( SO )
                                    :AddClass( "material-icons" )
                                    :cText := cIcon
                                 END WITH
                              ENDIF
                           END WITH
                        END WITH
                     ENDIF
                  NEXT
               END WITH
            END WITH
         END WITH
      END WITH

      cJs := " M.Collapsible.init(document.getElementById('" + cId + "')," + HB_JsonEncode( {=>} ) + ");" + hb_eol()
      Document:oContext:AddCodeOnLoad( cJs )

      ::cText := ""
      IF HB_IsObject( ::oLink )
         ::RemoveControl( ::oLink )
         ::oLink := NIL
      ENDIF
   ELSE
      WITH OBJECT ::oLink
         IF !Empty( ::cHRef )
            :chref   := ::cHref
            ::cHRef  := ""
         ENDIF
         :lDisabled := ::lDisabled
         :Create()
      END WITH

      IF !Empty( ::cIcon )
         WITH OBJECT ::oIcon := WIconGoogle():New( ::oLink, Self )
            :cText := ::cIcon
            ::cIcon := ""
         END WITH
      ENDIF

      WITH OBJECT ::oSpan := WSpan():New( ::oLink, Self )
         :cText := ::cText
         ::cText := ""
      END WITH

      IF !Empty( ::cWaveEffect )
         cClass := " waves-effect"
         IF ::cWaveEffect == "light"
            cClass += " waves-light"
         ENDIF
         ::AddClass( cClass )
      ENDIF
      IF ::lCloseNav
         ::oLink:AddClass( "sidenav-close" )
      ENDIF
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD SetNavbarItem( oItem ) CLASS ZSidenavItem

   // Sincronización forzada con una Navbar

   WITH OBJECT oItem
      ::cText := :cText
      ::cIcon := :cIcon
      ::lDisabled := :lDisabled
      IF HB_IsObject( :oDropdown )
         ::oDropdown := :oDropdown
      ELSE
         IF :IsEvent( "OnClick" )
            ::oLink:OnClick := :EventValue( "OnClick" )
         ELSE
            ::oLink:cHRef := :cHref
         ENDIF
      ENDIF
   END WITH

RETURN NIL

//------------------------------------------------------------------------------

