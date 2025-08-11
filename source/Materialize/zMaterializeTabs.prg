/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeTabs.prg
 * Descripción: class for Materialbox images
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatTabs

CLASS ZTabs FROM WControl
PUBLISHED:
   DATA aItems                INIT {} AS CLASS WTabsItem
   DATA nDuration             INIT 300
   DATA cOnShow               INIT ""
   DATA lSwipeable            INIT .F.
   DATA lFixedWidth           INIT .F.
   DATA nResponsiveThreshold  INIT 0

   METHOD AddItem( cText, cTargetID )  // --> WTabsItem

RESERVED:
   DATA cTag          INIT "ul"

   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End() INLINE (::aItems := {}, ::Super:End() )
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZTabs

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "tabs" )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTabs

   LOCAL hOpt := { => }
   LOCAL cJs, cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF ::nDuration != 300
      HB_HSet( hOpt, "duration", ::nDuration )
   ENDIF
   IF ::lSwipeable
      HB_HSet( hOpt, "swipeable", ::lSwipeable )
   ENDIF
   IF !Empty( ::nResponsiveThreshold )
      HB_HSet( hOpt, "responsiveThreshold", ::nResponsiveThreshold )
   ENDIF
   IF !Empty( ::cOnShow )
      HB_HSet( hOpt, "onShow", ::cOnShow )
   ENDIF

   cJs := " M.Tabs.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

   IF ::lFixedWidth
      ::AddClass( "tabs-fixed-width" )
   ENDIF

   IF Len( ::aItems ) > 0 .AND. ( AScan( ::aItems, {|v| v:lActive } ) ) == 0
      ::aItems[ 1 ]:lActive := .T.
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD AddItem( cText, cTargetID ) CLASS ZTabs

   LOCAL oItem

   DEFAULT cText TO "", cTargetID TO ""

   oItem := WTabsItem():New( Self, cText, cTargetID )

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZTabsItem FROM WControl
PUBLISHED:
   DATA cText        INIT ""
   DATA cTargetID    INIT ""
   DATA lDisabled    INIT .F.
   DATA lActive      INIT .F.
   DATA oLink        AS CLASS WLink

RESERVED:
   METHOD New( oParent AS CLASS WTabs, cText, cTargetID )
   METHOD End() INLINE (::oLink := NIL, ::Super:End() )
   METHOD PreProcess()

PROTECTED:
   DATA cTag      INIT "li"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cText, cTargetID ) CLASS ZTabsItem

   ::Super:New( oParent, oParent, .t. )

   IF !( "#" $ cTargetID )
      cTargetID := "#" + cTargetID
   ENDIF

   WITH OBJECT ::oLink := WLink():New( Self, Self, .t. )
      :cHref := cTargetID
      :cText := cText
   END WITH

   ::AddClass( "tab" )

   ::cText     := cText
   ::cTargetID := cTargetID

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTabsItem

   WITH OBJECT ::oLink
      :cHRef := ::cTargetID
      :cText := ::cText
      ::cText := ""
   END WITH

   IF ::lDisabled
      ::oLink:AddClass( "disabled" )
   ENDIF

   IF ::lActive
      ::oLink:AddClass( "active" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
