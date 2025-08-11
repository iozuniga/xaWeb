/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeDropdown.prg
 * Descripción: class for Materialize dropdown
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatDropdown

CLASS ZDropdown FROM WControl
PUBLISHED:
   DATA aTriggers       INIT {} AS CLASS WLink
   DATA aItems          INIT {} AS CLASS WDropdownItem
   DATA cAlignment      INIT "" VALUES "left", "right"
   DATA lAutoTrigger    INIT .T.
   DATA lConstrainWidth INIT .T.
   DATA lCoverTrigger   INIT .T.
   DATA lCloseOnClick   INIT .T.
   DATA lHover          INIT .F.
   DATA nInDuration     INIT 150
   DATA nOutDuration    INIT 250
   DATA cOnOpenStart    INIT ""
   DATA cOnOpenEnd      INIT ""
   DATA cOnCloseStart   INIT ""
   DATA cOnCloseEnd     INIT ""

   METHOD AddTrigger( oParent AS CLASS WControl, cText, cIcon )  // --> WLink
   METHOD AddItem( cText, cIcon, chRef )  // --> WDropdownItem
   METHOD AddDivider() INLINE ::AddItem( "-" )  // --> WDropdownItem

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()   INLINE ( ::aTriggers := {}, ::aItems := {}, ::Super:End() )
   METHOD RunTrigger()
   METHOD PreProcess()

PROTECTED:
   DATA cTag      INIT "ul"
   DATA hOpt

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZDropdown

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "dropdown-content" )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZDropdown

   LOCAL oTrigger
   LOCAL hOpt := { => }
   LOCAL cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF !Empty( ::cAlignment )
      HB_HSet( hOpt, "alignment", ::cAlignment )
   ENDIF
   IF !::lAutoTrigger
      HB_HSet( hOpt, "autoTrigger", ::lAutoTrigger )
   ENDIF
   IF !::lConstrainWidth
      HB_HSet( hOpt, "contrainWidth", ::lConstrainWidth )
   ENDIF
   IF !::lCoverTrigger
      HB_HSet( hOpt, "coverTrigger", ::lCoverTrigger )
   ENDIF
   IF !::lCloseOnClick
      HB_HSet( hOpt, "closeOnClick", ::lCloseOnClick )
   ENDIF
   IF ::lHover
      HB_HSet( hOpt, "hover", ::lHover )
   ENDIF
   IF ::nInDuration != 150
      HB_HSet( hOpt, "inDuration", ::nInDuration )
   ENDIF
   IF ::nOutDuration != 250
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

   ::hOpt := hOpt

   FOR EACH oTrigger IN ::aTriggers
      ::RunTrigger( oTrigger )
   NEXT

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD RunTrigger( oTrigger ) CLASS ZDropdown

   LOCAL cJs, cId

   oTrigger:AddDataset( "target", ::cId )
   oTrigger:AddClass( "dropdown-trigger" )
   cId := oTrigger:ValidId()
   cJs := " M.Dropdown.init(document.getElementById('" + cId + "')," + HB_JsonEncode( ::hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

RETURN nil

//------------------------------------------------------------------------------

METHOD AddTrigger( oParent, cText, cIcon )  CLASS ZDropdown

   LOCAL oTrigger

   DEFAULT cIcon TO ""

   WITH OBJECT oTrigger := WLink():New( oParent, Self, .T. )
      :cText := cText
      :Create()
   END WITH

   IF !Empty( cIcon )
      WIconGoogle():New( ::oTrigger, Self, .T. ):cText := cIcon
   ENDIF

   AAdd( ::aTriggers, oTrigger )

RETURN oTrigger

//------------------------------------------------------------------------------

METHOD AddItem( cText, cIcon, chRef ) CLASS ZDropdown

   LOCAL oItem

   oItem := WDropdownItem():New( Self, cText, cIcon, chRef )

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZDropDownItem FROM WControl
PUBLISHED:
   DATA cIcon     INIT ""
   DATA cText     INIT ""
   DATA cHRef     INIT ""
   DATA oLink     AS CLASS WLink
   DATA oIcon     AS CLASS WIconGoogle

RESERVED:
   METHOD New( oParent AS CLASS WDropdown, cText OPTIONAL, cIcon OPTIONAL, chRef OPTIONAL )
   METHOD PreProcess()
   METHOD GetIconName() INLINE IIF ( HB_IsObject( ::oIcon ), ::oIcon:cText, ::cIcon )
   METHOD GetText()     INLINE IIF ( HB_IsObject( ::oLink ), ::oLink:cText, ::cText )
   METHOD GetHRef()     INLINE IIF ( HB_IsObject( ::oLink ), ::oLink:cHRef, ::cHRef )

PROTECTED:
   DATA cTag         INIT "li"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cText, cIcon, chRef ) CLASS ZDropdownItem

   ::Super:New( oParent, oParent, .T. )

   DEFAULT cText TO "", cIcon TO "", chRef TO ""

   IF cText == "-"
      cText := ""
      ::AddClass( "divider" )
      ::nTabIndex := -1
   ELSE
      ::oLink := WLink():New( Self, Self, .T. )
      ::oLink:chRef := chRef
      ::oIcon := WIconGoogle():New( ::oLink, Self, .T. )
      ::oIcon:cText := cIcon
   ENDIF

   ::cText := cText
   ::cIcon := cIcon
   ::chRef := chRef

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZDropdownItem

   IF HB_IsObject( ::oIcon )
      IF !Empty( ::cIcon )
         ::oIcon:cText := ::cIcon
         ::cIcon := ""
      ELSEIF Empty( ::oIcon:cText )
         ::oLink:RemoveControl( ::oIcon )
      ENDIF
   ENDIF

   IF HB_IsObject( ::oLink  )
      WITH OBJECT ::oLink
         :cHref := ::cHref
         :cText := ::cText
         :lTextAfter := .T.
         :Create()
      END WITH
      ::cText := ""
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

