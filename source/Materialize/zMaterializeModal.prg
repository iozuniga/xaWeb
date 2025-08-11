/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeDropdown.prg
 * Descripción: class for Materialize modal
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatModal

CLASS ZModal FROM WDiv
PUBLISHED:
   DATA oContent           AS CLASS WDiv
   DATA oFooter            AS CLASS WDiv
   DATA lFixedFooter       INIT .F.
   DATA lBottomSheet       INIT .F.
   DATA aLinks             INIT {} AS CLASS WLink
   DATA aButtons           INIT {} AS CLASS WButton
   DATA cStartingStop      INIT "4%"
   DATA cEndingStop        INIT "10%"
   DATA nInDuration        INIT 250
   DATA nOutDuration       INIT 250
   DATA nOpacity           INIT 0.5
   DATA lDismissible       INIT .T.
   DATA lPreventScrolling  INIT .T.
   DATA cOnOpenStart    INIT ""
   DATA cOnOpenEnd      INIT ""
   DATA cOnCloseStart   INIT ""
   DATA cOnCloseEnd     INIT ""

   METHOD AddLink( oParent AS CLASS WControl, cText, cIcon OPTIONAL )  // --> WLink
   METHOD AddButton( oParent AS CLASS WControl, cText, cIcon OPTIONAL )  // --> WButton

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZModal

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "modal" )

   WITH OBJECT ::oContent := WDiv():New( Self, Self, .t. )
      :Addclass( "modal-content" )
   END WITH

   WITH OBJECT ::oFooter := WDiv():New( Self, Self, .t. )
      :Addclass( "modal-footer" )
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZModal

   ::oContent := NIL
   ::oFooter  := NIL
   ::aLinks   := {}
   ::aButtons := {}

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZModal

   LOCAL oLink, oButton
   LOCAL hOpt := { => }
   LOCAL cJs, cId, cTriggerId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF ::cStartingStop != "4%"
      HB_HSet( hOpt, "startingStop", ::cStartingStop )
   ENDIF
   IF ::cEndingStop != "10%"
      HB_HSet( hOpt, "endingStop", ::cEndingStop )
   ENDIF
   IF !::lDismissible
      HB_HSet( hOpt, "dismissible", ::lDismissible )
   ENDIF
   IF !::lPreventScrolling
      HB_HSet( hOpt, "preventScrolling", "::lPreventScrolling" )
   ENDIF
   IF ::nInDuration != 250
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

   FOR EACH oLink IN ::aLinks
      oLink:chRef := "#" + cId
      cTriggerId := oLink:ValidId()
   NEXT

   FOR EACH oButton IN ::aButtons
      oButton:AddDataset( "target", cId )
      cTriggerId := oButton:ValidId()
   NEXT

   cJs := " M.Modal.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

   IF Empty( ::oFooter:aControls )
      ::RemoveControl( ::oFooter )
      ::oFooter := NIL
   ENDIF

   IF ::lFixedFooter
      ::AddClass( "modal-fixed-footer" )
   ENDIF

   IF ::lBottomSheet
      ::AddClass( "bottom-sheet" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD AddLink( oParent, cText, cIcon )  CLASS ZModal

   LOCAL oLink

   DEFAULT cIcon TO ""

   WITH OBJECT oLink := WLink():New( oParent, Self, .t. )
      :Addclass( "btn modal-trigger" )
      :cText := cText
      :Create()
   END WITH

   IF !Empty( cIcon )
      WIconGoogle():New( ::oLink, Self, .t. ):cText := cIcon
   ENDIF

   AAdd( ::aLinks, oLink )

RETURN oLink

//------------------------------------------------------------------------------

METHOD AddButton( oParent, cText, cIcon )  CLASS ZModal

   LOCAL oBtn

   DEFAULT cIcon TO ""

   WITH OBJECT oBtn := WButton():New( oParent, Self, .t. )
      :Addclass( "btn modal-trigger" )
      :cText := cText
      :cIcon := cIcon
   END WITH

   AAdd( ::aButtons, oBtn )

RETURN oBtn

//------------------------------------------------------------------------------

