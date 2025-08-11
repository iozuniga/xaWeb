/*
 * Proyecto: xaWeb framework
 * Fichero: zMaterializePagination.prg
 * Descripción: class for Materialize pagination buttons
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatPagination

CLASS ZPagination FROM WControl
PUBLISHED:
   DATA cWaveEffect  INIT "" VALUES "", "normal", "light"
   DATA cIconLeft    INIT "chevron_left"
   DATA cIconRight   INIT "chevron_right"
   DATA aItems       INIT {} AS CLASS WPaginationItem
   DATA nPage        INIT 1

   EVENT OnPageChange( JsEvent )

   METHOD AddItem( cText, lIcon )  // --> WPaginationItem

RESERVED:
   DATA cTag          INIT "ul"

   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZPagination

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "pagination" )

RETURN Self

//------------------------------------------------------------------------------

METHOD AddItem( cText, lIcon ) CLASS ZPagination

   LOCAL oItem := WPaginationItem():New( Self, cText, lIcon )

   AAdd( ::aItems, oItem )

   oItem:SetPage( Len( ::aItems ) )

RETURN oItem

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZPagination

   LOCAL oItem
   LOCAL cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   oItem := WPaginationItem():New( Self, ::cIconLeft, .T. )

   HB_AIns( ::aControls, 1, oItem, .F. )

   ::AddItem( ::cIconRight, .T. )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZPaginationItem FROM WControl
PUBLISHED:
   DATA cText     INIT ""
   DATA nPage     INIT 0 READONLY
   DATA lIcon     INIT .F.
   DATA lActive   INIT .F.
   DATA lDisabled INIT .F.
   DATA oLink     AS CLASS WLink
   DATA oIcon     AS CLASS WIconGoogle

RESERVED:
   METHOD New( oParent AS CLASS WPagination, cText OPTIONAL, lIcon OPTIONAL) CONSTRUCTOR
   METHOD End() INLINE ( ::oLink := NIL, ::oIcon := NIL, ::Super:End() )
   METHOD PreProcess()
   METHOD SetPage( nPage ) INLINE ::nPage := nPage

PROTECTED:
   DATA cTag      INIT "li"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cText, lIcon ) CLASS ZPaginationItem

   ::Super:New( oParent, oParent, .T. )

   DEFAULT cText TO "", lIcon TO .F.

   ::cText  := cText
   ::lIcon  := lIcon

   WITH OBJECT ::oLink := WLink():New( Self, Self, .T. )
      :cHref := "#"
   END WITH

   IF ::lIcon
      ::oIcon := WIconGoogle():New( ::oLink, Self, Self, .T. )
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZPaginationItem

   LOCAL cClass

   IF ::lIcon
      ::oIcon:cText := ::cText
   ELSE
      ::oLink:cText := ::cText
   ENDIF

   ::cText := ""

   WITH OBJECT ::oParent
      IF !Empty( :cWaveEffect )
         cClass := "waves-effect"
         IF :cWaveEffect == "light"
            cClass += " waves-light"
         ENDIF
         ::AddClass( cClass )
      ENDIF
   END WITH

   IF ::lActive
      ::AddClass( "active" )
   ENDIF

   IF ::lDisabled
      ::AddClass( "disabled" )
   ENDIF

   ::OnClick := "<script>xa_triggerEvent('pagechange', '" + ::oParent:cId + "')</script>"

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

