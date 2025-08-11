/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeCollection.prg
 * Descripción: class for Materialize collections
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatCollection

CLASS ZCollection FROM WControl
PUBLISHED:
   DATA aItems    INIT {} AS CLASS WCollectionItem

   METHOD AddHeader( cText, nHeaderSize )  // --> WCollectionItem
   METHOD AddItem( cText, cUrl )  // --> WCollectionItem

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()   INLINE (::aItems := {}, ::Super:End() )

PROTECTED:
   DATA cTag      INIT "ul"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCollection

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "collection" )

RETURN Self

//------------------------------------------------------------------------------

METHOD AddHeader( cText, nHeaderSize ) CLASS ZCollection

   LOCAL oItem
   LOCAL cSize

   DEFAULT nHeaderSize TO 4

   cSize := ToString( nHeaderSize )

   WITH OBJECT oItem := WCollectionItem():New( Self )
      IF !Empty( cText )
         :cText := "<h" + cSize + ">" + cText + "</h" + cSize + ">"
      ENDIF
      :lheader := .T.
   END WITH

   AAdd( ::aItems, oItem )

   ::AddClass( "with-header" )

RETURN oItem

//------------------------------------------------------------------------------

METHOD AddItem( cText, cUrl ) CLASS ZCollection

   LOCAL oItem

   WITH OBJECT oItem := WCollectionItem():New( Self )
      :cText := cText
      IF !Empty( cUrl )
         :cHref := cUrl
      ENDIF
   END WITH

   AAdd( ::aItems, oItem )

RETURN oItem

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZCollectionItem FROM WControl
PUBLISHED:
   DATA oImage       AS CLASS WImage
   DATA oIcon        AS CLASS WIconGoogle
   DATA oTitle       AS CLASS WSpan
   DATA oSubTitle    AS CLASS WSpan
   DATA oSecLink     AS CLASS WLink
   DATA oSecIcon     AS CLASS WIconGoogle
   DATA oHRef        AS CLASS WTask
   DATA oSechRef     AS CLASS WTask
   DATA cSrcImage    INIT ""
   DATA cIcon        INIT ""
   DATA cIconColor   INIT ""
   DATA cSecIcon     INIT ""
   DATA cTitle       INIT ""
   DATA cText        INIT ""
   DATA lHeader      INIT .F.
   DATA lActive      INIT .F.

   METHOD cHRef( cValue ) SETGET
   METHOD cSecHRef( cValue ) SETGET
RESERVED:
   DATA cTag         INIT "li"

   METHOD New( oParent AS CLASS WCollection, cText OPTIONAL, xRef OPTIONAL, cIcon OPTIONAL ) CONSTRUCTOR
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cText, xRef, cIcon ) CLASS ZCollectionItem

   DEFAULT cText TO "", xRef TO "", cIcon TO ""

   ::Super:New( oParent, oParent, .T. )

   ::cText := cText
   ::chRef := xRef
   ::cIcon := cIcon

   ::oImage     := WImage():New( Self, Self, .T. )
   ::oIcon      := WIconGoogle():New( Self, Self, .T. )
   ::oTitle     := WSpan():New( Self, Self, .T. )
   ::oSubTitle  := WSpan():New( Self, Self, .T. )
   ::oSecLink   := WLink():New( Self, Self, .T. )
   ::oSecIcon   := WIconGoogle():New( ::oSecLink, Self, .T. )

   ::oImage:AddClass( "circle" )
   ::oIcon:AddClass( "circle" )
   ::oTitle:AddClass( "title" )

RETURN Self

//------------------------------------------------------------------------------

METHOD cHRef( cValue ) CLASS ZCollectionItem

   IF PCount() > 0
      IF HB_IsObject( cValue )
         ::oHRef := cValue
      ELSE
         ::oHRef := WTask():Url( cValue )
      ENDIF
   ENDIF

RETURN IIF( ::oHRef != NIL, ::oHRef:Html(), "" )

//------------------------------------------------------------------------------

METHOD cSecHRef( cValue ) CLASS ZCollectionItem

   IF PCount() > 0
      IF HB_IsObject( cValue )
         ::oSecHRef := cValue
      ELSE
         ::oSecHRef := WTask():Url( cValue )
      ENDIF
   ENDIF

RETURN IIF( ::oSecHRef != NIL, ::oSecHRef:Html(), "" )

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCollectionItem

   LOCAL oDiv
   LOCAL cRef := ::cHRef, cSecHRef := ::cSecHRef
   LOCAL lAvatar := .f.

   IF ::lHeader
      ::AddClass( "collection-header" )
   ELSE
      ::AddClass( "collection-item" )
   ENDIF

   IF ::lActive
      ::AddClass( "active" )
   ENDIF

   IF !Empty( ::cSrcImage )
      lAvatar := .T.
      ::AddClass( "avatar" )
      ::oImage:cSrc := ::cSrcImage
      ::RemoveControl( ::oIcon )
   ELSEIF !Empty( ::cIcon )
      lAvatar := .T.
      ::AddClass( "avatar" )
      ::RemoveControl( ::oImage )
      WITH OBJECT ::oIcon
         :cText := ::cIcon
         :cColor := ::cIconColor
      END WITH
   ELSE
      ::RemoveControl( ::oImage )
      ::RemoveControl( ::oIcon )
      ::oImage := nil
      ::oIcon := nil
   ENDIF

   IF !Empty( ::cTitle )
      lAvatar := .T.
      ::AddClass( "avatar" )
      ::oTitle:cText := ::cTitle
      ::cTitle := ""
   ELSE
      ::RemoveControl( ::oTitle )
      ::oTitle := NIL
   ENDIF

   IF !lAvatar
      ::RemoveControl( ::oSubTitle )
      ::oSubTitle := nil
      IF !Empty( cRef )
         ::cTag := "a"
      ENDIF
   ELSE
      WITH OBJECT ::oSubTitle
         :cText := "<p>" + ::cText + "</p>"
         ::cText := ""
      END WITH
   ENDIF

   IF !Empty( cSecHRef )
      ::oSecLink:AddClass( "secondary-content" )
      ::oSecLink:cHRef := cSecHRef
      IF !Empty( ::cSecIcon )
         ::oSecIcon:cText := ::cSecIcon
      ELSE
         ::RemoveControl( ::oSecIcon )
         ::oSecIcon := NIL
      ENDIF
   ELSE
      ::oSecLink:RemoveControl( ::oSecIcon )
      ::RemoveControl( ::oSecLink )
      ::oSecIcon := NIl
      ::oSecLink := NIL
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZCollectionItem

   LOCAL cHtml := ""

   IF !Empty( ::cHRef )
      cHtml += ' href="' + ::cHRef + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
