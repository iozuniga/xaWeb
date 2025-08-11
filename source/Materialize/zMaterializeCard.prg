/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeCard.prg
 * Descripción: class for Materialize Cards
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatCard

CLASS ZCard FROM WDiv
PUBLISHED:
   DATA oDivContent     AS CLASS WDiv
   DATA oDivImage       AS CLASS WDiv
   DATA oDivAction      AS CLASS WDiv
   DATA oImage          AS CLASS WImage
   DATA oButton         AS CLASS WButton
   DATA oTitle          AS CLASS WSpan
   DATA oText           AS CLASS WParagraph
   DATA cSize           INIT "" VALUES "small", "medium", "large"
   DATA cRevealTitle    INIT ""
   DATA cRevealText     INIT ""
   DATA lTitleOnImage   INIT .F.
   DATA lHorizontal     INIT .F.
   DATA lStacked        INIT .F.
   DATA lReveal         INIT .F.

   DATA oDivStacked     AS CLASS WDiv        // only availabe OnPreprocess() event
   DATA oDivReveal      AS CLASS WDiv        // only availabe OnPreprocess() event
   DATA oTitReveal      AS CLASS WText       // only availabe OnPreprocess() event
   DATA oIcoReveal      AS CLASS WIconGoogle // only availabe OnPreprocess() event
   DATA oTxtReveal      AS CLASS WParagraph  // only availabe OnPreprocess() event
   DATA aActions        INIT {} AS CLASS WLink

   METHOD AddAction( cText, cHRef OPTIONAL )  // --> WLink

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZCard

   ::Super:New( oParent )

   ::oDivImage   := WDiv():New( Self, Self, .T. )
   ::oDivContent := WDiv():New( Self, Self, .T. )
   ::oImage      := WImage():New( ::oDivImage, Self, .T. )
   ::oTitle      := WSpan():New( ::oDivContent, Self, .T. )
   ::oButton     := WButton():New( ::oDivImage, Self, .T. )
   ::oText       := WParagraph():New( ::oDivContent, Self, .T. )
   ::oDivAction  := WDiv():New( Self, Self, .T. )

   ::AddClass( "card" )
   ::oDivImage:AddClass( "card-image" )
   ::oDivContent:AddClass( "card-content" )
   ::oTitle:AddClass( "card-title" )
   ::oDivAction:AddClass( "card-action" )
   ::oButton:cDisplayType := "floating"
   ::oButton:AddClass( "halfway-fab" )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZCard

   ::oDivImage   := NIL
   ::oDivContent := NIL
   ::oImage      := NIL
   ::oTitle      := NIL
   ::oButton     := NIL
   ::oText       := NIL
   ::oDivAction  := NIL
   ::aActions    := {}
   ::oDivStacked := NIL
   ::oDivReveal  := NIL
   ::oTitReveal  := NIL
   ::oIcoReveal  := NIL
   ::oTxtReveal  := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD AddAction( cText, cHRef ) CLASS ZCard

   LOCAL oAction

   DEFAULT cHRef TO "#"

   WITH OBJECT oAction := WLink():New( ::oDivAction, Self )
      :cText := cText
      :cHRef := cHRef
   END WITH

   AAdd( ::aActions, oAction )

RETURN oAction

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZCard

   IF ::lHorizontal
      ::AddClass( "horizontal" )
   ENDIF

   IF !Empty( ::cSize )
      ::AddClass( ::cSize )
   ENDIF

   IF Empty( ::oImage:cSrc )
      ::oDivImage:RemoveControl( ::oImage )
      ::oImage := NIL
      ::oDivImage:RemoveControl( ::oButton )
      ::oButton := NIL
   ENDIF

   IF HB_IsObject( ::oButton ) .AND. Empty( ::oButton:cText ) .AND. Empty( ::oButton:cIcon )
      ::oDivImage:RemoveControl( ::oButton )
      ::oButton := NIL
   ENDIF

   IF Len( ::aActions ) == 0
      ::RemoveControl( ::oDivAction )
      ::oDivAction := NIL
   ENDIF

   IF Empty( ::oText:cText )
      ::oDivContent:RemoveControl( ::oText )
      ::oText := NIL
   ENDIF

   IF ::lTitleOnImage .AND. HB_IsObject( ::oImage )
      ::oDivContent:RemoveControl( ::oTitle )
      ::oDivImage:InsertControl( ::oTitle )
   ENDIF

   IF ::lStacked
      ::oDivStacked := WDiv():New( Self )
      ::oDivStacked:AddClass( "card-stacked" )
      ::RemoveControl( ::oDivContent )
      ::oDivStacked:InsertControl( ::oDivContent )
      IF ::oDivAction != NIL
         ::RemoveControl( ::oDivAction )
         ::oDivStacked:InsertControl( ::oDivAction )
      ENDIF
   ENDIF

   IF ::lReveal
      ::oTitle:AddClass( "activator" )
      IF ::oImage != NIL
         ::oImage:AddClass( "activator" )
      ENDIF
      WITH OBJECT ::oDivReveal := WDiv():New( Self )
         :AddClass( "card-reveal" )
         WITH OBJECT ::oTitReveal := WSpan():New( SO, Self )
            IF !Empty( ::cRevealTitle )
               :cText := ::cRevealTitle
            ELSE
               :cText := ::oTitle:cText
            ENDIF
            :AddClass( "card-title" )
            WITH OBJECT ::oIcoReveal := WIconGoogle():New( SO, Self )
               :cText := "close"
               :AddClass( "right" )
            END WITH
         END WITH
         WITH OBJECT ::oTxtReveal := WParagraph():New( SO, Self )
            IF !Empty( ::cRevealText )
               :cText := ::cRevealText
            ELSEIF ::oText != NIL
               :cText := ::oText:cText
            ENDIF
         END WITH
      END WITH
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
