/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeChip.prg
 * Descripción: class for Materialize Chip
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatChip

CLASS ZChip FROM WDiv
PUBLISHED:
   DATA oImage       AS CLASS WImage
   DATA oIcon        AS CLASS WIconGoogle
   DATA oClose       AS CLASS WIconGoogle
   DATA oText        AS CLASS WSpan
   DATA cIcon        INIT ""
   DATA cSrcImage    INIT ""
   DATA cText        INIT ""
   DATA lClose       INIT .F.
   DATA lOutlined    INIT .F.

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZChip

   ::Super:New( oParent )
   ::AddClass( "chip" )

   ::oImage := WImage():New( Self, Self, .T. )
   ::oIcon  := WIconGoogle():New( Self, Self, .T. )
   ::oText  := WSpan():New( Self, Self, .T. )
   ::oClose := WIconGoogle():New( Self, Self, .T. )

   WITH OBJECT ::oClose
      :cText := "close"
      :AddClass( "close" )
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZChip

   ::oImage := NIL
   ::oIcon  := NIL
   ::oText  := NIL
   ::oClose := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZChip

   ::oText:cText := ::cText

   ::cText := ""

   IF !::lClose
      ::RemoveControl( ::oClose )
   ENDIF

   IF !Empty( ::cIcon )
      ::oIcon:cText := ::cIcon
   ELSE
      ::RemoveControl( ::oIcon )
   ENDIF

   IF Empty( ::cSrcImage )
      ::RemoveControl( ::oImage )
   ELSE
      ::oImage:cSrc := ::cSrcImage
   ENDIF

   IF ::lOutlined
      ::AddClass( "outlined" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

