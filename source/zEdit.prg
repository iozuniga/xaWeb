/*
 * Proyecto: xaWeb framework
 * Fichero: ZInputText.prg
 * Descripción: class for Input type text HTML elements
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZEdit FROM WBasicEdit
PUBLISHED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA oError          AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA lLabelNewLine   INIT .F.

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZEdit

   ::oContainer := WDiv():New( oParent, Self, .T. )
   ::oLabel := WLabel():New( ::oContainer, Self, .T. )
   ::Super:New( ::oContainer, oParent, lAuto )
   ::oError := WDiv():New( ::oContainer, Self, .T. )

   ::AddClass( "xa-input__input" )
   ::oContainer:AddClass( "xa-input" )
   ::oLabel:AddClass( "xa-input__label" )
   ::oError:AddClass( "xa-input__error" )

   // Se asigna posteriormente. Necesario pq ZLabel:Preprocess() se ejecuta antes
   // que ZEdit:Preprocess()

   ::oLabel:cFor := "auto"

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZEdit

   ::oContainer := NIL
   ::oLabel     := NIL
   ::oError     := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZEdit

   LOCAL cId

   cId := ::ValidId()

   IF !Empty( ::cLabel )
      ::oLabel:cText := ::cLabel
   ENDIF

   IF Empty( ::oLabel:cText )
      ::oContainer:RemoveControl( ::oLabel )
      ::oLabel := NIL
   ELSE
      WITH OBJECT ::oLabel
         :cFor     := cId
         :cForm    := ::cForm
         IF ::lLabelNewLine
            :AddStyle( "display:block;" )
         ENDIF
      END WITH
   ENDIF

   IF Empty( ::nSize )
      ::AddStyle( "width: 100%;" )
   ELSE
      ::AddStyle( "width: " + ToString( ::nSize ) + "ch;"  )
      ::nSize := NIL
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
