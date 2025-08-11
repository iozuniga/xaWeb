/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeRadio.prg
 * Descripción: class for Materialize input-radio
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
  * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatRadio.
 *       This is done automatically when using the xa-materialize.ch file.
 */

#include "xaWeb.ch"

ANNOUNCE ZMatRadio

CLASS ZRadio FROM WBasicRadio
PUBLISHED:
   DATA oContainer      AS CLASS WLabel
   DATA oLabel          AS CLASS WSpan
   DATA cLabel          INIT ""
   DATA lWithGap        INIT .F.

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End() INLINE (::oLabel := nil, ::oContainer := nil, ::Super:End() )
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZRadio

   ::oContainer := WLabel():New( oParent, Self, .T. )
   ::Super:New( ::oContainer, Self )
   ::oLabel := WSpan():New( Self, Self, .T. )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZRadio

   IF !Empty( ::cLabel )
      ::oLabel:cText := ::cLabel
   ENDIF

   IF ::lWithGap
      ::Addclass( "with-gap" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------


