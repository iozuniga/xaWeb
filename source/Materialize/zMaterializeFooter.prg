/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeFooter.prg
 * Descripción: class for Materialize Footer
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatFooter.
 *       This is done automatically when using the xa-materialize.ch file.
 */

#include "xaWeb.ch"

ANNOUNCE ZMatFooter

CLASS ZFooter FROM WControl
PUBLISHED:
   DATA lSticky INIT .F.

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD PreProcess()

PROTECTED:
   DATA cTag      INIT "footer"

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZFooter

   ::Super:New( oParent, oOwner, lAuto )

   ::AddClass( "page-footer" )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZFooter

   LOCAL oSection
   LOCAL cClass

   cClass := "btn"

   IF ::lSticky
      oSection := ::GetSection()
      IF HB_IsObject( oSection )
         Document:AddStyle( "#" + oSection:cId, "display: flex; min-height: 100vh; flex-direction: column;" )
      ELSE
         Document:AddStyle( "body", "display: flex; min-height: 100vh; flex-direction: column;" )
      ENDIF
      Document:AddStyle( "main", "flex: 1 0 auto;" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

