/*
 * Proyecto: xaWeb framework
 * Fichero: ZDocSection.prg
 * Descripción: WDocSection class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZDocSection FROM WControl
PUBLISHED:
   DATA oParent         READONLY AS CLASS WDoc
   DATA cName           INIT ""
   DATA lDeploy         INIT .F.
   DATA lFooter         INIT .T.
   DATA lHide           INIT .F.

   METHOD New( oParent AS CLASS WControl, cName OPTIONAL, lDeploy OPTIONAL ) CONSTRUCTOR

   METHOD Register()    INLINE Document:RegisterSection( ::cName )

RESERVED:
   DATA cTag      INIT "x-doc-section"

   METHOD FirstForm()   INLINE sFirstForm( Self )
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cName, lDeploy ) CLASS ZDocSection

   DEFAULT cName TO "default", lDeploy TO .F.

   ::cName   := cName
   ::lDeploy := lDeploy
   ::cId     := cName

   //::Super:New( nil, oParent ) // No parent on purpose

RETURN ::Super:New( oParent )

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZDocSection

   IF ::lHide
      ::AddStyle( "display: none;" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

STATIC FUNCTION sFirstForm( oControl, oForm )

   LOCAL oIter

   FOR EACH oIter IN oControl:aControls
      IF oIter:IsKindOf( "WForm" )
         oForm := oIter
         EXIT
      ELSEIF oIter:IsKindOf( "WContainer" )
         sFirstForm( oIter, @oForm )
         IF oForm != NIL
            EXIT
         ENDIF
      ENDIF
   NEXT

RETURN oForm

//------------------------------------------------------------------------------


