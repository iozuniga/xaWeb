/*
 * Proyect: XailerWeb framework
 * File: ZDocPage.prg
 * Description: WDocPage class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZDocPage FROM WContainer
EXPORTED:
   DATA oParent         READONLY
   DATA cName           INIT ""
   DATA lForced         INIT .F.
   DATA lFooter         INIT .F.
   DATA lHide           INIT .F.

   METHOD New( oParent, cName, lForced ) CONSTRUCTOR

   METHOD Register()    INLINE Document:RegisterPage( ::cName )

RESERVED:
   DATA cTag      INIT "x-page"

   METHOD FirstForm()   INLINE sFirstForm( Self )
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cName, lForced ) CLASS ZDocPage

   DEFAULT cName TO "", lForced TO .F.

   ::oParent := oParent
   ::cName   := cName
   ::lForced := lForced
   ::cId     := cName

   ::Super:New( nil, oParent ) // No parent on purpose

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZDocPage

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


