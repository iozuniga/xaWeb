/*
 * Proyect: XailerWeb framework
 * File: ZContext.prg
 * Description: Base class for handling CSS Simple context
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Note: Internal class. Do not instantiate objects of this class
 *
 */

#include "xailerweb.ch"

CLASS ZContext FROM WPackage
EXPORTED:
   DATA cName                    INIT "xw_Context" READONLY
   DATA cCssMods                 INIT ""

   DATA aVarColors   INIT {"body-color","body-alt-color","body-text-color", "body-text-mutted-color",;
                           "primary-color","primary-hover-color","primary-text-color",;
                           "secondary-color","secondary-hover-color","secondary-text-color",;
                           "accent-color","accent-text-color","accent-hover-color",;
                           "border-color", "gr-color"}

   METHOD New( oDoc, nPos )      CONSTRUCTOR

   METHOD BodyColor()            INLINE "var(--body-color)"
   METHOD BodyAltColor()         INLINE "var(--body-alt-color)"
   METHOD BodyTextColor()        INLINE "var(--body-text-color)"
   METHOD BodyTextMutedColor()   INLINE "var(--body-text-mutted-color)"

   METHOD PrimaryColor()         INLINE "var(--primary-color)"
   METHOD PrimaryHoverColor()    INLINE "var(--primary-hover-color)"
   METHOD PrimaryTextColor()     INLINE "var(--primary-text-color)"

   METHOD SecondaryColor()       INLINE "var(--secondary-color)"
   METHOD SecondaryHoverColor()  INLINE "var(--secondary-hover-color)"
   METHOD SecondaryTextColor()   INLINE "var(--secondary-text-color)"

   METHOD AccentColor()          INLINE "var(--accent-color)"
   METHOD AccentHoverColor()     INLINE "var(--accent-hover-color)"
   METHOD AccentTextColor()      INLINE "var(--accent-text-color)"

   METHOD BorderColor()          INLINE "var(--border-color)"
   METHOD GrColor()              INLINE "var(--gr-color)"

RESERVED:
   METHOD Render()
   METHOD CompatibilityCssVars() INLINE ""

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oDoc, nPos ) CLASS ZContext

   ::Super:New( oDoc, nPos )

   oDoc:oContext := Self

RETURN Self

//------------------------------------------------------------------------------

METHOD Render() CLASS ZContext

   LOCAL cCss

   IF !Empty( ::cCssMods )
      ::AddCSS( ::cCssMods, ::cName )
   ENDIF

   cCss := ::CompatibilityCssVars()

   IF !Empty( cCss )
      ::AddCSS( cCss, ::cName )
   ENDIF

RETURN ::Super:Render()

//------------------------------------------------------------------------------

