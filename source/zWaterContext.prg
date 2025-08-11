/*
 * Proyecto: xaWeb framework
 * Fichero: ZWaterContext.prg
 * Descripción: Package for handling CSS Water context
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Referred to https://github.com/kognise/water.css/blob/master/LICENSE.md
 */

#include "xaWeb.ch"

#define THEME_AUTO   "https://cdn.jsdelivr.net/npm/water.css@2/out/water.min.css"
#define THEME_LIGHT  "https://cdn.jsdelivr.net/npm/water.css@2/out/light.min.css"
#define THEME_DARK   "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css"

CLASS ZWaterContext FROM WContext
OVERLOADED:
   METHOD BodyColor()            INLINE "var(--background)"  // --> cCssValue
   METHOD BodyAltColor()         INLINE "var(--background-alt)" // --> cCssValue
   METHOD BodyTextColor()        INLINE "var(--text-main)" // --> cCssValue
   METHOD BodyTextMuttedColor()  INLINE "var(--text-muted)" // --> cCssValue

   METHOD PrimaryColor()         INLINE "var(--highlight)" // --> cCssValue
   METHOD PrimaryHoverColor()    INLINE "var(--button-hover)" // --> cCssValue
   METHOD PrimaryTextColor()     INLINE "var(--background)" // --> cCssValue

   METHOD AccentTextColor()      INLINE "var(--text-bright)" // --> cCssValue

   METHOD BorderColor()          INLINE "var(--border)" // --> cCssValue

RESERVED:
   DATA cName        INIT "xa_WaterContext"  READONLY

   METHOD Render()
   METHOD CompatibilityCssVars()

ENDCLASS

//------------------------------------------------------------------------------

METHOD CompatibilityCssVars() CLASS ZWaterContext

   LOCAL cCss
   TEXT INTO cCss
   :root {
      --body-color: var(--background);
      --body-alt-color: var(--background-alt);
      --body-text-color: var(--text-main);
      --body-text-mutted-color: var(--text-mutted);
      --primary-color: var(--highlight);
      --primary-hover-color: var(--button-hover);
      --primary-text-color: var(--background);
      --border-color: var(--border);
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

METHOD Render() CLASS ZWaterContext

   LOCAL cJS, cCss

   IF Empty( ::cTheme )
      ::cTheme := Engine:Cookie( "theme" )
   ENDIF

   SWITCH ::cTheme
   CASE "auto"
   CASE ""
      ::AddCSS( THEME_AUTO )
      EXIT
   CASE "light"
      ::AddCSS( THEME_LIGHT )
      EXIT
   CASE "dark"
      ::AddCSS( THEME_DARK )
      EXIT
   END SWITCH

RETURN ::Super:Render()

//------------------------------------------------------------------------------

/*
This are the variables you can modify from water.css
You must assign any of the variables and delete the ones
not used. And asign the string to the property cCssMods.
This is just a sample for your own personal use.
*/

/*
   TEXT INTO cCss
   :root {
      --background-body
      --background
      --background-alt
      --selection
      --text-main
      --text-bright
      --text-muted
      --links
      --focus
      --border
      --code
      --animation-duration
      --button-hover
      --scrollbar-thumb
      --scrollbar-thumb-hover
      --form-placeholder
      --form-text
      --variable
      --highlight
      --select-arrow
   }

   @media (prefers-color-scheme: dark) {
      :root {
      }
   }

   @media (prefers-color-scheme: light) {
      :root {
      }
   }

   ENDTEXT
*/

//------------------------------------------------------------------------------
