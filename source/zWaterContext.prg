/*
 * Proyect: XailerWeb framework
 * File: ZWaterContext.prg
 * Description: Package for handling CSS Water context
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Copyleft https://github.com/kognise/water.css/blob/master/LICENSE.md
 */

#include "xailerweb.ch"

#define THEME_AUTO   "https://cdn.jsdelivr.net/npm/water.css@2/out/water.min.css"
#define THEME_LIGHT  "https://cdn.jsdelivr.net/npm/water.css@2/out/light.min.css"
#define THEME_DARK   "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css"

CLASS ZWaterContext FROM WContext
EXPORTED:
   DATA cName        INIT "xw_WaterContext"  READONLY
   DATA cTheme       INIT ""                 VALUES "", "auto", "dark", "light"

   METHOD GetScript()            INLINE Script()

   METHOD BodyColor()            INLINE "var(--background)"
   METHOD BodyAltColor()         INLINE "var(--background-alt)"
   METHOD BodyTextColor()        INLINE "var(--text-main)"
   METHOD BodyTextMuttedColor()  INLINE "var(--text-muted)"

   METHOD PrimaryColor()         INLINE "var(--highlight)"
   METHOD PrimaryHoverColor()    INLINE "var(--button-hover)"
   METHOD PrimaryTextColor()     INLINE "var(--background)"

   METHOD AccentTextColor()      INLINE "var(--text-bright)"

   METHOD BorderColor()          INLINE "var(--border)"

RESERVED:
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

   cJS := ::GetScript()
   cJS := StrTran( cJS, "<THEME>", ::cTheme, 1, 1 )

   ::AddScript( cJS ) // not deterministic

RETURN ::Super:Render()

//------------------------------------------------------------------------------

STATIC FUNCTION Script()

   LOCAL cScript

   TEXT INTO cScript
   document.addEventListener("DOMContentLoaded", () => {
     const savedTheme = "<THEME>";
     if (savedTheme === "") {
       savedTheme = localStorage.getItem("theme") || "auto";
     }
     localStorage.setItem("theme", savedTheme);
     xw_setCookie("theme", savedTheme, {'Expires': "Fri, 31 Dec 9999 23:59:59 GMT"});

   });
   ENDTEXT

RETURN cScript

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
