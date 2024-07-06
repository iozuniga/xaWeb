/*
 * Proyect: XailerWeb framework
 * File: ZSimpleContext.prg
 * Description: Package for handling CSS Simple context
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Copyleft https://github.com/kevquirk/simple.css
 * Note:
 * This CSS uses the theme light or dark depending on the OS preferences.
 * It can not be changed easily. There are some CSS variables that can be
 * changed.
 */

#include "xailerweb.ch"

#define THEME_MINI   "https://unpkg.com/simpledotcss/simple.min.css"
#define THEME_NORMAL "https://unpkg.com/simpledotcss/simplecss"

CLASS ZSimpleContext FROM WContext
EXPORTED:
   DATA cName                    INIT "SimpleContext" READONLY

   METHOD BodyColor()            INLINE "var(--bg)"
   METHOD BodyAltColor()         INLINE "var(--accent-bg)"
   METHOD BodyTextColor()        INLINE "var(--text)"
   METHOD BodyTextMuttedColor()  INLINE "var(--text-light)"

   METHOD AccentColor()          INLINE "var(--accent)"
   METHOD AccentHoverColor()     INLINE "var(--accent-hover)"
   METHOD AccentTextColor()      INLINE "var(--accent-text)"

   METHOD BorderColor()          INLINE "var(--border)"

RESERVED:
   METHOD Render()
   METHOD CompatibilityCssVars()

ENDCLASS

//------------------------------------------------------------------------------

METHOD Render() CLASS ZSimpleContext

   ::AddCSS( THEME_MINI )

RETURN ::Super:Render()

//------------------------------------------------------------------------------

METHOD CompatibilityCssVars() CLASS ZSimpleContext

   LOCAL cCss
   TEXT INTO cCss
   :root {
      --body-color: var(--bg);
      --body-alt-color: var(--accent-bg);
      --body-text-color: var(--text);
      --body-text-mutted-color: var(--text-light);
      --accent-color: var(--accent);
      --accent-text-color: var(--accent-text);
      --accent-hover-color: var(--acent-hover);
      --border-color: var(--border);
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

/*
This are the variables you can modify from simple.css
You must delete or comment the variables not used
and asign the string to the property cCssMods.
This is just a sample for your own personal use.
*/

/*
   TEXT INTO cCss
   :root {
     --sans-font: -apple-system, BlinkMacSystemFont, "Avenir Next", Avenir,
       "Nimbus Sans L", Roboto, "Noto Sans", "Segoe UI", Arial, Helvetica,
       "Helvetica Neue", sans-serif;
     --mono-font: Consolas, Menlo, Monaco, "Andale Mono", "Ubuntu Mono", monospace;
     --standard-border-radius: 5px;
   }

   @media (prefers-color-scheme: dark) {
      :root {
       color-scheme: dark;
       --bg: #212121;
       --accent-bg: #2b2b2b;
       --text: #dcdcdc;
       --text-light: #ababab;
       --accent: #ffb300;
       --accent-hover: #ffe099;
       --accent-text: var(--bg);
       --code: #f06292;
       --preformatted: #ccc;
       --disabled: #111;
      }
   }

   @media (prefers-color-scheme: light) {
      :root {
      }
     --bg: #fff;
     --accent-bg: #f5f7ff;
     --text: #212121;
     --text-light: #585858;
     --border: #898EA4;
     --accent: #0d47a1;
     --accent-hover: #1266e2;
     --accent-text: var(--bg);
     --code: #d81b60;
     --preformatted: #444;
     --marked: #ffdd33;
     --disabled: #efefef;
   }
   ENDTEXT
*/

//------------------------------------------------------------------------------

