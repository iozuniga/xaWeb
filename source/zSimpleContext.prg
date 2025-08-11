/*
 * Proyecto: xaWeb framework
 * Fichero: ZSimpleContext.prg
 * Descripción: Package for handling CSS Simple context
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Referred to https://github.com/kevquirk/simple.css
 * Note:
 * This CSS uses the theme light or dark depending on the OS preferences.
 * It can not be changed easily. There are some CSS variables that can be
 * changed.
 */

#include "xaWeb.ch"

#define THEME_MINI   "https://unpkg.com/simpledotcss/simple.min.css"
#define THEME_NORMAL "https://unpkg.com/simpledotcss/simplecss"

CLASS ZSimpleContext FROM WContext
OVERLOADED:
   METHOD BodyColor()            INLINE "var(--bg)"  // --> cCssValue
   METHOD BodyAltColor()         INLINE "var(--accent-bg)" // --> cCssValue
   METHOD BodyTextColor()        INLINE "var(--text)" // --> cCssValue
   METHOD BodyTextMuttedColor()  INLINE "var(--text-light)" // --> cCssValue

   METHOD AccentColor()          INLINE "var(--accent)" // --> cCssValue
   METHOD AccentHoverColor()     INLINE "var(--accent-hover)" // --> cCssValue
   METHOD AccentTextColor()      INLINE "var(--accent-text)" // --> cCssValue

   METHOD BorderColor()          INLINE "var(--border)" // --> cCssValue

RESERVED:
   DATA cName                    INIT "SimpleContext" READONLY
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

