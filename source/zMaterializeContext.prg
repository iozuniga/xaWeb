/*
 * Proyect: XailerWeb framework
 * File: ZMaterializeContext.prg
 * Description: Package for handling CSS Water context
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Copyleft https://materializeweb.com/
 * Copyleft https://github.com/materializecss/materialize/
 */

#include "xailerweb.ch"

#define URL_CSS      "https://cdn.jsdelivr.net/npm/@materializecss/materialize@2.1.0/dist/css/materialize.min.css"
#define URL_JS       "https://cdn.jsdelivr.net/npm/@materializecss/materialize@2.1.0/dist/js/materialize.min.js"

CLASS ZMaterializeContext FROM WContext
EXPORTED:
   DATA cName        INIT "xw_MaterializeContext"  READONLY

RESERVED:
   METHOD Render()
   METHOD CompatibilityCssVars() INLINE ""
   METHOD MyTheme()

ENDCLASS

//------------------------------------------------------------------------------

METHOD Render() CLASS ZMaterializeContext

   ::AddCss( ::MyTheme() )
   ::AddScript( JsTop() ):lBottom := .F.
   ::AddCSS( URL_CSS )
   ::AddScript( URL_JS )
   ::AddScript( JsBottom() )

RETURN ::Super:Render()

//------------------------------------------------------------------------------
// Overload this method with your own variables
// Do not forget to comment the RETURN ""
// Materialize v2.1.0 (https://materializeweb.com)
// On future versions this variables may not be appropiate

METHOD MyTheme() CLASS ZMaterializeContext

   LOCAL cCss

   RETURN ""

   TEXT INTO cCss
   :root {
      --md-sys-color-primary-light: #006495;
      --md-sys-color-on-primary-light: #ffffff;
      --md-sys-color-primary-container-light: #cbe6ff;
      --md-sys-color-on-primary-container-light: #001e30;
      --md-sys-color-secondary-light: #50606f;
      --md-sys-color-on-secondary-light: #ffffff;
      --md-sys-color-secondary-container-light: #d4e4f6;
      --md-sys-color-on-secondary-container-light: #0d1d29;
      --md-sys-color-tertiary-light: #66587b;
      --md-sys-color-on-tertiary-light: #ffffff;
      --md-sys-color-tertiary-container-light: #ecdcff;
      --md-sys-color-on-tertiary-container-light: #211634;
      --md-sys-color-error-light: #ba1a1a;
      --md-sys-color-error-container-light: #ffdad6;
      --md-sys-color-on-error-light: #ffffff;
      --md-sys-color-on-error-container-light: #410002;
      --md-sys-color-background-light: #fcfcff;
      --md-sys-color-on-background-light: #1a1c1e;
      --md-sys-color-surface-light: #fcfcff;
      --md-sys-color-on-surface-light: #1a1c1e;
      --md-sys-color-surface-variant-light: #dee3ea;
      --md-sys-color-on-surface-variant-light: #41474d;
      --md-sys-color-outline-light: #72787e;
      --md-sys-color-inverse-on-surface-light: #f0f0f3;
      --md-sys-color-inverse-surface-light: #2e3133;
      --md-sys-color-inverse-primary-light: #8fcdff;
      --md-sys-color-shadow-light: #000000;
      --md-sys-color-surface-tint-light: #006495;
      --md-sys-color-outline-variant-light: #c1c7ce;
      --md-sys-color-scrim-light: #000000;

      --md-sys-color-primary-dark: #8fcdff;
      --md-sys-color-on-primary-dark: #003450;
      --md-sys-color-primary-container-dark: #004b71;
      --md-sys-color-on-primary-container-dark: #cbe6ff;
      --md-sys-color-secondary-dark: #b8c8d9;
      --md-sys-color-on-secondary-dark: #22323f;
      --md-sys-color-secondary-container-dark: #394856;
      --md-sys-color-on-secondary-container-dark: #d4e4f6;
      --md-sys-color-tertiary-dark: #d0bfe7;
      --md-sys-color-on-tertiary-dark: #362b4a;
      --md-sys-color-tertiary-container-dark: #4d4162;
      --md-sys-color-on-tertiary-container-dark: #ecdcff;
      --md-sys-color-error-dark: #ffb4ab;
      --md-sys-color-error-container-dark: #93000a;
      --md-sys-color-on-error-dark: #690005;
      --md-sys-color-on-error-container-dark: #ffdad6;
      --md-sys-color-background-dark: #1a1c1e;
      --md-sys-color-on-background-dark: #e2e2e5;
      --md-sys-color-surface-dark: #1a1c1e;
      --md-sys-color-on-surface-dark: #e2e2e5;
      --md-sys-color-surface-variant-dark: #41474d;
      --md-sys-color-on-surface-variant-dark: #c1c7ce;
      --md-sys-color-outline-dark: #8b9198;
      --md-sys-color-inverse-on-surface-dark: #1a1c1e;
      --md-sys-color-inverse-surface-dark: #e2e2e5;
      --md-sys-color-inverse-primary-dark: #006495;
      --md-sys-color-shadow-dark: #000000;
      --md-sys-color-surface-tint-dark: #8fcdff;
      --md-sys-color-outline-variant-dark: #41474d;
      --md-sys-color-scrim-dark: #000000;
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

STATIC FUNCTION JsTop()

   LOCAL cJs

   TEXT INTO cJs
      const theme = localStorage.getItem('theme');
      if (theme) document.documentElement.setAttribute('theme', theme);
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

STATIC FUNCTION JsBottom()

   LOCAL cJs

   TEXT INTO cJs
      const currentTheme = localStorage.getItem('theme');
      const switchElem = document.querySelector('#theme-switch');

      const setTheme = (isDark) => {
       if (isDark) {
         switchElem.classList.add('is-dark');
         switchElem.querySelector('i').innerText = 'light_mode';
         switchElem.title = 'Switch to light mode';
       }
       else {
         switchElem.classList.remove('is-dark');
         switchElem.querySelector('i').innerText = 'dark_mode';
         switchElem.title = 'Switch to dark mode';
       }
      }

      if (switchElem) {
       // Load
       if (currentTheme) setTheme(currentTheme === 'dark');
       // Change
       switchElem.addEventListener('click', e => {
         e.preventDefault();
         if (!switchElem.classList.contains('is-dark')) {
           // Dark Theme
           document.documentElement.setAttribute('theme', 'dark');
           localStorage.setItem('theme', 'dark');
           setTheme(true);
         }
         else {
           // Light Theme
           document.documentElement.removeAttribute('theme');
           document.documentElement.setAttribute('theme', 'light');
           localStorage.setItem('theme', 'light');
           //localStorage.removeItem('theme');
           setTheme(false);
         }
       });
      }
   ENDTEXT

RETURN cJs