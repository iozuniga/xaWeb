/*
 * Proyecto: xaWeb framework
 * Fichero: ZContext.prg
 * Descripción: Base class for handling contexts
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: Internal class. Do not instantiate objects of this class
 *
 */

#include "xaWeb.ch"

CLASS ZContext FROM WPackage
PUBLISHED:
   DATA cName                    INIT "xa_Context" READONLY
   DATA cTheme                   INIT "" VALUES "auto","dark","light"
   DATA cCssMods                 INIT ""

   DATA aVarColors   INIT {"body-color","body-alt-color","body-text-color","body-text-mutted-color",;
                           "primary-color","primary-hover-color","primary-text-color",;
                           "secondary-color","secondary-hover-color","secondary-text-color",;
                           "accent-color","accent-text-color","accent-hover-color",;
                           "border-color","gr-color"} READONLY


   METHOD BodyColor()            INLINE "var(--body-color)"  // --> cssVarColor
   METHOD BodyAltColor()         INLINE "var(--body-alt-color)" // --> cssVarColor
   METHOD BodyTextColor()        INLINE "var(--body-text-color)" // --> cssVarColor
   METHOD BodyTextMutedColor()   INLINE "var(--body-text-mutted-color)" // --> cssVarColor

   METHOD PrimaryColor()         INLINE "var(--primary-color)" // --> cssVarColor
   METHOD PrimaryHoverColor()    INLINE "var(--primary-hover-color)" // --> cssVarColor
   METHOD PrimaryTextColor()     INLINE "var(--primary-text-color)" // --> cssVarColor

   METHOD SecondaryColor()       INLINE "var(--secondary-color)" // --> cssVarColor
   METHOD SecondaryHoverColor()  INLINE "var(--secondary-hover-color)" // --> cssVarColor
   METHOD SecondaryTextColor()   INLINE "var(--secondary-text-color)" // --> cssVarColor

   METHOD AccentColor()          INLINE "var(--accent-color)" // --> cssVarColor
   METHOD AccentHoverColor()     INLINE "var(--accent-hover-color)" // --> cssVarColor
   METHOD AccentTextColor()      INLINE "var(--accent-text-color)" // --> cssVarColor

   METHOD BorderColor()          INLINE "var(--border-color)" // --> cssVarColor
   METHOD GrColor()              INLINE "var(--gr-color)" // --> cssVarColor
   METHOD OpaqueColor( nPer )  // --> cssVarColor

   METHOD AddCustomColor( cKey, xCoLorLight, xColorDark )  // --> nil
   METHOD CustomColor( cKey )   INLINE "var(--" + cKey + ")"  // --> cssVarColor

RESERVED:
   DATA hCustomColors INIT {=>}

   METHOD New( oDoc AS CLASS WDoc, nPos )      CONSTRUCTOR
   METHOD End()                        VIRTUAL
   METHOD Render()
   METHOD CompatibilityCssVars()       INLINE ""
   METHOD GetHelper( oControl )        VIRTUAL
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oDoc, nPos ) CLASS ZContext

   ::Super:New( oDoc, nPos )

   oDoc:oContext := Self

RETURN Self

//------------------------------------------------------------------------------

METHOD AddCustomColor( cKey, xColorLight, xColorDark ) CLASS ZContext

   DEFAULT xColorLight TO "", xColorDark TO ""

   HB_HSet( ::hCustomColors, cKey, { xColorLight, xColorDark } )

RETURN NIL

//------------------------------------------------------------------------------
/*
Primary ancestor context controls the theme for any other context and creates
the custom colors created by the user. Any context can access to the current
theme via localStorage, Cookie or document.documentElement.getAttribute('theme')
*/

METHOD Render() CLASS ZContext

   LOCAL aColor
   LOCAL cKey, cDark, cLight, cColor, cCss, cJs

  TEXT INTO cJs
   {
      let theme = "<THEME>";
      const sTheme = localStorage.getItem('theme');

      if (sTheme ) {
         localStorage.removeItem('theme');
         xa_deleteCookie('theme');
      }
      if (theme === "auto") {
         if (sTheme) {
            theme = sTheme;
         } else {
            if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
               theme = "dark";
            } else {
               theme = "light";
            }
         }
      }
      if (sTheme && (theme === "") )  {
         theme = sTheme;
      }
      if (theme.length > 0 ) {
         localStorage.setItem("theme", theme);
         xa_setCookie("theme", theme, {'Expires': "Fri, 31 Dec 9999 23:59:59 GMT"});
         document.documentElement.setAttribute('theme', theme);
      }
   }
  ENDTEXT

   IF Len( ::hCustomColors ) > 0
      cDark  := ""
      cLight := ""

      FOR EACH cKey, aColor IN HB_HKeys( ::hCustomColors ), HB_HValues( ::hCustomColors )
         cLight += "--" + cKey + ":" + ToString( aColor[ 1 ] ) + ";" + hb_eol()
         cDark  += "--" + cKey + ":" + ToString( aColor[ 2 ] ) + ";" + hb_eol()
      NEXT

      cCss := ":root[theme=light] {" + hb_eol() + cLight + "}" + hb_eol() + ;
              ":root[theme=dark] {" + hb_eol() + cDark + "}" + hb_eol()
      ::AddCSS( cCss, "context" )
   ENDIF

   cCss := ::cCssMods

   IF !Empty( cCss )
      ::AddCSS( cCss, "context" )
   ENDIF

   cCss := ::CompatibilityCssVars()

   IF !Empty( cCss )
      ::AddCSS( cCss, "context" )
   ENDIF

   cJs := StrTran( cJs, "<THEME>", ::cTheme, 1, 1 )

   ::AddScript( cJs, "context", .F., .T. ):lBottom := .F. // top

RETURN ::Super:Render()

//------------------------------------------------------------------------------

METHOD OpaqueColor( nPer, nColor ) CLASS ZContext

   LOCAL cColor, cPer, cCol

   DEFAULT nPer TO 0.7

   IF nPer > 1
      nPer := Min( 1, nPer / 100 )
   ENDIF

   cPer := LTrim( Str( nPer, 10, 2 ) )

   IF PCount() > 1
      cCol := ToString( nColor )
   ELSE
      cCol := "var(--gr-color)"
   ENDIF

   cColor := "linear-gradient(rgba([color],[color],[color],[per]),"+;
             "rgba([color],[color],[color],[per]));"

   cColor := StrTran( cColor, "[color]", cCol )
   cColor := StrTran( cColor, "[per]", cPer )

RETURN cColor

//------------------------------------------------------------------------------

/* Old code, but better not delete

   IF Len( ::hCustomColors ) > 0
      cDark  := "["
      cLight := "["
      cColor := "["

      FOR EACH cKey, aColor IN HB_HKeys( ::hCustomColors ), HB_HValues( ::hCustomColors )
         cDark  += '"' + ToString( aColor[ 2 ] ) + '",'
         cLight += '"' + ToString( aColor[ 1 ] ) + '",'
         cColor += '"' + cKey + '",'
      NEXT

      cDark  := SubStr( cDark, 1, Len( cDark ) - 1 ) + "]"
      cLight := SubStr( cLight, 1, Len( cLight ) - 1 ) + "]"
      cColor := SubStr( cColor, 1, Len( cColor ) - 1 ) + "]"
   ELSE
      cDark  := "[]"
      cLight := "[]"
      cColor := "[]"
   ENDIF

   cJs := StrTran( cJs, "<DARK>", cDark, 1, 1 )
   cJs := StrTran( cJs, "<LIGHT>", cLight, 1, 1 )
   cJs := StrTran( cJs, "<COLORS>", cColor, 1, 1 )

  TEXT INTO cJs
   {
      let theme    = "<THEME>";
      const dark   = <DARK>;
      const light  = <LIGHT>;
      const custom = <COLORS>;
      const sTheme = localStorage.getItem('theme');
      const root = document.documentElement;

      if (sTheme ) {
         localStorage.removeItem('theme');
         xa_deleteCookie('theme');
      }

      if (theme === "auto") {
         if (sTheme) {
            theme = sTheme;
         } else {
            if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
               theme = "dark";
            } else {
               theme = "light";
            }
         }
      }

      if (sTheme && (theme === "") )  {
         theme = sTheme;
      }

      if (theme.length > 0 ) {
         localStorage.setItem("theme", theme);
         xa_setCookie("theme", theme, {'Expires': "Fri, 31 Dec 9999 23:59:59 GMT"});
         document.documentElement.setAttribute('theme', theme);
      }

      const vColor = (theme === "dark" ? dark : light);

      custom.forEach((v, i) => {
         root.style.setProperty(`--${v}`, vColor[i]);
      });
   }
  ENDTEXT

*/