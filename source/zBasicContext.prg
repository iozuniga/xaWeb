/*
 * Proyect: XailerWeb framework
 * File: ZBasicContext.prg
 * Description: Basic Package for handling CSS context
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZBasicContext FROM WContext
EXPORTED:
   DATA cName        INIT "xw_BasicContext"  READONLY
   DATA cTheme       INIT ""                 VALUES "auto", "dark", "light"

//   Defined in its ancestor
//   DATA aVarColors   INIT {"body-color","body-alt-color","body-text-color", "body-text-mutted-color",;
//                           "primary-color","primary-hover-color","primary-text-color",;
//                           "secondary-color","secondary-hover-color","secondary-text-color",;
//                           "accent-color","accent-text-color","accent-hover-color",;
//                           "border-color"}

   DATA aDarkColors  INIT {"#00001a","DarkSlateBlue","white","LightGray",;
                           "Navy","cornflowerblue","white",;
                           "MediumVioletRed","HotPink","DimGray",;
                           "Gold","PaleGoldenrod","Black",;
                           "LightSteelBlue", "64"}

   DATA aLightColors INIT {"white","whitesmoke","black","gray",;
                           "Tan","Wheat","black",;
                           "Teal","LightSeaGreen","DimGray",;
                           "SkyBlue","LightBlue","Black",;
                           "Bisque", "255"}

   METHOD SetColor( cName, cValue, lDark )

RESERVED:
   METHOD Render()
   METHOD GetScript() INLINE Script()

ENDCLASS

//------------------------------------------------------------------------------

METHOD Render() CLASS ZBasicContext

   LOCAL aTmp
   LOCAL cJS, cDark, cLight, cColor, d, l, c

   ::AddCss( Css(), ::cName )

   cJS    := ::GetScript()
   cDark  := "["
   cLight := "["
   cColor := "["

   FOR EACH d, l, c IN ::aDarkColors, ::aLightColors, ::aVarColors
      cDark  += '"' + d + '",'
      cLight += '"' + l + '",'
      cColor += '"' + c + '",'
   NEXT

   cDark  := SubStr( cDark, 1, Len( cDark ) - 1 ) + "]"
   cLight := SubStr( cLight, 1, Len( cLight ) - 1 ) + "]"
   cColor := SubStr( cColor, 1, Len( cColor ) - 1 ) + "]"

   cJS := StrTran( cJS, "<DARK>", cDark, 1, 1 )
   cJS := StrTran( cJS, "<LIGHT>", cLight, 1, 1 )
   cJS := StrTran( cJS, "<COLORS>", cColor, 1, 1 )
   cJS := StrTran( cJS, "<THEME>", ::cTheme, 1, 1 )

   ::AddScript( cJS ) // not deterministic

RETURN ::Super:Render()

//------------------------------------------------------------------------------

METHOD SetColor( cName, cValue, lDark ) CLASS ZBasicContext

   LOCAL aCo
   LOCAL nAt

   DEFAULT lDark TO ( ::cTheme == "dark" )

   nAt := AScan( ::aVarColors, cName )
   aCo := IIF( lDark, ::aDarkColors, ::aLightColors )

   IF nAt > 0
      aCo[ nAt ] := cValue
      RETURN .t.
   ENDIF

RETURN .f.

//------------------------------------------------------------------------------

STATIC FUNCTION Css()

   LOCAL cCss

   #IFDEF _LINUX_
      FILE "www/xw_BasicContext.css" INTO cCss
   #ELSE
      FILE "www\xw_BasicContext.css" INTO cCss
   #ENDIF

RETURN cCss

//------------------------------------------------------------------------------

STATIC FUNCTION Script()

   LOCAL cScript

   TEXT INTO cScript

   function setTheme(name) {
     const dark   = <DARK>;
     const light  = <LIGHT>;
     const dColor = <COLORS>;
     const root   = document.documentElement;

     if (name === "auto" ) {
       if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
         name = "dark";
       } else {
         name = "light";
       }
     }
     const vColor = (name === "dark" ? dark : light);
     dColor.forEach((v, i) => {
       root.style.setProperty(`--${v}`, vColor[i]);
     });
   }

   document.addEventListener("DOMContentLoaded", () => {
     let savedTheme = "<THEME>";
     if (savedTheme === "") {
       savedTheme = localStorage.getItem("theme") || "auto";
     }
     setTheme(savedTheme);

     for (const optionElement of document.querySelectorAll("#selTheme option")) {
       optionElement.selected = savedTheme === optionElement.value;
     }

     const selector = document.querySelector("#selTheme");
     if (selector) {
       selector.addEventListener("change", () => {
         localStorage.setItem("theme", this.value);
         setTheme(this.value);
       });
     }
   });

   ENDTEXT

RETURN cScript

//------------------------------------------------------------------------------

