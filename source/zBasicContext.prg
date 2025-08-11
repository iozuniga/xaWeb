/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicContext.prg
 * Descripción: Basic Package for handling CSS context
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZBasicContext FROM WContext
PUBLISHED:
   DATA cName        INIT "xa_BasicContext"  READONLY

   DATA aDarkColors  INIT {"#00001a","DarkSlateBlue","white","LightGray",;
                           "Navy","cornflowerblue","white",;
                           "MediumVioletRed","HotPink","DimGray",;
                           "Gold","PaleGoldenrod","Black",;
                           "LightSteelBlue","#000040"}

   DATA aLightColors INIT {"white","whitesmoke","black","gray",;
                           "Tan","Wheat","black",;
                           "Teal","LightSeaGreen","DimGray",;
                           "SkyBlue","LightBlue","Black",;
                           "Bisque","#0000ff"}

   METHOD SetColor( cName, cValue, lDark )  // --> lValue

RESERVED:
   METHOD Render()
   METHOD GetScript() INLINE Script()

ENDCLASS

//------------------------------------------------------------------------------

METHOD Render() CLASS ZBasicContext

   LOCAL aTmp
   LOCAL cJS, cDark, cLight, cColor, d, l, c

   ::AddCss( Css() )

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

   ::AddScript( cJS, "context" ) // not deterministic

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
      FILE "www/xa_BasicContext.css" INTO cCss
   #ELSE
      FILE "www\xa_BasicContext.css" INTO cCss
   #ENDIF

RETURN cCss

//------------------------------------------------------------------------------

STATIC FUNCTION Script()

   LOCAL cScript

   TEXT INTO cScript
   {
     const theme  = localStorage.getItem('theme');
     const dark   = <DARK>;
     const light  = <LIGHT>;
     const dColor = <COLORS>;
     const root   = document.documentElement;
     const vColor = (theme === "dark" ? dark : light);

     dColor.forEach((v, i) => {
       root.style.setProperty(`--${v}`, vColor[i]);
     });
   }
   ENDTEXT

RETURN cScript

//------------------------------------------------------------------------------


