/*
 * Proyecto: xaWeb framework
 * Fichero: ZInputMask.prg
 * Descripción: Class InputMask
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * https://beholdr.github.io/maska/
 * https://github.com/beholdr/maska

   Default maska tokens:
    '#': { pattern: /[0-9]/ },       // digits
    '@': { pattern: /[a-zA-Z]/ },    // letters (No international chars)
    '*': { pattern: /[a-zA-Z0-9]/ }, // letters & digits (No international chars)

   Extra xaWeb tokens CA-Clipper alike (one token == one input char --> not multiple):
    'A': { pattern: /[A-Za-zŽžÀ-ÿ]/ }  // only letters
    'N': { pattern: /[A-Za-zŽžÀ-ÿ0-9]/ }  // letters & digits
    'D': { pattern: /[0-9]/ }  // Only digits
    'U': { pattern: /[A-Za-zŽžÀ-ÿ0-9]/ }  // letters & digits & transform to upper case

   Extra xaWeb tokens (not CA-Clipper alike);
   '0': { pattern: /\d/ }  // Numbers multiple
   '9': { pattern: /\d/ }  // Numbers optional
   'B': { pattern: /[A-Za-zŽžÀ-ÿ0-9]/ }  // letters & digits multiple
   'V': { pattern: /[A-Za-zŽžÀ-ÿ0-9]/ }  // letters & digits & transform to upper case & multiple
   Examples:
    - "0.99" Numeric any length, two digits after decimal point
    - "#99.#99.#99.#99" IP address
    - "B B" Two words
    - "V V V" Three words convert to upper case

   Extra xaWeb picture function for <<strictly>> numeric values:
   '!#ll:dd:p" 'll' is the locale setting for the format. By default ::cLocale,
               'dd' is the fraction for decimal portion (number),
               'p' (number) if greater than zero, Only admits positive values.
               When this picture funcion is used the rest of the mask is useless
         Note: If you use a locale setting that uses '.' as thousand separator,
               the decimal point must be set with the ',' char.
*/

#include "xaWeb.ch"
#include "error.ch"

#define URL_MASKA    "https://cdn.jsdelivr.net/npm/maska@3/dist/cdn/maska.js"

FUNCTION InputMask( lCheckExists ) // --> WInputMask

   STATIC oInputMask

   IF !Empty( lCheckExists )
      RETURN HB_IsObject( oInputMask )
   ENDIF

   IF oInputMask == NIL
      oInputMask := WInputMask():New()
   ENDIF

RETURN oInputMask

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZInputMask
PUBLISHED:
   DATA cLocale      INIT "en"

RESERVED:
   METHOD New() CONSTRUCTOR
   METHOD SetMask( oControl  AS CLASS WBasicEdit, cMask ) // --> NIL

END CLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZInputMask

   LOCAL cJs

   TEXT INTO cJs
   const { Mask, MaskInput } = Maska;
   new MaskInput(".maska", {tokens:{
      U:{pattern: /[A-Za-zŽžÀ-ÿ0-9]/,multiple:false,transform:(chr) => chr.toUpperCase()},
      A:{pattern: /[A-Za-zŽžÀ-ÿ]/,multiple:false},
      N:{pattern: /[A-Za-zŽžÀ-ÿ0-9]/,multiple:false},
      D:{pattern: /[0-9]/,multiple:false},
      0:{pattern: /\d/,multiple:true},
      9:{pattern: /\d/,optional:true},
      B:{pattern: /[A-Za-zŽžÀ-ÿ0-9]/,multiple:true},
      V:{pattern: /[A-Za-zŽžÀ-ÿ0-9]/,multiple:true,transform:(chr) => chr.toUpperCase()},
      }});
   new MaskInput(".maska-number-es", {preProcess: (val) => val.replace(/[$.]/g, "")});
   ENDTEXT

   Document:AddScript( URL_MASKA )

   WITH OBJECT Document:AddScript( cJs )
      :lBottom := .T.
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD SetMask( oControl, cMask ) CLASS ZInputMask

   LOCAL aTokens
   LOCAL cLocale
   LOCAL nFraction
   LOCAL lNumeric, lPositive, lClass

   lNumeric := ( Left( cMask, 2 ) ==  "!#" )
   lClass   := .F.

   IF lNumeric
      aTokens := HB_ATokens( SubStr( cMask, 3 ), ":" )
      IF Len( aTokens ) > 0
         cLocale := aTokens[ 1 ]
         IF Empty( cLocale )
            cLocale := ::cLocale
         ENDIF
      ELSE
         cLocale := ::cLocale
      ENDIF
      IF Len( aTokens ) > 1
         nFraction := Val( aTokens[ 2 ] )
      ELSE
         nFraction := 0
      ENDIF
      IF Len( aTokens ) > 2
         lPositive := ( Val( aTokens[ 3 ] ) > 0 )
      ELSE
         lPositive := .F.
      ENDIF
   ENDIF

   WITH OBJECT oControl
      IF lNumeric
         IF nFraction > 0
            :AddDataset( "maska-number-fraction", Str( nFraction, 1 ) )
         ELSE
            :AddDataset( "maska-number" )
         ENDIF
         IF !Empty( cLocale )
            :AddDataset( "maska-number-locale", ::cLocale )
            IF cLocale $ "es;de;fr;br"
               :AddClass( "maska-number-es" )
               lClass := .T.
            ENDIF
         ENDIF
         IF lPositive
            :AddDataset( "maska-number-unsigned", "true" )
         ENDIF
      ELSE
         :AddDataset( "maska", cMask )
      ENDIF
      IF !lClass
         :AddClass( "maska" )
      ENDIF
   END WITH

RETURN NIL

//------------------------------------------------------------------------------

