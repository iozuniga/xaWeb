/*
 * Proyect: XailerWeb framework
 * File: ZMeta.prg
 * Description: Meta headers control class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZMeta
EXPORT:
   DATA cType        INIT "" VALUES "name", "http-equiv", "charset"
   DATA cValue       INIT ""
   DATA cContent     INIT ""

   METHOD New( cType, cValue, cContent )
   METHOD Html()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cType, cValue, cContent ) CLASS ZMeta

   ::cType    := cType
   ::cValue   := cValue
   ::cContent := cContent

RETURN Self

//------------------------------------------------------------------------------

METHOD Html() CLASS ZMeta

   LOCAL cBuffer

   cBuffer :=  HTML_SPACES + '<meta ' + ::cType + '="' + ::cValue + '"'

   IF !Empty( ::cContent )
      cBuffer += ' content="' + ::cContent + '"'
   ENDIF

   cBuffer += '>'

RETURN cBuffer

//------------------------------------------------------------------------------



