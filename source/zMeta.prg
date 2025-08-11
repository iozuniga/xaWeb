/*
 * Proyecto: xaWeb framework
 * Fichero: ZMeta.prg
 * Descripción: Meta headers control class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZMeta
PUBLISHED:
   DATA cType        INIT "" VALUES "name", "http-equiv", "charset"
   DATA cValue       INIT ""
   DATA cContent     INIT ""
   DATA oParent      AS CLASS WDoc

   METHOD New( oDoc, cType, cValue, cContent ) CONSTRUCTOR

RESERVED:
   METHOD End()     INLINE ::oParent := NIL
   METHOD Html()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oDoc, cType, cValue, cContent ) CLASS ZMeta

   ::oParent  := oDoc
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



