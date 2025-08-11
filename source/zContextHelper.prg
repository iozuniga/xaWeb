/*
 * Proyecto: xaWeb framework
 * Fichero: ZContextHelper.prg
 * Descripción: Class helper for handling any context
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZContextHelper
RESERVED:
   DATA oContext        AS CLASS WContext
   DATA oActiveControl  AS CLASS WControl

   METHOD New( oContext AS CLASS WContext ) CONSTRUCTOR
   METHOD End()

   METHOD SetControl( oControl ) INLINE ::oActiveControl := oControl
   METHOD SetValue( cKey, xValue )
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oContext ) CLASS ZContextHelper

   ::oContext := oContext

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZContextHelper

   ::oContext := NIL
   ::oActiveControl := NIL

RETURN nil

//------------------------------------------------------------------------------

METHOD SetValue( cKey, aValues, aDef ) CLASS ZContextHelper

   LOCAL xVal
   LOCAL cDes, cClass := ""

   IF ::oActiveControl != NIL
      IF Left( cKey,  1 ) != "_"
         cClass += " " + cKey
      ENDIF

      FOR EACH xVal, cDes IN aValues, aDef
         IF xVal != NIL
            cDes := StrTran( cDes, "?", ToString( xVal ), 1, 1 )
            cClass += IIF( !Empty( cDes ), " ", "" ) + cDes
         ENDIF
      NEXT

      ::oActiveControl:AddClass( cClass )
   ENDIF

RETURN nil

///------------------------------------------------------------------------------

