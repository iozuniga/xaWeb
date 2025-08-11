/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasicStyle.prg
 * Descripción: Style helper class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Important: VALUES clause is necessary in order to assing style to controls
 * Note: This classs automatically sets the styles of its parent control when its
         data is assigned. Be aware, that the value returned for this data
         members may not be accurate, since the style may be also changed on its
         parent directly
 */

#include "xaWeb.ch"

CLASS ZBasicStyle
RESERVED:
   DATA oActiveControl

   METHOD New( oControl ) CONSTRUCTOR
   METHOD End()
   METHOD SetStyle( cKey, cValue )
   METHOD SetControl( oControl )    INLINE ::oActiveControl := oControl

END CLASS

//------------------------------------------------------------------------------

METHOD New( oControl ) CLASS ZBasicStyle

   ::oActiveControl := oControl

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZBasicStyle

   ::oActiveControl := NIL

RETURN NIL

//------------------------------------------------------------------------------

METHOD SetStyle( cKey, cValue ) CLASS ZBasicStyle

   IF ::oActiveControl != NIL
      cKey := Lower( SubStr( cKey, 1 ) )
      cKey := StrTran( cKey, "_", "-" )

      IF !Empty( cValue )
         ::oActiveControl:AddStyle( cKey + ":" + ToString( cValue ) + ";")
      ELSE
         ::oActiveControl:DelStyle( cKey )
      ENDIF
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------

