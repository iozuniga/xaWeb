/*
 * Proyecto: xaWeb framework
 * Fichero: ZLink.prg
 * Descripción: HTML Link class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 *
 * Nota: Es un Container, para poder incluir imagenes
 */

#include "xaWeb.ch"

CLASS ZLink FROM WControl
PUBLISHED:
   DATA oHRef     AS CLASS WTask
   DATA cTarget   INIT "" VALUES "_self", "_blank", "_parent", "_top"
   DATA cText     INIT ""
   DATA cTitle    INIT ""
   DATA lDisabled INIT .F. PERSISTENT

   METHOD cHRef( cValue ) SETGET  // --> cText

PROTECTED:
   DATA cTag      INIT "a"

RESERVED:
   METHOD End()
   METHOD HtmlTagBody()
   METHOD PreProcess()
ENDCLASS

//------------------------------------------------------------------------------

METHOD End() CLASS ZLink

   IF HB_IsObject( ::oHRef )
      ::oHRef:End()
      ::oHRef := NIL
   ENDIF

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD cHRef( cValue ) CLASS ZLink

   IF PCount() > 0
      IF HB_IsObject( cValue )
         ::oHRef := cValue
      ELSE
         ::oHRef := WTask():Url( cValue )
      ENDIF
   ELSE
      // services must be processed on its OnClick event
      IF HB_IsObject( ::oHRef ) .AND. ::oHRef:cType != "service"
         RETURN ::oHRef:Html()
      ENDIF
   ENDIF

RETURN ""

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZLink

   STATIC lCss := .F.

   IF HB_IsObject( ::oHRef ) .AND. ::oHRef:cType $ "service;script"
      ::OnClick := ::oHRef
   ELSEIF Empty( ::cHRef )
      ::AddClass( "emptyref" )
      IF !lCss .OR. Document:lRender2
         Document:AddCSS( "a.emptyref:hover {cursor:default};" )
         lCss := .t.
      ENDIF
   ENDIF

   IF ::lDisabled
      ::AddClass( "disabled" )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZLink

   LOCAL cHtml := ""

   IF !Empty( ::cHRef )
      cHtml += ' href="' + ::cHRef + '"'
   ENDIF

   IF !Empty( ::cTarget )
      cHtml += ' target="' + ::cTarget + '"'
   ENDIF

   IF !Empty( ::cTitle )
      cHtml += ' title="' + ::cTitle + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

