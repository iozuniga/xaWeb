/*
 * Proyect: XailerWeb framework
 * File: ZText.prg
 * Description: Basic text HTML class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Nota: Hereda de WContainer porque es posible poner inner tags en un tag H1, p.e.
 */

#include "xailerweb.ch"

CLASS ZText FROM WContainer
EXPORTED:
   DATA cText    INIT ""
   DATA cValue   INIT "" PERSISTENT

   METHOD New( oParent, cText ) CONSTRUCTOR

PUBLIC:
   DATA cTag     INIT ""

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagIni()
   METHOD HtmlTagEnd()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cText ) CLASS ZText

   IF !Empty( cText )
      ::cText := cText
   ENDIF

RETURN ::Super():New( oParent )

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZText

   IF ( Len( ::aControls ) > 0 .OR. !Empty( ::cId ) ) .AND. Empty( ::cTag ) ;
      .OR. !Empty( ::cStyle ) .OR. HB_IsObject( ::foStyle )
      ::cTag := "span"
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagIni() CLASS ZText

   IF Empty( ::cTag )
      RETURN ""
   ENDIF

RETURN ::Super:HtmlTagIni()

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZText

   IF Empty( ::cTag )
      RETURN HTML_SPACES + ::cText + hb_eol()
   ELSEIF Len( ::aControls ) == 0
      RETURN '>' + ::cText + '</' + ::cTag + '>' + hb_eol()
   ENDIF

RETURN ::Super:HtmlTagEnd()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZText

   LOCAL cHtml := ""

   IF Len( ::aControls ) > 0
      IF !Empty( ::cText )
         Document:nIndent ++
         cHtml += hb_eol() + HTML_SPACES + ::cText + hb_eol()
         Document:nIndent --
      ENDIF
//      cHtml += ::ChildrenHtml() TOFIX
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
