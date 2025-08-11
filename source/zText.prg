/*
 * Proyecto: xaWeb framework
 * Fichero: ZText.prg
 * Descripción: Basic text HTML class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Nota: Hereda de WContainer porque es posible poner inner tags en un tag H1, p.e.
 */

#include "xaWeb.ch"
#include "error.ch"


CLASS ZText FROM WControl
PUBLISHED:
   DATA cText    INIT ""

RESERVED:
   DATA cValue   INIT "" PERSISTENT    // TOFIX: verificar si es necesario
   DATA cTag     INIT ""

   METHOD PreProcess()
   METHOD HtmlTagIni()
   METHOD HtmlTagEnd()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZText

   IF ::IsChildren()
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

   LOCAL cText := ::cText

   IF !Empty( cText ) .AND. ::NeedTranslate()
      cText := Translator():Translate( cText )
   ENDIF

   IF Empty( ::cTag )
      RETURN HTML_SPACES + cText + hb_eol()
   ELSEIF !::IsChildren()
      RETURN '>' + cText + '</' + ::cTag + '>' + hb_eol()
   ENDIF

RETURN ::Super:HtmlTagEnd()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZText

   LOCAL cHtml := "", cTagBody

   IF ::IsChildren()
      IF !Empty( ::cText )
         Document:nIndent ++
         cHtml += hb_eol() + HTML_SPACES + ::cText + hb_eol()
         Document:nIndent --
      ENDIF
   ENDIF

   cTagBody := ::Super:HtmlTagBody()

   IF !Empty( cTagBody ) .AND. Empty( ::cTag )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Elements of type WText cannot have properties assigned other than 'cText' (" + ::cText + "). Use a WSpan control"
         :Operation   := "WText:RunHtml()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
   ENDIF

   cHtml += cTagBody

RETURN cHtml

//------------------------------------------------------------------------------
