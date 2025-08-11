/*
 * Proyecto: xaWeb framework
 * Fichero: ZLabel.prg
 * Descripción: HTML input-label class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZLabel FROM WControl
PUBLISHED:
   DATA cText           INIT ""
   DATA cFor            INIT ""
   DATA cForm           INIT ""

PROTECTED:
   DATA cTag            INIT "label"

RESERVED:
   METHOD HtmlTagBody()
   METHOD Preprocess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS ZLabel

   LOCAL cLabel

   IF Empty( ::cFor ) .AND. Engine:lDebug
      WITH OBJECT ErrorNew()
         IF !Empty( ::cText )
            cLabel := ::cText
         ELSE
            cLabel := ::ValidId()
         ENDIF
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Label ['+ cLabel + '] should be associated with a "form" field. Use the "FOR" clause with the field ID'
         :Operation   := "WLabel:cFor assignment recommendation"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

RETURN ::Super:Preprocess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZLabel

   LOCAL cHtml := ""

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cForm + '"'
   ENDIF

   IF !Empty( ::cFor )
      cHtml += ' for="' + ::cFor + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

