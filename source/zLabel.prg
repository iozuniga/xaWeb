/*
 * Proyect: XailerWeb framework
 * File: ZLabel.prg
 * Description: HTML input-label class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZLabel FROM WControl
EXPORTED:
   DATA cTag            INIT "label"
   DATA cText           INIT ""
   DATA cFor            INIT ""
   DATA cForm           INIT ""

RESERVED:
   METHOD HtmlTagBody()
   METHOD Preprocess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS ZLabel

   IF Empty( ::cFor ) .AND. Engine:lDebug
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Labels should be associated with a "form" field. Use the "for" clause with the field ID'
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

