/*
 * Proyect: XailerWeb framework
 * File: zListbox.prg
 * Description: HTMLListbox class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZListbox FROM WControl
EXPORTED:
   DATA aItems       INIT {}              // {{value1, text1}, ..., {valueN, textN}}
   DATA cValue       INIT "" PERSISTENT
   DATA cForm        INIT ""
   DATA cName        INIT ""
   DATA nSize        INIT NIL
   DATA lAutofocus   INIT .F.
   DATA lDisabled    INIT .F.
   DATA lMultiple    INIT .F.
   DATA lRequired    INIT .F.

PROTECTED:
   DATA cTag         INIT "select"

RESERVED:
   METHOD HtmlTagBody()
   METHOD HtmlTagEnd()

ENDCLASS

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZListbox

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cForm + '"'
   ENDIF

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   IF ::lAutofocus
      cHtml += ' autofocus'
   ENDIF

   IF ::lDisabled
      cHtml += ' disabled'
   ENDIF

   IF ::lMultiple
      cHtml += ' multiple'
   ENDIF

   IF ::lRequired
      cHtml += ' required'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZListbox

   LOCAL aRow
   LOCAL cHtml, cSel, cVal

   cHtml := ::Super:HtmlTagEnd()
   cVal  := ::cValue

   Document:nIndent ++

   FOR EACH aRow IN ::aItems

      IF !Empty( cVal ) .AND. cVal == aRow[ 1 ]
         cSel := "selected "
      ELSE
         cSel := ""
      ENDIF

      cHtml += HTML_SPACES + '<option ' + cSel + 'value="' + aRow[ 1 ] + '">' + ;
               aRow[ 2 ] + '</option>' + hb_eol()
   NEXT

   Document:nIndent --

   cHtml += HTML_SPACES + '</' + ::cTag + '>' + hb_eol()

RETURN cHtml