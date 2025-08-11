/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeTextArea.prg
 * Descripción: class for Materialize TextArea
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
  * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatTextArea.
 *       This is done automatically when using the xa-materialize.ch file.
 *
 * Note from Materialize:
 *      When dynamically changing the value of a textarea like setting
 *      HTMLElement#value attribute, you must trigger an autoresize on it
 *      afterwords because simply updating the element's value does not automatically
 *      trigger the events we've binded to the textarea.
 *      M.Forms.textareaAutoResize(textAreaElement);
 */

#include "xaWeb.ch"

ANNOUNCE ZMatTextArea

CLASS ZTextArea FROM WControl
PUBLISHED:
   DATA oContainer   AS CLASS WDiv
   DATA oLabel       AS CLASS WLabel
   DATA cText        INIT ""
   DATA cValue       INIT "" //PERSISTENT
   DATA cPlaceHolder INIT ""
   DATA cWrap        INIT "" VALUES "hard", "soft"
   DATA cLabel       INIT ""
   DATA cForm        INIT ""
   DATA cName        INIT ""

   DATA nRows        INIT NIL
   DATA nCols        INIT NIL
   DATA nMaxLength   INIT NIL

   DATA lReadOnly    INIT .F.

PROTECTED:
   DATA cTag          INIT "textarea"
   DATA lEndTagForced INIT .T.

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()
   METHOD HtmlTagBody()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZTextArea

   ::oContainer := WDiv():New( oParent, Self, .t. )
   ::oContainer:AddClass( "input-field" )

   ::Super:New( ::oContainer, Self, lAuto )
   ::AddClass( "materialize-textarea" )

   ::oLabel := WLabel():New( ::oContainer, Self, .t. )

   ::cPlaceHolder := " "

   ::oLabel:cFor := "auto"

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZTextArea

   ::oContainer := NIL
   ::oLabel     := NIL

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZTextArea

   LOCAL cHtml := ""

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

   IF !Empty( ::cPlaceHolder )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   IF HB_IsNumeric( ::nMaxLength )
      cHtml += ' maxlength="' + ToString( ::nMaxLength ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nRows )
      cHtml += ' rows="' + ToString( ::nRows ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nCols )
      cHtml += ' cols="' + ToString( ::nCols ) + '"'
   ENDIF

   IF !Empty( ::cWrap )
      cHtml += ' wrap="' + ::cWrap + '"'
   ENDIF

   IF ::lReadOnly
      cHtml += ' readonly'
   ENDIF

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cForm + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTextArea

   LOCAL cId, cJs

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF !Empty( ::cLabel )
      ::oLabel:cText := ::cLabel
   ENDIF

   IF Empty( ::oLabel:cText )
      ::oContainer:RemoveControl( ::oLabel )
      ::oLabel := NIL
   ELSE
      WITH OBJECT ::oLabel
         :cFor     := cId
         :cForm    := ::cForm
      END WITH
   ENDIF

   cJs := " M.CharacterCounter.init(document.getElementById('" + cId + "'));" + hb_eol()

   Document:oContext:AddCodeOnLoad( cJs )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------


