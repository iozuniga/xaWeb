
 * Proyect: XailerWeb framework
 * File: ZInput.prg
 * Description: Input-HTML controls base class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Note: This class is internal. Should not be instanciated directly
 */

#include "xailerweb.ch"

CLASS ZInput FROM WControl
EXPORTED:
   DATA cName              INIT ""
   DATA cAutoComplete      INIT "" // https://www.w3schools.com/tags/att_input_autocomplete.asp
   DATA cDirName           INIT ""
   DATA cForm              INIT ""
   DATA cFormAction        INIT ""
   DATA cFormEncType       INIT "" VALUES "application/x-www-form-urlencoded", ;
                                       "multipart/form-data",;
                                       "text/plain"
   DATA cFormMethod        INIT "" VALUES "get", "post"
   DATA cFormTarget        INIT "" VALUES "_blank", "_self", "_parent", "_top"
   DATA cDataField         INIT ""
   DATA lDisabledOnEdit    INIT .F.

   DATA lAutoFocus         INIT .F.
   DATA lDisabled          INIT .F.
   DATA lFormNoValidate    INIT .F.
   DATA lReadOnly          INIT .F.
   DATA lRequired          INIT .F.

   EVENT OnValidate( element )

PROTECTED:
   DATA cTag            INIT "input"
   DATA cType           INIT ""

RESERVED:
   METHOD HtmlTagBody()
   METHOD PreProcess()
HIDDEN:

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZInput

   LOCAL oForm
   LOCAL cId, cJs, cValid

   IF Empty( ::cId )
      cId := ::RandomId()
   ELSE
      cId := ::cId
   ENDIF

   IF ::IsEvent( "OnValidate" )
      cValid := ::EventValue( "OnValidate" )
      ::EventDelete( "OnValidate" )
      oForm := ::GetForm()
      IF oForm != NIL
         cJs := 'fv_' + oForm:cId + '.register("#' + cId + ;
                 '", "' + cValid + '");' + hb_eol()
         Document:AddScript( cJs, "init", .F. )
      ENDIF
   ENDIF

   IF !Empty( ::cDataField )
      ::AddDataset( "field", ::cDataField )
      ::cName := ::cDataField
   ENDIF

   IF ::lDisabledOnEdit
      ::AddDataset( "disabledOnEdit", iif( ::lDisabledOnEdit, "true", "false" ) )
   ENDIF

   IF Empty( ::cName )
      ::cName := cId
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZInput

   LOCAL cHtml := ""

   IF !Empty( ::cType )
      cHtml += ' type="' + ::cType + '"'
   ENDIF

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

   IF !Empty( ::cAutoComplete )
      cHtml += ' autocomplete="' + ::cAutoComplete + '"'
   ENDIF

   IF !Empty( ::cDirName )
      cHtml += ' dirname="' + ::cDirName + '"'
   ENDIF

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cForm + '"'
   ENDIF

   IF !Empty( ::cFormAction )
      cHtml += ' formaction="' + ::cFormAction + '"'
   ENDIF

   IF !Empty( ::cFormEncType )
      cHtml += ' formaenctype="' + ::cFormEncType + '"'
   ENDIF

   IF !Empty( ::cFormMethod )
      cHtml += ' formmethod="' + ::cFormMethod + '"'
   ENDIF

   IF !Empty( ::cFormTarget )
      cHtml += ' formmtarget="' + ::cFormTarget + '"'
   ENDIF

   IF ::lAutoFocus
      cHtml += ' autofocus'
   ENDIF

   IF ::lDisabled
      cHtml += ' disabled'
   ENDIF

   IF ::lFormNoValidate
      cHtml += ' formnovalidate'
   ENDIF

   IF ::lReadOnly
      cHtml += ' readonly'
   ENDIF

   IF ::lRequired
      cHtml += ' required'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
