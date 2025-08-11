/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeInputText.prg
 * Descripción: class for Materialize input-text
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
  * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatInput.
 *       This is done automatically when using the xa-materialize.ch file.
 */


#include "xaWeb.ch"

ANNOUNCE ZMatInput

CLASS ZInputText FROM WInput
PUBLISHED:
   DATA oContainer   AS CLASS WDiv
   DATA oLabel       AS CLASS WLabel
   DATA oSupport     AS CLASS WSpan
   DATA oPrefix      AS CLASS WIconGoogle
   DATA oSuffix      AS CLASS WIconGoogle
   DATA oDivPrefix   AS CLASS WDiv
   DATA oDivSuffix   AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA cSupport        INIT ""
   DATA cPrefix         INIT ""
   DATA cSuffix         INIT ""
   DATA cPlaceHolder    INIT ""
   DATA lCharCounter    INIT .T.
   DATA lOutlined       INIT .F.
   DATA lValidate       INIT .T.

   METHOD oAutoComplete( value ) SETGET AS CLASS WAutoComplete

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()
   METHOD HtmlTagBody()
   METHOD oError() INLINE ::oSupport

PROTECTED:
   DATA foAutoComplete

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZInputText

   ::oContainer := WDiv():New( oParent, Self, .t. )

   ::oDivPrefix  := WDiv():New( ::oContainer, Self, .t. )
   ::oDivSuffix  := WDiv():New( ::oContainer, Self, .t. )

   ::oDivPrefix:AddClass( "prefix" )
   ::oDivSuffix:AddClass( "suffix" )

   ::oPrefix  := WIconGoogle():New( ::oDivPrefix, Self, .t. )
   ::oSuffix  := WIconGoogle():New( ::oDivSuffix, Self, .t. )

   ::Super:New( ::oContainer, Self )

   ::oLabel   := WLabel():New( ::oContainer, Self, .t. )
   ::oSupport := WSpan():New( ::oContainer, Self, .t. )

   ::oContainer:AddClass( "input-field" )
   ::oContainer:AddClass( "xa-input" )

   ::AddClass( "xa-input__input" )

   ::oSupport:AddClass( "xa-input__error" )
   ::oSupport:AddClass( "supporting-text" )

   ::cPlaceHolder := " "
   ::oLabel:cFor  := "auto"

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZInputText

   ::oContainer := NIL
   ::oDivPrefix := NIL
   ::oDivSuffix := NIL
   ::oPrefix    := NIL
   ::oSuffix    := NIL
   ::oLabel     := NIL
   ::oSupport   := NIL

   IF HB_IsObject( ::foAutoComplete )
      ::foAutoComplete:End()
      ::foAutoComplete := NIL
   ENDIF

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD oAutoComplete( oValue ) CLASS ZInputText

   IF PCount() > 0
      ::foAutoComplete := oValue
   ENDIF

   IF ::foAutoComplete == NIL
      ::foAutoComplete := WAutoComplete():New( Self, Self, .t. )
   ENDIF

RETURN ::foAutoComplete

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZInputText

   LOCAL cJs, cId

   cId := ::ValidId()

   IF ::lOutlined
      ::oContainer:AddClass( "outlined" )
   ENDIF

   IF !Empty( ::cLabel )
      ::oLabel:cText := ::cLabel
   ENDIF

   IF Empty( ::oLabel:cText )
      ::oContainer:RemoveControl( ::oLabel )
      ::oLabel := NIL
   ELSE
      WITH OBJECT ::oLabel
         :cFor  := cId
         :cForm := ::cForm
      END WITH
   ENDIF

   IF !Empty( ::cSupport )
      ::oSupport:cText := ::cSupport
   ELSEIF !::lValidate
      ::oSupport:oStyle:Display := "none"
   ENDIF

   IF Empty( ::cPrefix  )
      ::oContainer:RemoveControl( ::oDivPrefix )
      ::oDivPrefix := NIL
      ::oPrefix := NIL
   ELSE
      ::oPrefix:cText := ::cPrefix
   ENDIF

   IF Empty( ::cSuffix  )
      ::oContainer:RemoveControl( ::oDivSuffix )
      ::oDivSuffix := NIL
      ::oSuffix := NIL
   ELSE
      ::oSuffix:cText := ::cSuffix
   ENDIF

   IF ::foAutocomplete != NIL
      ::foAutoComplete:PreProcess( cId )
      ::AddClass( "autocomplete" )
   ENDIF

   IF ::lCharCounter
      cJs := " M.CharacterCounter.init(document.getElementById('" + cId + "'));" + hb_eol()
      Document:oContext:AddCodeOnLoad( cJs )
   ENDIF

   IF !Empty( ::cPlaceHolder ) .AND. ::NeedTranslate()
      ::cPlaceHolder := Translator():Translate( ::cPlaceHolder )
   ENDIF

   IF ::cType == "hidden"
      ::oContainer:oStyle:Display := "none"
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZInputText

   LOCAL cHtml := ""
   IF !( ::cPlaceHolder == "" )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------


