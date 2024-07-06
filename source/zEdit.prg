/*
 * Proyect: XailerWeb framework
 * File: ZInputText.prg
 * Description: class for Input type text HTML elements
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZEdit FROM WInput
EXPORTED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA oError          AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA cType           INIT "text" VALUES "text", "password", "tel", "url", "search"
   DATA cText           INIT ""
   DATA cValue          INIT "" PERSISTENT
   DATA cPattern        INIT ""
   DATA cPlaceHolder    INIT ""
   DATA nSize           INIT NIL
   DATA nMaxLength      INIT NIL
   DATA nMinLength      INIT NIL
   DATA lLabelNewLine   INIT .F.

   METHOD New( oParent ) CONSTRUCTOR

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZEdit

   ::oContainer := WDiv():New( oParent, nil )
   ::oLabel := WLabel():New( ::oContainer, nil )
   ::Super:New( ::oContainer, Self )
   ::oError := WDiv():New( ::oContainer, nil )

   ::AddClass( "xw-input__input" )
   ::oContainer:AddClass( "xw-input" )
   ::oLabel:AddClass( "xw-input__label" )
   ::oError:AddClass( "xw-input__error" )

   ::oLabel:cFor := "auto"

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZEdit

   LOCAL cId

   IF Empty( ::cId )
      cId := ::RandomId()
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
         IF ::lLabelNewLine
            :AddStyle( "display:block;" )
         ENDIF
      END WITH
   ENDIF

   IF Empty( ::nSize )
      ::AddStyle( "width: 100%;" )
   ELSE
      ::AddStyle( "width: " + ToString( ::nSize ) + "ch;"  )
      ::nSize := NIL
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZEdit

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF !Empty( ::cPattern )
      cHtml += ' pattern="' + ::cPattern + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMaxLength )
      cHtml += ' maxlength="' + ToString( ::nMaxLength ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMinLength )
      cHtml += ' minlength="' + ToString( ::nMinLength ) + '"'
   ENDIF

   IF !Empty( ::cPlaceHolder )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
