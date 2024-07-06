/*
 * Proyect: XailerWeb framework
 * File: ZEmail.prg
 * Description: Input-Email HTML control class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZEmail FROM WInput
EXPORTED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA oError          AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA cPlaceHolder    INIT ""
   DATA cValue          INIT "" PROPERTY
   DATA cText           INIT ""
   DATA nMaxLength      INIT NIL
   DATA nMinLength      INIT NIL
   DATA nSize           INIT NIL
   DATA lMultiple       INIT .F.
   DATA lLabelNewLine   INIT .F.

   METHOD New( oParent ) CONSTRUCTOR

PROTECTED:
   DATA cType        INIT "email"

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZEmail

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

METHOD PreProcess() CLASS ZEmail

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

METHOD HtmlTagBody() CLASS ZEmail

   LOCAL cHtml := ""

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF !Empty( ::cPlaceHolder )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   IF ::lMultiple
      cHtml += ' multiple'
   ENDIF

   IF HB_IsNumeric( ::nMaxLength )
      cHtml += ' maxlength="' + ToString( ::nMaxLength ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMinLength )
      cHtml += ' minlength="' + ToString( ::nMinLength ) + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------