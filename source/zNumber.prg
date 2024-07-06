/*
 * Proyect: XailerWeb framework
 * File: ZNumber.prg
 * Description: Input-HTML type number class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZNumber FROM WInput
EXPORTED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA oError          AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA cText           INIT ""
   DATA cPlaceHolder    INIT ""
   DATA nValue          INIT NIL PERSISTENT
   DATA nMax            INIT NIL
   DATA nMin            INIT NIL
   DATA nStep           INIT NIL
   DATA nSize           INIT NIL
   DATA lLabelNewLine   INIT .F.

   METHOD New( oParent ) CONSTRUCTOR

PROTECTED:
   DATA cType        INIT "number"

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZNumber

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

METHOD PreProcess() CLASS ZNumber

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

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZNumber

   LOCAL cHtml := ""

   IF HB_IsNumeric( ::nValue )
      cHtml += ' value="' + ToString( ::nValue ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMax )
      cHtml += ' max="' + ToString( ::nMax ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nMin )
      cHtml += ' min="' + ToString( ::nMin ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nStep )
      cHtml += ' step="' + ToString( ::nStep ) + '"'
   ENDIF

   IF HB_IsNumeric( ::nSize )
      cHtml += ' size="' + ToString( ::nSize ) + '"'
   ENDIF

   IF !Empty( ::cPlaceHolder )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
