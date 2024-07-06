/*
 * Proyect: XailerWeb framework
 * File: ZDateTime.prg
 * Description: Input-DateTime HTML controls
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

CLASS ZDateTime FROM WInput
EXPORTED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA oError          AS CLASS WDiv

   DATA cLabel          INIT ""
   DATA xValue          INIT NIL PROPERTY
   DATA cType           INIT "date" VALUES "date", "datetime-local", "month", "time", "week"
   DATA nMax            INIT NIL
   DATA nMin            INIT NIL
   DATA nStep           INIT NIL
   DATA lLabelNewLine   INIT .F.

   METHOD New( oParent ) CONSTRUCTOR

RESERVED:
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZDateTime

   ::oContainer := WDiv():New( oParent )
   ::oLabel := WLabel():New( ::oContainer )
   ::Super:New( ::oContainer, Self )
   ::oError := WDiv():New( ::oContainer )

   ::AddClass( "xw-input__input" )
   ::oContainer:AddClass( "xw-input" )
   ::oLabel:AddClass( "xw-input__label" )
   ::oError:AddClass( "xw-input__error" )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZDateTime

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

METHOD HtmlTagBody() CLASS ZDateTime

   LOCAL cHtml := ""

   IF HB_IsDate( ::xValue )
      cHtml += ' value="' +  HB_DToC( ::xValue, "YYYY-MM-DD" ) + '"'
   ELSEIF HB_IsDateTime( ::xValue )
      cHtml += ' value="' +  HB_TToC( ::xValue, "YYYY-MM-DD", "HH:MM:SS" ) + '"'
   ELSEIF HB_IsNumeric( ::xValue )
      cHtml += ' value="' + ToString( ::xValue ) + '"'
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

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
