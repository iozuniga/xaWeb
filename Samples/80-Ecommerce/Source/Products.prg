/*
 * Proyecto: xaECommerce
 * Fichero: Products.prg
 * Descripción:
 * Autor:
 * Fecha: 01/02/2025
 */

#include "xa-materialize.ch"

CLASS WProducts

   DATA oParent
   DATA aProducts INIT {} AS CLASS WProduct

   METHOD New( oParent )
   METHOD LoadProducts( nType )
   METHOD Html( oParent )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS WProducts

   ::oParent := oParent

RETURN Self

//------------------------------------------------------------------------------

METHOD LoadProducts( nType ) CLASS WProducts


RETURN ::aProducts

//------------------------------------------------------------------------------

METHOD Html( oParent ) CLASS WProducts


      WITH OBJECT WDiv():New( oParent )
         :oContext:Container()
         :oContext:Row()
         :oStyle:Margin_top := "1rem;"
         :oStyle:Margin_bottom := "1rem;"
         :AddStyle( "gap: 20px;" )
         WITH OBJECT WCard():New( SO )
            :oContext:Col( 12, 6, 4 )
            :oTitle:cText := "Card Title"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link" )
            :AddAction( "This is a link" )
         END WITH
         WITH OBJECT WCard():New( SO )
            :oContext:Col( 12, 6, 4 )
            :oTitle:cText := "Card Title"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link" )
            :AddAction( "This is a link" )
         END WITH
         WITH OBJECT WCard():New( SO )
            :oContext:Col( 12, 6, 4 )
            :oTitle:cText := "Card Title"
            :oText:cText := "I am a very simple card. I am good at containing small bits of information. I am convenient because I require little markup to use effectively."
            :AddAction( "This is a link" )
            :AddAction( "This is a link" )
         END WITH
      END WITH

RETURN NIL

//------------------------------------------------------------------------------


