/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeSelect.prg
 * Descripción: class for Materialize switch
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * TODO: Dropdown options in case is necessary
 */

#include "xaWeb.ch"

ANNOUNCE ZMatSelect

CLASS ZSelect FROM WBasicSelect
PUBLISHED:
   DATA oContainer      AS CLASS WDiv
   DATA oLabel          AS CLASS WLabel
   DATA cLabel          INIT ""
   DATA cPlaceHolder    INIT ""
   DATA lOutlined       INIT .F.
   DATA lMultiple       INIT .F.
   DATA lBrowserDefault INIT .F.

   METHOD AddItem( cValue, cLabel, cImage )  // --> WSelectItem

RESERVED:
   DATA lImages      INIT .F.

   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD End() INLINE (::oContainer := NIL, ::oLabel := NIL, ::Super:End() )
   METHOD PreProcess()
   METHOD HtmlTagBody()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZSelect

   ::oContainer := WDiv():New( oParent, Self, .t. )
   ::Super:New( ::oContainer, Self )
   ::oLabel  := WLabel():New( ::oContainer, Self, .t. )

   ::oContainer:AddClass( "input-field" )
   ::cPlaceHolder := " "

RETURN Self

//------------------------------------------------------------------------------

METHOD AddItem( cValue, cLabel, cImage ) CLASS ZSelect

   LOCAL oItem

   oItem := ::Super:AddItem( cValue, cLabel )

   IF !Empty( cImage )
      oItem:cImage := cImage
   ENDIF

RETURN oItem

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZSelect

   LOCAL hOpt := { => }
   LOCAL cId, cJs

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF !Empty( ::cLabel )
      ::oLabel:cText := ::cLabel
      ::oLabel:cFor := cId
   ELSE
      ::oContainer:RemoveControl( ::oLabel )
   ENDIF

   IF ::lOutlined
      ::oContainer:AddClass( "outlined" )
   ENDIF

   IF ::lMultiple
      ::AddClass( "multiple" )
   ENDIF

   IF ::lBrowserDefault
      ::Addclass( "browser-default" )
   ENDIF

   cJs := " M.FormSelect.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )


RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZSelect

   LOCAL cHtml := ""

   IF !( ::cPlaceHolder == "" )
      cHtml += ' placeholder="' + ::cPlaceHolder + '"'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZSelectItem FROM WBasicSelectItem
PUBLISHED:
   DATA cImage       INIT ""

RESERVED:
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZSelectItem

   IF !Empty( ::cImage )
      ::AddDataset( "icon", ::cImage )
      WITH OBJECT ::oParent
         IF !:lImages
            :AddClass( "icons" )
            :lImages := .T.
         ENDIF
      END WITH
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
