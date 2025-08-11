/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeButton.prg
 * Descripción: class for Materialbox images
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZMaterialBox FROM WImage
PUBLISHED:
   DATA cCaption        INIT ""
   DATA nInDuration     INIT 275
   DATA nOutDuration    INIT 200
   DATA cOnOpenStart    INIT ""
   DATA cOnOpenEnd      INIT ""
   DATA cOnCloseStart   INIT ""
   DATA cOnCloseEnd     INIT ""

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZMaterialBox

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "materialboxed" )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZMaterialBox

   LOCAL hOpt := { => }
   LOCAL cJs, cId

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   IF ::nInDuration != 275
      HB_HSet( hOpt, "inDuration", ::nInDuration )
   ENDIF
   IF ::nOutDuration != 200
      HB_HSet( hOpt, "outDuration", ::nOutDuration )
   ENDIF
   IF !Empty( ::cOnOpenStart )
      HB_HSet( hOpt, "onOpenStart", ::cOnOpenStart )
   ENDIF
   IF !Empty( ::cOnOpenEnd )
      HB_HSet( hOpt, "onOpenEnd", ::cOnOpenEnd )
   ENDIF
   IF !Empty( ::cOnCloseStart )
      HB_HSet( hOpt, "onCloseStart", ::cOnCloseStart )
   ENDIF
   IF !Empty( ::cOnCloseEnd )
      HB_HSet( hOpt, "onCloseEnd", ::cOnCloseEnd )
   ENDIF

   cJs := " M.Materialbox.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

   IF !Empty( ::cCaption )
      ::AddDataset( "caption", ::cCaption )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

