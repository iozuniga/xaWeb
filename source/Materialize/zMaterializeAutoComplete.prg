/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeAutocomplete.prg
 * Descripción: class for Materialize input-text autocomplete
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * TODO: Dropdown options in case is necessary
 */


#include "xaWeb.ch"

ANNOUNCE ZMatAutoComplete

CLASS ZAutoComplete
PUBLISHED:
   DATA oParent            AS CLASS WInputText
   DATA aItems             INIT {} // Array of hashes( "id", "text", "image" )
   DATA nMinLength         INIT 1
   DATA lIsMultiSelct      INIT .F.
   DATA cMaxDropDownHeight INIT "300px"
   DATA cOnAutoComplete    INIT ""
   DATA cOnSearch          INIT ""
   DATA lAllowUnsafeHTML   INIT .F.

   METHOD AddItem( cId OPTIONAL, cText OPTIONAL, cImage OPTIONAL )  // --> hHash
RESERVED:
   METHOD PreProcess()
   METHOD End()   VIRTUAL

ENDCLASS

//------------------------------------------------------------------------------

METHOD AddItem( cId, cText, cImage ) CLASS ZAutoComplete

   LOCAL hHash := { => }

   DEFAULT cId    TO ToString( Len( ::aItems ) + 1 ),;
           cText  TO "",;
           cImage TO ""

   HB_HSet( hHash, "id", cId )
   HB_HSet( hHash, "text", cText )
   HB_HSet( hHash, "image", cImage )

   AAdd( ::aItems, hHash )

RETURN hHash

//------------------------------------------------------------------------------

METHOD PreProcess( cId ) CLASS ZAutoComplete

   LOCAL hOpt := { => }
   LOCAL cJs

   IF Len( ::aItems ) == 0
      RETURN nil
   ENDIF

   IF ::lIsMultiSelct
      HB_HSet( hOpt, "isMultiSelect", .T. )
   ENDIF

   IF ::nMinLength != 1
      HB_HSet( hOpt, "minLength", ::nMinLength )
   ENDIF

   IF ::cMaxDropDownHeight != "300px"
      HB_HSet( hOpt, "maxDropDownHeight", ::cMaxDropDownHeight )
   ENDIF

   IF !Empty( ::cOnAutoComplete )
      HB_HSet( hOpt, "onAutoComplete", ::cOnAutoComplete )
   ENDIF

   IF !Empty( ::cOnSearch )
      HB_HSet( hOpt, "onSearch", ::cOnSearch )
   ENDIF

   IF ::lAllowUnsafeHTML
      HB_HSet( hOpt, "allowUnsafeHTML", .T. )
   ENDIF

   HB_HSet( hOpt, "data", ::aItems )

   cJs := " M.Autocomplete.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()

   Document:oContext:AddCodeOnLoad( cJs )

RETURN NIL

//------------------------------------------------------------------------------


