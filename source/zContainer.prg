/*
 * Proyect: XailerWeb framework
 * File: ZContainer.prg
 * Description: Base class for HTML containers
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Note: This class is internal. Should not be instanciated directly
 *       To create a new container, consider inherit from WDiv
 *       since includes the basic Html() method functionality
 */

#include "xailerweb.ch"

CLASS ZContainer FROM WControl
EXPORTED:
   DATA oParent READONLY

   METHOD New( oParent, oOwner ) CONSTRUCTOR

RESERVED:
   DATA aControls    INIT {}
   DATA cText        INIT NIL   // Es importante que no se asigne NUNCA

   METHOD InsertControl( oControl )
   METHOD RemoveControl( oControl )
   METHOD SwapControls( xCtl1, xCtl2 )
   METHOD ChildrenHtml()
   METHOD GenJS( aData, aEvent )
   METHOD PreProcess()
   METHOD HtmlTagEnd()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner ) CLASS ZContainer

   ::Super:New( oParent, oOwner )

RETURN Self

//------------------------------------------------------------------------------

METHOD InsertControl( oControl ) CLASS ZContainer

   AAdd( ::aControls, oControl )

RETURN Len( ::aControls )

//------------------------------------------------------------------------------

METHOD PreProcess()  CLASS ZContainer

   AEval( ::aControls, {|v| v:PreProcess() } )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD RemoveControl( oControl ) CLASS ZContainer

   LOCAL n

   IF ( n := AScan( ::aControls, {| Ctl | Ctl == oControl } ) ) > 0
      ::aControls[ n ]:oParent := NIL
      HB_ADel( ::aControls, n, .T. )
   ENDIF

RETURN ( n > 0 )

//------------------------------------------------------------------------------

METHOD SwapControls( xCtl1, xCtl2 ) CLASS ZContainer

   LOCAL oTemp
   LOCAL lSuccess

   IF !HB_IsNumeric( xCtl1 )
      xCtl1 := AScan( ::aControls, {|oCtl| xCtl1 == oCtl } )
   ENDIF

   IF !HB_IsNumeric( xCtl2 )
      xCtl2 := AScan( ::aControls, {|oCtl| xCtl2 == oCtl } )
   ENDIF

   lSuccess := .F.

   IF xCtl1 > 0 .AND. xCtl2 > 0 .AND. xCtl1 != xCtl2
      oTemp := ::aControls[ xCtl2 ]
      ::aControls[ xCtl2 ] := ::aControls[ xCtl1 ]
      ::aControls[ xCtl1 ] := oTemp
      lSuccess := .T.
   ENDIF

RETURN lSuccess

//------------------------------------------------------------------------------

METHOD ChildrenHtml() CLASS ZContainer

   LOCAL oCtl
   LOCAL cHtml := ""

   Document:nIndent ++

   FOR EACH oCtl IN ::aControls
      cHtml += oCtl:RunHtml()
   NEXT

   Document:nIndent --

RETURN cHtml

//------------------------------------------------------------------------------

METHOD GenJS( aData, aEvent ) CLASS ZContainer

   LOCAL oCtl

   FOR EACH oCtl IN ::aControls
      oCtl:GenJS( aData, aEvent )
   NEXT

RETURN ::Super:GenJS( aData, aEvent )

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZContainer

   LOCAL cHtml
   LOCAL lChild

   lChild := ( Len( ::aControls ) > 0 )

   IF !Empty( ::cText )
      cHtml := '>' + ::cText + IIF( !lChild, '</' + ::cTag + '>', '' ) + hb_eol()
   ELSE
      cHtml := '>' + IIF( !lChild, '</' + ::cTag + '>', '' ) + hb_eol()
   ENDIF

   IF lChild
      cHtml += ::ChildrenHtml()
      cHtml += HTML_SPACES + '</' + ::cTag + '>' + hb_eol()
   ENDIF

RETURN cHtml

//------------------------------------------------------------------------------

