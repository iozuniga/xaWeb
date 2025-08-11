/*
 * Proyecto: xaWeb framework
 * Fichero: ZContainer.prg
 * Descripción: Base class for HTML containers
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This class is internal. Should not be instanciated directly
 *       To create a new container, consider inherit from WDiv
 *       since includes the basic Html() method functionality
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZContainer FROM WBasic
PUBLISHED:
   DATA lTextAfter   INIT .F.

   METHOD ControlById( cID )      // --> oControl

RESERVED:
   DATA oParent      READONLY
   DATA aControls    INIT {}

   METHOD End()
   METHOD InsertControl( oControl AS CLASS WControl )
   METHOD RemoveControl( oControl AS CLASS WControl )
   METHOD SwapControls( xCtl1, xCtl2 )
   METHOD ChildrenHtml()
   METHOD GenJS( aData, aEvent )
   METHOD PreProcess()
   METHOD HtmlTagEnd()
   METHOD IsChildren()

PROTECTED:
   DATA cText        INIT NIL   // Es importante que no se asigne NUNCA
   DATA lCloseTag    INIT .T.

ENDCLASS

//------------------------------------------------------------------------------

METHOD ControlById( cID ) CLASS ZContainer

   cID := Lower( cID )

   IF Lower( ::cID ) == cID
      RETURN Self
   ENDIF

RETURN FindControl( Self, cId )

//------------------------------------------------------------------------------

STATIC FUNCTION FindControl( oOwner, cId )

   LOCAL oControl, oFound

   FOR EACH oControl IN oOwner:aControls
      IF Lower( oControl:cId ) == cId
         oFound := oControl
      ELSE
         oFound := FindControl( oControl, cId )
      ENDIF
      IF HB_IsObject( oFound )
         EXIT
      ENDIF
   NEXT

RETURN oFound

//------------------------------------------------------------------------------

METHOD End() CLASS ZContainer

   LOCAL oCtl

   FOR EACH oCtl IN ::aControls
      IF HB_IsObject( oCtl )
         oCtl:End()
      ENDIF
   NEXT

   ::aControls := {}

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD InsertControl( oControl ) CLASS ZContainer

   AAdd( ::aControls, oControl )

RETURN Len( ::aControls )

//------------------------------------------------------------------------------

METHOD PreProcess()  CLASS ZContainer

   LOCAL oControl

   FOR EACH oControl IN ::aControls
      IF HB_IsObject( oControl )
         oControl:PreProcess()
      ENDIF
   NEXT

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
// No es buena idea modificar la dimensión de aControls, pues posiblemente se
// haga desde un bucle FOR EACH oCtl IN ::aControls. Es preferible pasar el
// elemento a NIL y hacer el control de tipo

METHOD RemoveControl( oControl ) CLASS ZContainer

   LOCAL n

   IF ( n := AScan( ::aControls, {| Ctl | Ctl == oControl } ) ) > 0
      ::aControls[ n ]:oParent := NIL
      ::aControls[ n ] := NIL
      //HB_ADel( ::aControls, n, .T. )
   ELSE
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := "Control not found on class " + ::Classname +;
                         " (" + oControl:Classname + ")"
         :Operation   := "WContainer:RemoveControl()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

RETURN ( n > 0 )

//------------------------------------------------------------------------------

METHOD SwapControls( xCtl1, xCtl2 ) CLASS ZContainer

   LOCAL oTemp
   LOCAL lSuccess

   IF Document:lOnPreProcess
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'You can not swap controls inside a Preprocess event'
         :Operation   := "WContainer:SwapControl()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
   ENDIF

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

METHOD IsChildren()

   LOCAL oCtl

   FOR EACH oCtl IN ::aControls
      IF HB_IsObject( oCtl )
         RETURN .T.
      ENDIF
   NEXT

RETURN .F.

//------------------------------------------------------------------------------

METHOD ChildrenHtml() CLASS ZContainer

   LOCAL oCtl
   LOCAL cHtml := ""

   Document:nIndent ++

   FOR EACH oCtl IN ::aControls
      IF HB_IsObject( oCtl )
         cHtml += oCtl:RunHtml()
      ENDIF
   NEXT

   Document:nIndent --

RETURN cHtml

//------------------------------------------------------------------------------

METHOD GenJS( aData, aEvent ) CLASS ZContainer

   LOCAL oCtl

   FOR EACH oCtl IN ::aControls
      IF HB_IsObject( oCtl )
         oCtl:GenJS( aData, aEvent )
      ENDIF
   NEXT

RETURN ::Super:GenJS( aData, aEvent )

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZContainer

   LOCAL cHtml, cText
   LOCAL lChild, lAfter

   cText  := ::cText
   lChild := ::IsChildren()
   lAfter := ::lTextAfter

   IF !Empty( cText ) .AND. ::NeedTranslate()
      cText := Translator():Translate( cText )
   ENDIF

   cHtml := '>'

   IF !lAfter .AND. !Empty( cText )
      cHtml += cText
   ENDIF

   IF lChild
      cHtml += hb_eol() + ::ChildrenHtml()
   ENDIF

   IF lAfter .AND. !Empty( cText )
      IF lChild
         Document:nIndent ++
         cHtml += HTML_SPACES + cText + hb_eol()
         Document:nIndent --
      ELSE
         cHtml += cText
      ENDIF
   ENDIF

   IF ::lCloseTag
      cHtml += IIF( lChild, HTML_SPACES, "" ) + '</' + ::cTag + '>' + hb_eol()
   ENDIF

RETURN cHtml

//------------------------------------------------------------------------------

