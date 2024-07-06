/*
 * Proyect: XailerWeb framework
 * File: ZPackage.prg
 * Description: Base class for HTML packages
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZPackage
EXPORTED:
   DATA oDoc         INIT NIL AS CLASS WDoc
   DATA aScripts     INIT {}  AS CLASS WScript
   DATA aDepends     INIT {}  AS CLASS WPackage
   DATA aCSS         INIT {}  AS CLASS WCss
   DATA cName        INIT ""  READONLY

   METHOD New( oDoc, nPos )  CONSTRUCTOR

   METHOD AddScript( cText, cName, lUri )
   METHOD AddCSS( cText, cName, llUri )
   METHOD AddDependency( cPackage ) INLINE AAdd( ::aDepends, Lower( cPackage ) )

RESERVED:
   METHOD Render()

PROTECTED:
   DATA nIndex       INIT 0

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oDoc, nPos ) CLASS ZPackage

   LOCAL nAt

   DEFAULT nPos TO 0

   IF !HB_IsObject( oDoc )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'Invalid Document object'
         :Operation   := "WPackage:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
         RETURN Self
      END WITH
   ENDIF

   IF Empty( ::cName )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'Invalid Package name'
         :Operation   := "WPackage:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
         RETURN Self
      END WITH
   ENDIF

   ::oDoc := oDoc
   nAt    := AScan( oDoc:aPackages, {|v| v:cName == ::cName } )

   IF !Empty( nAt )
      oDoc:aPackages[ nAt ] := Self
   ELSE
      IF nPos > 0 .AND. nPos <= Len( oDoc:aPackages )
         HB_AIns( oDoc:aPackages, nPos, Self, .T. )
         AEval( oDoc:aPackages, {|v,e| v:nIndex := e, nPos + 1 } )
      ELSE
         AAdd( oDoc:aPackages, Self )
         ::nIndex := Len( oDoc:aPackages )
      ENDIF
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD Render() CLASS ZPackage

   LOCAL oCss, oScript

   FOR EACH oCss IN ::aCSS
      IF !Empty( oCss:cCode ) .OR. !::oDoc:IsCSS( oCss:cUrl )
         Aadd( ::oDoc:aCSS, oCss )
      ENDIF
   NEXT

   FOR EACH oScript IN ::aScripts
      IF !Empty( oScript:cCode ) .OR. !::oDoc:IsScript( oScript:cUrl )
         Aadd( ::oDoc:aScripts, oScript )
      ENDIF
   NEXT

RETURN NIL

//------------------------------------------------------------------------------

METHOD AddScript( cText, cName, lUri ) CLASS ZPackage

   LOCAL oScript
   LOCAL nAt

   IF Empty( cText )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Parameter "cText" empty. Script not included'
         :Operation   := "WPackage:AddScript()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
   ENDIF

   DEFAULT cName TO ""
   DEFAULT lUri  TO !( hb_eol() $ cText ) .AND. !( "FUNCTION" $ Upper( cText ) )

   IF !Empty( cName ) .AND. !lUri
      cName := Upper( cName )
      nAt := AScan( ::aScripts, {|v| Upper( v:cName ) == cName } )
      IF nAt > 0
         ::aScripts[ nAt ]:AddCode( cText )
         RETURN ::aScripts[ nAt ]
      ENDIF
   ENDIF

   oScript := WScript():New( cText, cName, lUri )

   AAdd( ::aScripts, oScript )

RETURN oScript

//------------------------------------------------------------------------------

METHOD AddCSS( cText, cName, lUri ) CLASS ZPackage

   LOCAL oCss

   DEFAULT cName TO ::cName

   oCss := WCss():New( cText, cName, lUri )

   AAdd( ::aCss, oCss )

RETURN oCss

//------------------------------------------------------------------------------

