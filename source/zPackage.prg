/*
 * Proyecto: xaWeb framework
 * Fichero: ZPackage.prg
 * Descripción: Base class for HTML packages
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZPackage
PUBLISHED:
   DATA oDoc         INIT NIL AS CLASS WDoc
   DATA aScripts     INIT {}  AS CLASS WScript
   DATA aCSS         INIT {}  AS CLASS WCss

   METHOD New( oDoc, nPos, lAuto )  CONSTRUCTOR

   METHOD AddScript( cText, cName OPTIONAL, lUri OPTIONAL, lTop OPTIONAL )  // --> WScript
   METHOD AddCSS( cText, cName OPTIONAL, llUri OPTIONAL )    // --> WCss
   METHOD AddDependency( cPackage ) INLINE AAdd( ::aDepends, cPackage )

RESERVED:
   DATA cName        INIT ""  READONLY
   DATA aDepends     INIT {}  AS CLASS WPackage
   DATA aParseInfo   INIT {}
   DATA lAutoCreated INIT .F.

   METHOD End()
   METHOD GetCssByUrl( cUrl ) // --> WCss
   METHOD Render()
   METHOD PreProcess()  VIRTUAL

PROTECTED:
   DATA nIndex       INIT 0

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oDoc, nPos, lAuto ) CLASS ZPackage

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

   IF Document:lOnPreProcess
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'You can not create Packages inside a Preprocess event'
         :Operation   := "WPackage:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
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

   WITH OBJECT Engine
      IF !Empty( lAuto )
         ::lAutoCreated := .T.
      ELSEIF ( :lFromIde .OR. :lCmdArgs )
         ::aParseInfo := ParseInfo( Self )
      ENDIF
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZPackage

   ::oDoc := NIL

   AEval( ::aScripts, {|v| v:End() } )
   AEval( ::aCSS, {|v| v:End() } )

   ::aScripts   := {}
   ::aCSS       := {}
   ::aDepends   := {}
   ::aParseInfo := {}

RETURN nil

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

METHOD AddScript( cText, cName, lUri, lTop ) CLASS ZPackage

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
   DEFAULT lTop  TO .F.

   IF !Empty( cName ) .AND. !lUri
      cName := Upper( cName )
      nAt := AScan( ::aScripts, {|v| Empty( v:cUrl ) .AND. Upper( v:cName ) == cName  } )
      IF nAt > 0
         ::aScripts[ nAt ]:AddCode( cText, lTop )
         RETURN ::aScripts[ nAt ]
      ENDIF
   ENDIF

   oScript := WScript():New( cText, cName, lUri )

   AAdd( ::aScripts, oScript )

RETURN oScript

//------------------------------------------------------------------------------

METHOD AddCSS( cText, cName, lUri ) CLASS ZPackage

   LOCAL oCss
   LOCAL nAt

   DEFAULT lUri TO !( hb_eol() $ cText ) .AND. !( "{" $ cText )

   IF lUri
      oCss := ::GetCssByUrl( cText )
      IF HB_IsObject( oCss )
         RETURN oCss
      ENDIF
   ELSEIF !Empty( cName )
      nAt := AScan( ::aCss, {|v| v:cName == cName } )
      IF nAt > 0
         oCss := ::aCSS[ nAt ]
         oCss:AddCode( cText )
         RETURN oCss
      ENDIF
   ENDIF

   oCss := WCss():New( cText, cName, lUri )

   AAdd( ::aCss, oCss )

RETURN oCss

//------------------------------------------------------------------------------

METHOD GetCssByUrl( cUrl ) CLASS ZPackage

   LOCAL oCss
   LOCAL nAt

   nAt := AScan( ::aCss, {|v| Lower( v:cUrl ) == Lower( cUrl ) } )

   IF nAt > 0
      oCss := ::aCSS[ nAt ]
   ENDIF

RETURN oCss

//------------------------------------------------------------------------------


