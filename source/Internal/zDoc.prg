/*
 * Proyect: XailerWeb framework
 * File: ZDoc.prg
 * Description: Base class for HTML documents
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

STATIC nIndent := 0

CLASS ZDoc

EXPORTED:
   DATA oRouter         INIT NIL AS CLASS WRouter
   DATA oContext        INIT NIL AS CLASS WContext
   DATA oBody
   DATA aPages          INIT {}
   DATA aScripts        INIT {}
   DATA aCSS            INIT {}
   DATA aMetas          INIT {}
   DATA aPackages       INIT {}
   DATA cName           INIT "" READONLY // lower Classname
   DATA cContent        INIT "text/html" VALUES "text/html", "application/json",;
                                                "application/x-www-form-urlencoded",;
                                                "multipart/form-data",;
                                                "text/plain"
   DATA cCharSet        INIT "UTF-8"
   DATA cLang           INIT "en"
   DATA cTitle          INIT "No Title"
   DATA cAction         INIT "" READONLY
   DATA cDescription    INIT ""
   DATA cStyleSheet     INIT ""
   DATA nStatus         INIT 200
   DATA lRendered       INIT .F. READONLY

   METHOD New( oRouter )      CONSTRUCTOR
   METHOD Create( oRouter )
   METHOD CreateDoc()         VIRTUAL
   METHOD Start( nOperType, cValue, hParam )
   METHOD Service( cMethod )
   METHOD Action( cMethod )

   METHOD AddScript( cText, cName )
   METHOD AddMeta( cType, cValue, cContent )
   METHOD AddCSS( cText, cName, lUri )
   METHOD AddCSSCode( cCode ) ;
         INLINE ::aCSS[ 1 ]:AddCode( cCode )
   METHOD AddStyle( cKey, xStyle ) ;
          INLINE ::aCSS[ 1 ]:AddStyle( cKey, xStyle )
   METHOD AddStyleById( cId, cKey, xStyle ) ;
          INLINE ::aCSS[ 1 ]:AddStyleById( cId, cKey, xStyle )

   METHOD ContextVar( cName )

   METHOD Render( xPage )
   METHOD SearchControl( cId )
   METHOD GetDefaultPage()
   METHOD AddPage( cName )
   METHOD GetPage( cName )
   METHOD RegisterPage( cPage )

   METHOD IsService()      INLINE Engine:IsService()
   METHOD IsServiceJS()    INLINE Engine:IsServiceJS()
   METHOD IsCSS( cUri )
   METHOD IsScript( cUri )
   METHOD IsPackage( cName )

   EVENT OnInitialize( oSender )
   EVENT OnStartHead( oSender, BYREF cHead )
   EVENT OnEndHead( oSender, BYREF cHead )
   EVENT OnStartBody( oSender, BYREF cBody )
   EVENT OnEndBody( oSender, BYREF cBody )

RESERVED:
   DATA aComponents  INIT {}
   DATA aServiceJs   INIT {}
   DATA aRegPages    INIT {}

   METHOD nIndent( nValue ) SETGET
   METHOD Body( cPage )
   METHOD Head()
   METHOD HtmlErrors()
   METHOD CheckId( oElement, cId )

   METHOD ServiceJs( oObj, cMsg, ... )
   METHOD SaveSession()
   METHOD LoadSession()

   METHOD wwwFileExists( cFile )

   METHOD InsertComponent( oComponent )

PROTECTED:
   DATA hID             INIT { => }
   DATA cHead           INIT ""
   DATA lCreated        INIT .F.

   METHOD GenJS( cMsg, ... )
   METHOD GenServiceJS()
   METHOD CheckPackages()
   METHOD PreProcess()        INLINE AEval( ::aPages, {|v| v:PreProcess() } )
   METHOD DataProperties()    INLINE __objGetProperties( Self, .F. )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oRouter ) CLASS ZDoc

   ::oRouter := oRouter
   ::cTitle  := Application:cTitle

   Document := Self

   ::oBody := sHtmlBody():New() // sin parent a aposta para evitar InsertControl()

   ::AddMeta( "name", "viewport", "width=device-width, initial-scale=1" )
   ::AddCSS( "" )

RETURN Self

//------------------------------------------------------------------------------

METHOD Create( oRouter ) CLASS ZDoc

   IF !HB_IsObject( Document )
      ::New( oRouter )
   ENDIF

   ::CreateDoc()
   ::OnInitialize()

RETURN Self

//------------------------------------------------------------------------------

METHOD InsertComponent( oComponent ) CLASS ZDoc

   AAdd( ::aComponents, oComponent )

RETURN Len( ::aComponents )

//------------------------------------------------------------------------------

METHOD Start( nOperType, cMsg, hParam ) CLASS ZDoc

   LOCAL oPage
   LOCAL xPage
   LOCAL cPage
   LOCAL lSrvJS

   IF ::lCreated
      ::Create()
   ENDIF

   Session() // sólo la instanaciamos y así se carga la sesión. Hace load automatico

   IF !Empty( cMsg )
      IF ::IsService()
         IF ( lSrvJS := ::IsServiceJS() )
            SetClassHook( {|O,d,v| ::ServiceJS( O, d, v ) } )
         ENDIF
         Engine:cRetService := __objSendMsg( Self, cMsg, hParam )
         IF lSrvJS
            SetClassHook( NIL )
         ENDIF
         RETURN NIL
      ELSE
         xPage := __objSendMsg( Self, cMsg, hParam )
      ENDIF
   ENDIF

   FOR EACH cPage IN ::aRegPages
      oPage := __objSendMsg( Self, cPage )
      IF !HB_IsObject( oPage )
         oPage := ::GetPage( cPage )
      ENDIF
      IF HB_IsObject( oPage )
         oPage:lForced := .T.
      ENDIF
   NEXT

RETURN ::Render( xPage )

//------------------------------------------------------------------------------

METHOD GetDefaultPage() CLASS ZDoc

   LOCAL oPage

   IF Len( ::aPages ) == 0
      oPage := WDocPage():New( Self, "default" )
      AAdd( ::aPages, oPage )
   ELSE
      oPage := ::GetPage()
      IF oPage == NIL
         oPage := ::aPages[ 1 ]
      ENDIF
   ENDIF

RETURN oPage

//------------------------------------------------------------------------------

METHOD GetPage( cName ) CLASS ZDoc

   LOCAL oPage
   LOCAL nAt

   DEFAULT cName TO "default"

   cName := Lower( cName )

   IF ( nAt := AScan( ::aPages, {|v| Lower( v:cName ) == cName } ) ) > 0
      oPage := ::aPages[ nAt ]
   ENDIF

RETURN oPage

//------------------------------------------------------------------------------

METHOD RegisterPage( cPage ) CLASS ZDoc

   IF __objHasMethod( Self, cPage )
      AAdd( ::aRegPages, cPage )
   ELSE
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'Invalid Page "' + cPage +'"'
         :Operation   := "WDoc:RegisterPage( cPage )"
         Eval( ErrorBlock(), :__WithObject(), 3 )
         RETURN .F.
      END WITH
   ENDIF

RETURN .T.

//------------------------------------------------------------------------------

METHOD AddPage( cName ) CLASS ZDoc

   LOCAL oPage

   IF Empty( cName )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'WDocPage must have a valid name'
         :Operation   := "WDoc:AddPage()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
   ENDIF

   oPage := ::GetPage( cName )

   IF oPage != NIL
      RETURN oPage
   ENDIF

   oPage := WDocPage():New( Self, cName )
   AAdd( ::aPages, oPage )

RETURN oPage

//------------------------------------------------------------------------------

METHOD Service( cMethod ) CLASS ZDoc

   IF !__objHasMethod( Self, cMethod )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Invalid Method "' + cMethod +'"'
         :Operation   := "WDoc:Service()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN "Error"
   ENDIF

RETURN Engine:BaseUri() + "?service=" + Lower( ::Classname ) + "-" + cMethod

//------------------------------------------------------------------------------

METHOD Action( cMethod ) CLASS ZDoc

   IF !__objHasMethod( Self, cMethod )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Invalid Method "' + cMethod +'"'
         :Operation   := "WDoc:Service()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN "error"
   ENDIF

RETURN Engine:BaseUri() + "?action=" + Lower( ::Classname ) + "-" + cMethod

//------------------------------------------------------------------------------

METHOD AddScript( cText, cName ) CLASS ZDoc

   LOCAL oScript
   LOCAL nAt
   LOCAL lUri

   IF Empty( cText )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Parameter "cText" empty. Script not included'
         :Operation   := "WDoc:AddScript()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
   ENDIF

   lUri := !( hb_eol() $ cText ) .AND. !( "FUNCTION" $ Upper( cText ) )

   IF !Empty( cName ) .AND. !lUri
      nAt := AScan( ::aScripts, {|v| Upper( v:cName ) == Upper( cName ) } )
      IF nAt > 0
         ::aScripts[ nAt ]:AddCode( cText )
         RETURN ::aScripts[ nAt ]
      ENDIF
   ENDIF

   oScript := WScript():New( cText, cName, lUri )

   AAdd( ::aScripts, oScript )

RETURN oScript

//------------------------------------------------------------------------------

METHOD AddCSS( cText, cName, lUri ) CLASS ZDoc

   LOCAL oCss

   oCss := WCss():New( cText, cName, lUri )

   AAdd( ::aCss, oCss )

RETURN oCss

//------------------------------------------------------------------------------

METHOD AddMeta( cType, cValue, cContent ) CLASS ZDoc

   LOCAL oMeta

   FOR EACH oMeta IN ::aMetas
      IF oMeta:cType == cType .AND. oMeta:cValue == cValue
         oMeta:cContent := cContent
         RETURN oMeta
      ELSEIF oMeta:cType == cType .AND. Empty( oMeta:cContent ) // charset
         oMeta:cValue := cValue
         RETURN oMeta
      ENDIF
   NEXT

   oMeta := WMeta():New( cType, cValue, cContent )

   AAdd( ::aMetas, oMeta )

RETURN oMeta

//------------------------------------------------------------------------------

METHOD ContextVar( cName ) CLASS ZDoc

   LOCAL cValue

   IF HB_IsObject( ::oContext ) .AND. __objHasMethod( ::oContext, cName )
      cValue := __objSendMsg( ::oContext, cName )
   ELSE
      cValue := ""
   ENDIF

RETURN cValue

//------------------------------------------------------------------------------

METHOD Render( xPage ) CLASS ZDoc

   LOCAL cRet

   ::cName := Lower( ::Classname )

   IF ::IsService()
      WITH OBJECT Engine
         IF ::IsServiceJS()
            cRet := ::GenServiceJS()
         ELSE
            cRet := :cRetService
         ENDIF
         IF HB_IsString( cRet )
            :AddHeader( "Content-Length", ToString( Len( cRet ) ) )
         ENDIF
         :Header( ::nStatus )
         :Send( cRet )
      END WITH
   ELSE
      ::SaveSession()
      ::CheckPackages()
      AEval( ::aPackages, {|v| v:Render() } ) // Before preprocess, so its Css can be overloaded
      ::PreProcess()

      WITH OBJECT Engine
         :Header( ::nStatus )
         :Send( "<!DOCTYPE html>" + hb_eol() + hb_eol() + '<html lang="' + ::cLang + '">' + hb_eol() + hb_eol() )
         :Send( ::Head() )
         :Send( ::Body( xPage ) )
         :Send( "</html>" + hb_eol() )
      END WITH
   ENDIF

   ::lRendered := .T.

RETURN nil

//------------------------------------------------------------------------------

METHOD Head() CLASS ZDoc

   LOCAL oMeta, oScript, oCss
   LOCAL cPath, cFile, cCss, cHtml := "<head>" + hb_eol()
   LOCAL nFor, nAt

   ::OnStartHead( @cHtml )

   ::nIndent := 1

   cHtml += HTML_SPACES + '<title>' + ::cTitle + '</title>' + hb_eol()

   ::AddMeta( "http-equiv", "Content-type", ::cContent + ";charset=" + ::cCharSet )

   IF !Empty( ::cDescription )
      ::AddMeta( "name", "description", ::cDescription )
   ENDIF

   FOR EACH oMeta IN ::aMetas
      cHtml += oMeta:Html() + hb_eol()
   NEXT

   // Css render

   FOR EACH oCss IN ::aCSS
      IF oCss:IsUrl()
         cHtml += oCss:HtmlUrl()
      ENDIF
   NEXT

   cCss := ""

   FOR nFor := 2 TO Len( ::aCSS )
      oCss := ::aCSS[ nFor ]
      IF oCss:IsCode()
         cCss += oCss:HtmlCode()
      ENDIF
   NEXT

   IF ::aCSS[ 1 ]:IsCode()
      cCss += ::aCSS[ 1 ]:HtmlCode()
   ENDIF

   IF !Empty( cCss )
       cHtml += HTML_SPACES + '<style>' + hb_eol() + cCss + ;
                HTML_SPACES + '</style>' + hb_eol()
   ENDIF

   FOR EACH oScript IN ::aScripts
      IF !oScript:lBottom
         cHtml += oScript:Html() + hb_eol()
      ENDIF
   NEXT

   ::nIndent := 0

   cHtml += "</head>" + hb_eol()

   ::OnEndHead( @cHtml )

   Engine:lDocHeadDone := .T.

RETURN cHtml

//------------------------------------------------------------------------------

METHOD Body( xPage ) CLASS ZDoc

   LOCAL oScript, oPage, oIter
   LOCAL cJs, cHtml := ::oBody:Html()
   LOCAL nPage := 0

   SWITCH ValType( xPage )
   CASE "C"
      oPage := ::GetPage( xPage )
      EXIT
   CASE "N"
      IF xPage > 0 .AND. xPage <= Len( ::aPages )
         oPage := ::aPages[ xPage ]
      ENDIF
      EXIT
   CASE "O"
      IF xPage:IsKindOf( "WDocPage" )
         oPage := xPage
      ENDIF
      EXIT
   END SWITCH

   IF Len( ::aPages ) == 0
      RETURN "<body><h1>XailerWeb error:</h1><p><h2>No WDocPages in the document. Imposible to continue.</h2></p></body>" + hb_eol()
   ENDIF

   IF oPage == NIL
      oPage := ::GetDefaultPage()
   ENDIF

   ::nIndent := 1

   ::OnStartBody( @cHtml )

   FOR EACH oIter IN ::aPages
      WITH OBJECT oIter
         IF :lForced .AND. !:lFooter
            cHtml += :RunHtml()
         ENDIF
      END WITH
   NEXT

   IF !oPage:lForced
      cHtml += oPage:RunHtml()
   ENDIF

   FOR EACH oIter IN ::aPages
      WITH OBJECT oIter
         IF :lForced .AND. :lFooter
            cHtml += :RunHtml()
         ENDIF
      END WITH
   NEXT

   ::nIndent := 0

   FILE "..\www\xw_backpack.js" INTO cJs

   // Por si se edita en Windows y se ejecuta en Linux

   IF !( hb_eol() == ( Chr( 13 ) + Chr( 10 ) ) )
      cJs := StrTran( cJs, Chr( 13 ) + Chr( 10 ), hb_eol() )
   ENDIF

   oScript := WScript():New( cJs, "xw_backpack", .F. )

   HB_AIns( ::aScripts, 1, oScript, .T. )

   // Js variable (the last JS to execute)

   ::AddScript( ::GenJS(), "init" )

   ::nIndent := 1

   FOR EACH oScript IN ::aScripts
      WITH OBJECT oScript
         IF :lBottom .AND. !:lDefer
            cHtml += :Html()
         ENDIF
      END WITH
   NEXT

   FOR EACH oScript IN ::aScripts
      WITH OBJECT oScript
         IF :lBottom .AND. :lDefer
            cHtml += :Html()
         ENDIF
      END WITH
   NEXT

   ::nIndent := 0

   cHtml += ::HtmlErrors()

   cHtml += "</body>" + hb_eol()

   ::OnEndBody( @cHtml )

RETURN cHtml

//------------------------------------------------------------------------------

METHOD nIndent( nValue ) CLASS ZDoc

   IF HB_IsNumeric( nValue )
      nIndent := nValue
   ENDIF

RETURN nIndent

//------------------------------------------------------------------------------

METHOD HtmlErrors() CLASS ZDoc

   LOCAL cHtml := ""

   IF !Empty( Engine:cLogHtml )
      TEXT INTO cHtml TABS 2
         <iframe srcdoc='{ERROR}'
            style="background-color:yellow; position:fixed; left:0; bottom:0; border:none;"height="150px" width="100%" scrolling="yes" title="Error log">
         </iframe>
      ENDTEXT

      cHtml := StrTran( cHtml, "{ERROR}", Engine:cLogHtml )
      cHtml := StrTran( cHtml, "{TAB}", HTML_SPACES )
      Engine:LogClear()
   ENDIF

RETURN cHtml

//------------------------------------------------------------------------------

METHOD GenJS() CLASS ZDoc

   LOCAL oPage, oCtl
   LOCAL aData, aEvent, aValue
   LOCAL cText

   cText  := ""
   aData  := {}
   aEvent := {}

   ::oBody:GenJs( aData, aEvent )

   FOR EACH oPage IN ::aPages
      FOR EACH oCtl IN oPage:aControls
         oCtl:GenJs( aData, aEvent )
      NEXT
   NEXT

   cText += 'const xw_docName  = "' + Document:cName + '";' + hb_eol()
   cText += "var xw_mapProp  = new Map(); " + hb_eol()
   cText += "var xw_mapEvent = new Map(); " + hb_eol()
   cText += "var xw_mapPost  = new Map(); " + hb_eol()

   FOR EACH aValue IN aData
      cText += "xw_mapProp.set('" + aValue[ 1 ] + "', {" + aValue[ 2 ] + '});' + hb_eol()
   NEXT

   FOR EACH aValue IN aEvent
      cText += "xw_mapEvent.set('" + aValue[ 1 ] + "', {" + aValue[ 2 ] + '});' + hb_eol()
   NEXT

   cText += 'xw_deleteCookie("event");' + hb_eol()
   cText += 'xw_deleteCookie("state");' + hb_eol()

   cText += 'for (const [key, obj] of xw_mapEvent) {' + hb_eol()
   cText += '  for(const[eve, val] of Object.entries(obj)) {' + hb_eol()
   cText += '    const ele = document.getElementById(key);' + hb_eol()
   cText += '    if (ele) {' + hb_eol()
   cText += '      xw_processEvent(ele, eve, val);' + hb_eol()
   cText += '    }' + hb_eol()
   cText += "  }" + hb_eol()
   cText += "}" + hb_eol()

RETURN cText

//------------------------------------------------------------------------------

METHOD SaveSession() CLASS ZDoc

   LOCAL aVal, hHash := { => }

   FOR EACH aVal IN ::DataProperties()
      HB_HSet( hHash, aVal[ 1 ], aVal[ 2 ] )
   NEXT

   IF Len( hHash ) > 0
      IF !Session():Save( hHash )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := "Session could not be saved"
            :Operation   := "Session():Save( hHash )"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD LoadSession() CLASS ZDoc

   LOCAL hHash
   LOCAL cKey, xVal

   hHash := Session():Load()

   IF HB_IsHash( hHash )
      FOR EACH cKey, xVal IN HB_HKeys( hHash ), HB_HValues( hHash )
         IF __objHasMsg( Self, cKey )
            __objSendMsg( Self, "_" + cKey, xVal )
         ENDIF
      NEXT
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD CheckId( oElement, cId ) CLASS ZDoc

   IF HB_HHasKey( ::hID, cId )
      IF !( HB_HGet( ::hID, cId ) == oElement )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := "Duplicated ID: '" + cId + "'."
            :Operation   := "WBasic:cId set value"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
   ELSE
      HB_HSet( ::hid, cId, oElement )
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD SearchControl( cId ) CLASS ZDoc

   IF HB_HHasKey( ::hID, cId )
      RETURN HB_HGet( ::hID, cId )
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD ServiceJs( oObj, cMsg, ... ) CLASS ZDoc

   AAdd( ::aServiceJs, { oObj, cMsg, ... } )

RETURN nil

//------------------------------------------------------------------------------

METHOD GenServiceJs() CLASS ZDoc

   LOCAL aRow
   LOCAL obj
   LOCAL cJs, cId, cMb, cVa

   cJs := ""

   FOR EACH aRow IN ::aServiceJs
      obj := aRow[ 1 ]
      IF __objHasData( obj, "cId" )
         cId := __objSendMsg( obj, "cId" )
         IF !Empty( cId )
            cMb := MemberToJs( aRow[ 2 ] )
            cVa := ParamsToJs( cMb, aRow[ 3 ] )
            cJs += 'xw_temp=document.getElementById("' + cId + '");'
            cJs += 'if (xw_temp) xw_temp["' + cMb + '"]=' + cVa + ';'
            cJs += 'delete xw_temp;'
         ENDIF
      ENDIF
   NEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD CheckPackages() CLASS ZDoc

   LOCAL oPack
   LOCAL aNames  := {}
   LOCAL cDepend

//   IF ::oContext == NIL
//      WBasicContext():New( Self )
//   ENDIF

   FOR EACH oPack IN ::aPackages
      AAdd( aNames, oPack:cName )
      FOR EACH cDepend IN oPack:aDepends
         IF AScan( aNames, cDepend ) == 0
            WITH OBJECT ErrorNew()
               :Subsystem   := ERROR_SUBSYSTEM
               :Severity    := ES_ERROR
               :Description := 'Dependent package: ' + cDepend + ' should be added before ' + oPack:cName
               :Operation   := "WPackage:AddDependency()"
               Eval( ErrorBlock(), :__WithObject(), 3 )
            END WITH
         ENDIF
      NEXT
   NEXT

RETURN nil

//------------------------------------------------------------------------------

METHOD IsCSS( cUri ) CLASS ZDoc

RETURN ( AScan( ::aCSS, {|v| Lower( v:cUrl ) == Lower( cUri ) } ) > 0 )

//------------------------------------------------------------------------------

METHOD IsScript( cUri ) CLASS ZDoc

RETURN ( AScan( ::aScriptS, {|v| Lower( v:cUrl ) == Lower( cUri ) } ) > 0 )

//------------------------------------------------------------------------------

METHOD IsPackage( cName ) CLASS ZDoc

RETURN ( AScan( ::aPackages, {|v| v:cName == Lower( cName ) } ) > 0 )

//------------------------------------------------------------------------------

METHOD wwwFileExists( cFile )  CLASS ZDoc

   LOCAL cPath

   cPath := Engine:DocumentRoot()

   IF Left( cFile, 1 ) == hb_OsPathSeparator()
      cFile := SubStr( cFile, 2 )
   ENDIF

RETURN HB_FileExists( cPath + cFile )

//------------------------------------------------------------------------------

STATIC FUNCTION MemberToJs( cValue )

   cValue := Lower( SubStr( cValue , 2 ) )

   cValue := "innerHTML" // TOFIX

RETURN cValue

//------------------------------------------------------------------------------

STATIC FUNCTION ParamsToJs( cMember, xValue )

   LOCAL cValue

   cValue := '"' + xValue + '"' // TOFIX

RETURN cValue

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS sHtmlBody FROM WBasic STATIC
RESERVED:
   METHOD Html()

PROTECTED:
   DATA cTag   INIT "body"
   DATA cId    INIT ""

END CLASS

//------------------------------------------------------------------------------

METHOD Html() CLASS sHtmlBody

   LOCAL cHtml := HTML_SPACES

   IF ::AreJsEvents()
      ::cId := "_body_"
   ENDIF

   cHtml += '<' + ::cTag + ::Super:HtmlTagBody() + '>' + hb_eol()

RETURN cHtml

//------------------------------------------------------------------------------
