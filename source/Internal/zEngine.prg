/*
 * Proyect: XailerWeb framework
 * File: zHttpEngine.prg
 * Description: CGI Engine
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "fileio.ch"
#include "error.ch"

STATIC aEnv :=  { "AUTH_TYPE",;
                  "ANNOTATION_SERVER",;
                  "CONTENT_TYPE",;
                  "CONTENT_LENGTH",;
                  "DOCUMENT_ROOT",;
                  "GATEWAY_INTERFACE",;
                  "HTTP_ACCEPT",;
                  "HTTP_COOKIE",;
                  "HTTP_USER_AGENT",;
                  "HTTP_REFERER",;
                  "HTTPS",;
                  "PATH_INFO",;
                  "PATH_TRANSLATED",;
                  "QUERY_STRING",;
                  "REQUEST_METHOD",;
                  "REQUEST_URI",;
                  "REMOTE_HOST",;
                  "REMOTE_ADDR",;
                  "REMOTE_USER",;
                  "SERVER_SOFTWARE",;
                  "SERVER_NAME",;
                  "SERVER_PROTOCOL",;
                  "SERVER_PORT",;
                  "SCRIPT_NAME" }

STATIC nMilliSeconds

CLASS ZEngine

EXPORTED:
   DATA cLogFile        INIT "error.log"

   DATA aResCookies     INIT {}     // to response
   DATA hReqCookies     INIT { => } // received
   DATA hParams         INIT { => } // Get parameters
   DATA hState          INIT { => } // properties PERSISTENT
   DATA hEvent          INIT { => } // click event en Web page
   DATA hPost           INIT { => } // Post data
   DATA hCargo          INIT { => } // Cargo data
   DATA hResHeaders     INIT { => }

   DATA cPostBuffer     INIT "" READONLY
   DATA cFirstQuery     INIT "" READONLY // value for ('action', 'form', 'service')

   DATA nOperType       INIT OPER_DEFAULT VALUES OPER_DEFAULT, OPER_ACTION, ;
                                                 OPER_FORM, OPER_SERVICE,;
                                                 OPER_SERVICEJS

   DATA nSessionTTL     INIT 3600 // seconds
   DATA cRetService     INIT ""

   METHOD New()         CONSTRUCTOR
   METHOD End()         INLINE ::Render()

   METHOD LogForDebug( ... )
   METHOD JsConsole( cText )

   METHOD Cookies()        INLINE HB_HGetDef(::hEnvironment, "HTTP_COOKIE", "" )
   METHOD ContentType()    INLINE HB_HGetDef(::hEnvironment, "CONTENT_TYPE", "" )
   METHOD ContentLength()  INLINE HB_HGetDef(::hEnvironment, "CONTENT_LENGTH", "" )
   METHOD DocumentRoot()   INLINE HB_HGetDef(::hEnvironment, "DOCUMENT_ROOT", "" ) + hb_OsPathSeparator()
   METHOD QueryString()    INLINE HB_HGetDef(::hEnvironment, "QUERY_STRING", "" )
   METHOD RequestMethod()  INLINE HB_HGetDef(::hEnvironment, "REQUEST_METHOD", "" )
   METHOD BaseUri()        INLINE HB_TokenGet( ::RequestUri(), , "?" )
   METHOD RequestUri()     INLINE HB_HGetDef(::hEnvironment, "REQUEST_URI", "" )
   METHOD PathInfo()       INLINE HB_HGetDef(::hEnvironment, "PATH_INFO", "" )
   METHOD RemoteAddr()     INLINE HB_HGetDef(::hEnvironment, "REMOTE_ADDR", "" )
   METHOD ScriptName()     INLINE HB_HGetDef(::hEnvironment, "SCRIPT_NAME", "" )
   METHOD IsSecure()       INLINE ( HB_HGetDef(::hEnvironment, "HTTPS", "off" ) == "on" )

   METHOD Cookie( cName )  INLINE IIF( HB_HHasKey( ::hReqCookies, cName ),;
                                       HB_HGet( ::hReqCookies, cName ), "" )

   METHOD LogFile()

   METHOD IsPost()      INLINE !Empty( ::cPostBuffer )
   METHOD IsGet()       INLINE ( Len( ::hParams ) > 0 )

   METHOD Send( cBuffer, lLog )           // --> lSuccess
   METHOD Header( nStatus )               // --> lSuccess
   METHOD Render( lError )                // --> lSuccess
   METHOD AddHeader( cName, cValue )      // --> lSuccess
   METHOD AddResCookie( cName, cValue )   // --> oCookie

   METHOD lDebug( lValue ) SETGET
   METHOD cContent( cValue ) SETGET

   METHOD SessionId( cId )

   METHOD IsWebPage()   INLINE ( ::nOperType == OPER_DEFAULT )
   METHOD IsService()   INLINE ( ::nOperType == OPER_SERVICE .OR. ;
                                 ::nOperType == OPER_SERVICEJS )
   METHOD IsServiceJS() INLINE ( ::nOperType == OPER_SERVICEJS )
   METHOD IsInputForm() INLINE ( ::nOperType == OPER_FORM )
   METHOD IsAction()    INLINE ( ::nOperType == OPER_ACTION )

RESERVED:
   DATA hEnvironment    INIT { => }
   DATA cLogText        INIT ""  READONLY
   DATA cLogHtml        INIT ""  READONLY
   DATA hDebug          INIT -1
   DATA lHeaderDone     INIT .F.
   DATA lDocHeadDone    INIT .F.

   METHOD cStatus( nStatus )
   METHOD LogEnv()
   METHOD LogText( ... )
   METHOD LogClear()    INLINE ::cLogText := "", ::cLogHtml := ""

HIDDEN:
   DATA FlDebug         INIT .F.

ENDCLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZEngine

   LOCAL aTokens
   LOCAL hHash
   LOCAL cBuffer, cToken, cVar, cVal, cJson, cId
   LOCAL nLen, nRead, nAt
   LOCAL lType

   nMilliSeconds := hb_MilliSeconds()

   HB_HCaseMatch( ::hEnvironment, .F. )
   HB_HAutoAdd( ::hEnvironment, .T. )

   FOR EACH cToken IN aEnv
      HB_HSet( ::hEnvironment, cToken, GetEnv( cToken ) )
   NEXT

   cBuffer := ::QueryString()
   aTokens := HB_ATokens( cBuffer, "&" )
   lType   := .F.

   HB_HCaseMatch( ::hParams, .F. )

   FOR EACH cToken IN aTokens
      IF !Empty( cToken )
         cVar := Lower( AllTrim( HB_TokenGet( cToken, 1, "=" ) ) )
         cVal := XA_urlDecode( AllTrim( HB_TokenGet( cToken, 2, "=" ) ) )
         IF !Empty( cVar ) .AND. !HB_HHasKey( ::hParams, cVar )
            HB_HSet( ::hParams, cVar, cVal )
         ENDIF
         IF !lType
            SWITCH cVar
            CASE "service"
               IF ( ::ContentType() == "application/javascript" )
                  ::nOperType := OPER_SERVICEJS
               ELSE
                  ::nOperType := OPER_SERVICE
               ENDIF
               EXIT
            CASE "form"
               ::nOperType := OPER_FORM
               EXIT
            CASE "action"
               ::nOperType := OPER_ACTION
               EXIT
            OTHERWISE
               ::nOperType := OPER_DEFAULT
            END SWITCH
            lType := .t.
            ::cFirstQuery := cVal
         ENDIF
      ENDIF
   NEXT

   IF ::RequestMethod() = "POST"
      nLen := Val( ::ContentLength() )
      IF nLen > 0
         cBuffer       := Space( nLen )
         nRead         := FRead( STDIN, @cBuffer, nLen )
         cBuffer       := Left( cBuffer, nRead )
         ::cPostBuffer := cBuffer
         HB_HCaseMatch( ::hPost, .F. )
         FOR EACH cToken IN HB_ATokens( cBuffer, "&" )
            cVar := XA_urlDecode( AllTrim( HB_TokenGet( cToken, 1, "=" ) ) )
            cVal := XA_urlDecode( AllTrim( HB_TokenGet( cToken, 2, "=" ) ) )
            IF !Empty( cVar )
               HB_HSet( ::hPost, cVar, cVal )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   cBuffer := ::Cookies()

   aTokens := HB_ATokens( cBuffer, ";" )

   HB_HCaseMatch( ::hReqCookies, .F. )

   FOR EACH cToken IN aTokens
      IF !Empty( cToken )
         cVar := AllTrim( HB_TokenGet( cToken, 1, "=" ) )
         cVal := XA_urlDecode( AllTrim( HB_TokenGet( cToken, 2, "=" ) ) )
         IF !Empty( cVar ) .AND. !HB_HHasKey( ::hReqCookies, cVar )
            HB_HSet( ::hReqCookies, cVar, cVal )
         ENDIF
      ENDIF
   NEXT

   HB_HCaseMatch( ::hResHeaders, .F. )
   HB_HAutoAdd( ::hResHeaders, .T. )

   ::AddHeader( "Content-type", "text/html" )

   HB_HCaseMatch( ::hState, .F. )
   cJson := HB_HGetDef( ::hReqCookies, "state", "{}" )
   HB_JsonDecode( cJson, @hHash )
   ::hState := hHash

   HB_HCaseMatch( ::hEvent, .F. )
   cJson := HB_HGetDef( ::hReqCookies, "event", "{}" )
   HB_JsonDecode( cJson, @hHash )
   ::hEvent := hHash

   HB_HCaseMatch( ::hCargo, .F. )
   cJson := HB_HGetDef( ::hReqCookies, "cargo", "{}" )
   HB_JsonDecode( cJson, @hHash )
   ::hCargo := hHash

   // Si se ha usado xw_setHbData() modo POST se incluye en State

   FOR EACH cVar, cVal IN HB_HKeys( ::hPost ), HB_HValues( ::hPost )
      nAt := At( "@", cVar )
      IF nAt > 0
         cId  := Left( cVar, nAt - 1 )
         cVar := SubStr( cVar, nAt + 1)
         IF !HB_HHasKey( ::hState, cId )
            HB_HSet( ::hState, HB_Hash( cVar, cVal ) )
         ELSE
            hHash := HB_HGet( ::hState, cId )
            HB_HSet( hHash, cVar, cVal )
         ENDIF
      ENDIF
   NEXT

RETURN Self

//------------------------------------------------------------------------------

METHOD LogFile() CLASS ZEngine

   LOCAL cFile := ::cLogFile

   IF !( HB_OsPathSeparator() $ cFile )
      cFile := ::DocumentRoot() + cFile
   ENDIF

RETURN cFile

//------------------------------------------------------------------------------

METHOD Send( cBuffer, lLog ) CLASS ZEngine

   DEFAULT lLog TO .T.

   IF Len( cBuffer ) == 0
      RETURN .F.
   ENDIF

   IF lLog .AND. ::hDebug >= 0
      FWrite( ::hDebug, cBuffer )
   ENDIF

RETURN ( FWrite( STDOUT, cBuffer ) > 0 )

//------------------------------------------------------------------------------

METHOD Header( nStatus ) CLASS ZEngine

   LOCAL oCookie
   LOCAL cBuffer, cKey, cValue

   IF ::lHeaderDone
      RETURN NIL
   ENDIF

   cBuffer := "status: " + ToString( nStatus ) + hb_eol()

   FOR EACH cKey, cValue IN HB_HKeys( ::hResHeaders ), HB_HValues( ::hResHeaders )
      cBuffer += cKey + ": " + cValue + hb_eol()
   NEXT

   IF !::IsService()
      WITH OBJECT ::AddResCookie( "session", ::SessionId()  )
         :nMaxAge := ::nSessionTTL
      END WITH

      FOR EACH oCookie IN ::aResCookies
         cBuffer += oCookie:Render() + hb_eol()
      NEXT

      IF ::hDebug >= 0
         IF !Empty( ::QueryString() )
            FWrite( ::hDebug, "dbg> Query string: " + ::QueryString() + hb_eol() )
         ENDIF
         FOR EACH cKey, cValue IN HB_HKeys( ::hParams ), HB_HValues( ::hParams )
            Fwrite( ::hDebug, "dbg> Get-Param " + cKey + ":" + cValue + hb_eol() )
         NEXT
         FOR EACH cKey, cValue IN HB_HKeys( ::hReqCookies ), HB_HValues( ::hReqCookies )
            Fwrite( ::hDebug, "dbg> Get-Cookie " + cKey + ":" + cValue + hb_eol() )
         NEXT
         IF !Empty( ::cPostBuffer )
            Fwrite( ::hDebug, "dbg> Get-Postdata: " + ::cPostBuffer + hb_eol() )
         ENDIF
         FWrite( ::hDebug, Replicate( "-", 80 ) + hb_eol() )
         FWrite( ::hDebug, cBuffer + Replicate( "-", 80 ) + hb_eol() )
      ENDIF
   ENDIF

   cBuffer += hb_eol()   // empty line to separate http header

   ::lHeaderDone := .T.

RETURN ( FWrite( STDOUT, cBuffer ) > 0 )

//------------------------------------------------------------------------------

METHOD AddHeader( cName, cValue ) CLASS ZEngine

   HB_HSet( ::hResHeaders, cName, cValue )

RETURN NIL

//------------------------------------------------------------------------------

METHOD AddResCookie( cName, cValue ) CLASS ZEngine

   LOCAL oCookie

   //oCookie := WCookie():New( cName, cValue, ::IsSecure() )
   oCookie := WCookie():New( cName, cValue, .F. )
   oCookie:cSameSite := "strict"

   AAdd( ::aResCookies, oCookie )

RETURN oCookie

//------------------------------------------------------------------------------

METHOD Render( lError ) CLASS ZEngine

   STATIC lDone := .F.

   DEFAULT lError TO .F.

   IF lDone
      RETURN nil
   ENDIF

   IF ::lDebug
      ::AddHeader( "Cache-Control", "no-cache" )
   ENDIF

   IF lError
      IF !::IsService()
         IF !::lHeaderDone
            ::Header( 422 )
         ENDIF
         IF !::lDocHeadDone
            ::Send( "<!DOCTYPE html>" + hb_eol() + '<html lang="en">' + hb_eol() )
            ::Send( "<head></head>" + hb_eol() )
         ENDIF
         ::Send( "<body>" + hb_eol() )
         ::Send( "<h1>XailerWeb error</h1>" + hb_eol() )
      ELSE
         ::AddHeader( "Content-Length", ToString( Len( ::cLogText ) ) )
         ::Header( 206 )
         ::Send( ::cLogText )
      ENDIF
   ELSEIF !HB_IsObject( Document )
      ::Header( 206 )
      ::Send( "<!DOCTYPE html>" + hb_eol() + '<html lang="en">' + hb_eol() )
      ::Send( "<head></head>" + hb_eol() )
      ::Send( "<body><h1>Invalid WDoc object. Imposible to continue.</h1></body>" + hb_eol() )
      ::Send( "</html>" + hb_eol() )
   ELSEIF !Document:lRendered
      Document:Render()
   ENDIF

   IF !Empty( ::cLogText )
      IF !::lDebug
         ::lDebug := .T.
      ENDIF
      FWrite( ::hDebug, ::cLogText )
      IF lError .AND. !::IsService()
         ::Send( ::cLogHtml + hb_eol() + "</body>" + hb_eol() + "</html>" + hb_eol() )
      ENDIF
   ENDIF

   IF ::lDebug // Close file
         FWrite( ::hDebug, hb_eol() + "Execute time in milliseconds: " + Str( HB_MilliSeconds()- nMilliSeconds) )
      ::lDebug := .F.
   ENDIF

   lDone := .T.

RETURN NIL

//------------------------------------------------------------------------------

METHOD lDebug( lValue ) CLASS ZEngine

   LOCAL cFile

   IF PCount() > 0
      ::FlDebug := lValue
   ELSE
      RETURN ::FlDebug
   ENDIF

   IF ::FlDebug
      cFile := ::LogFile()
      IF File( cFile )
         FErase( cFile )
      ENDIF
      ::hDebug := FCreate( cFile,  FC_NORMAL ) // TOFIX: Linux
   ELSEIF ::hDebug >= 0
      FClose(::hDebug )
      ::hDebug := -1
   ENDIF

RETURN ::FlDebug

//------------------------------------------------------------------------------

METHOD cContent( cValue ) CLASS ZEngine

   IF PCount() > 0
      ::AddHeader( "Content-type", cValue )
   ENDIF

RETURN HB_HGetDef( ::hResHeaders, "Content-type", "" )

//------------------------------------------------------------------------------

METHOD LogText( ... )  CLASS ZEngine

   LOCAL xValue, cText := ""

   FOR EACH xValue IN hb_aParams()
      GenLog( xValue, @cText )
      IF !xValue:__enumIsLast()
         cText += ", "
      ENDIF
   NEXT

   IF ::hDebug >= 0
      FWrite( Engine:hDebug, "LOGDEBUG >>" + cText + "<<" + hb_eol() )
   ENDIF

   ::cLogText += cText

   cText := StrTran( cText, "'", '"' )
   cText := StrTran( cText, "<", "&lt;" )
   cText := StrTran( cText, ">", "&gt;" )
   cText := StrTran( cText, "'", '"' )
   cText := StrTran( cText, TAB, '&ensp;' )
   cText := StrTran( cText, hb_eol(), '<br>' )

   ::cLogHtml += cText + "<br>"

RETURN cText

//------------------------------------------------------------------------------

METHOD LogForDebug( ... )  CLASS ZEngine

   LOCAL cText

   cText := ::LogText( hb_aParams() )

   IF ::IsService()
      ::cLogText := ""
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'Can not be used on services ' + cText
         :Operation   := "Logdebug()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN ""
   ENDIF

RETURN cText

//------------------------------------------------------------------------------

METHOD JsConsole( ... ) CLASS ZEngine

   LOCAL xValue, cText

   FOR EACH xValue IN hb_aParams()
      cText := ToString( xValue )
      cText := StrTran( cText, "<br>", hb_eol() )
      cText := StrTran( cText, "<p>", "" )
      cText := StrTran( cText, "</p>", "" )
      cText := StrTran( cText, Space( TAB_LENGTH ), "" )
      cText := StrTran( cText, "&lt;", "<" )
      cText := StrTran( cText, "&gt;", ">" )
      ::Send( HTML_SPACES + '<script>console.log("' + cText + '")</script>' + hb_eol(), .F. )
   NEXT

RETURN nil

//------------------------------------------------------------------------------

METHOD cStatus( nStatus ) CLASS ZEngine

   STATIC aStatus := {{100,"Continue"},;
                      {101,"Switching Protocols"},;
                      {200,"OK"},;
                      {201,"Created"},;
                      {202,"Accepted"},;
                      {203,"Non-Authoritative Information"},;
                      {204,"No Content"},;
                      {205,"Reset Content"},;
                      {206,"Partial Content"},;
                      {300,"Multiple Choices"},;
                      {301,"Moved Permanently"},;
                      {302,"Found"},;
                      {303,"See Other"},;
                      {304,"Not Modifie"},;
                      {305,"Use Proxy"},;
                      {306,"(Unused)"},;
                      {307,"Temporary Redirect"},;
                      {308,"Permanent Redirect"},;
                      {400,"Bad Request"},;
                      {401,"Unauthorized"},;
                      {402,"Payment Required"},;
                      {403,"Forbidden"},;
                      {404,"Not Found"},;
                      {405,"Method Not Allowed"},;
                      {406,"Not Acceptable"},;
                      {407,"Proxy Authentication Required"},;
                      {408,"Request Timeout"},;
                      {409,"Conflict"},;
                      {410,"Gone"},;
                      {411,"Length Required"},;
                      {412,"Precondition Failed"},;
                      {413,"Content Too Large"},;
                      {414,"URI Too Long"},;
                      {415,"Unsupported Media Type"},;
                      {416,"Range Not Satisfiable"},;
                      {417,"Expectation Failed"},;
                      {418,"(Unused)"},;
                      {421,"Misdirected Request"},;
                      {422,"Unprocessable Content"},;
                      {426,"Upgrade Required"},;
                      {500,"Internal Server Error"},;
                      {501,"Not Implemented"},;
                      {502,"Bad Gateway"},;
                      {503,"Service Unavailable"},;
                      {504,"Gateway Timeout"},;
                      {505,"HTTP Version Not Supported"} }

   LOCAL nAt

   IF ( nAt := AScan( aStatus, {|v| v[ 1 ] == nStatus } ) ) > 0
      RETURN ToString( aStatus[ nAt, 1 ] ) + " " + aStatus[ nAt, 2 ]
   ENDIF

RETURN "200 OK"

//------------------------------------------------------------------------------

METHOD SessionId() CLASS ZEngine

   STATIC cSession

   IF Empty( cSession )
      IF HB_HHasKey( ::hReqCookies, "session" )
         cSession := HB_HGet( ::hReqCookies, "session" )
      ELSE
         cSession := HB_SHA1( HB_TToC( HB_DateTime() ) )
      ENDIF
   ENDIF

RETURN cSession

//------------------------------------------------------------------------------

METHOD LogEnv() CLASS ZEngine

   LOCAL cKey, cValue

   FOR EACH cKey, cValue IN HB_HKeys( ::hEnvironment ), HB_HValues( ::hEnvironment )
      ::LogText( cKey + " = " + XA_urlDecode( cValue ) )
   NEXT

RETURN nil

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION GenLog( Value, cText )

   LOCAL cKey, xVal

   IF cText == NIL
      cText := ""
   ENDIF

   IF HB_IsArray( Value )
       cText += "["
      FOR EACH xVal IN Value
         GenLog( xVal, @cText )
         IF !xVal:__enumIsLast()
            cText += ", "
         ENDIF
      NEXT
      cText += "]"
   ELSEIF HB_IsHash( Value )
       cText += "{"
      FOR EACH cKey, xVal IN HB_HKeys( Value ), HB_HValues( Value )
         cText += cKey + ":"
         GenLog( xVal, @cText )
         IF !xVal:__enumIsLast()
            cText += ", "
         ENDIF
      NEXT
      cText += "}"
   ELSE
      cText += ToString( Value )
   ENDIF

RETURN cText
