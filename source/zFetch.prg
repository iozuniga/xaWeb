/*
 * Proyecto: xaWeb framework
 * Fichero: ZFetch.prg
 * Descripción: JavaScript Fetch operations control class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZFetch

PUBLISHED:
   DATA oUrl            AS CLASS WTask
   DATA hResHeaders     INIT { => }
   DATA hJsParams       INIT { => }
   DATA cSourceId       INIT ""
   DATA cTargetId       INIT ""
   DATA cBody           INIT ""
   DATA cCallBack       INIT ""
   DATA cJsFunction     INIT "" // always starts with xa_fetch. Js funcion to use instead of xa_Fetch
   DATA cMethod         INIT "GET"         VALUES "GET", "POST", "PUT", "DELETE",;
                                                  "PATCH", "CONNECT", "OPTIONS",;
                                                  "TRACE", "HEAD"
   DATA cMode           INIT ""            VALUES "cors", "no-cors", "same-origin"
   DATA cCache          INIT ""            VALUES "default", "no-cache", "reload",;
                                                  "force-cache", "only-if-cached"
   DATA cCredentials    INIT ""            VALUES "include", "same-origin", "omit"
   DATA cRedirect       INIT ""            VALUES "manual", "follow", "error"
   DATA cReferrerPolicy INIT ""            VALUES "no-referrer",;
                                                  "no-referrer-when-downgrade",;
                                                  "origin",;
                                                  "origin-when-cross-origin",;
                                                  "same-origin", "strict-origin",;
                                                  "strict-origin-when-cross-origin",;
                                                  "unsafe-url"
   DATA cPriority       INIT ""            VALUES "auto", "high", "low"
   DATA cContentType    INIT "application/json" ;
                                           VALUES "application/json",;
                                                  "application/x-www-form-urlencoded",;
                                                  "multipart/form-data",;
                                                  "text/plain",;
                                                  "application/octet-stream",;
                                                  "application/javascript"
   DATA lKeepAlive      INIT .F.

   METHOD New( cUrl ) CONSTRUCTOR

   METHOD AddJsParam( cId, cProperty )     INLINE HB_HSet( ::hJsParams,  cId, cProperty )
   METHOD AddHeader( cName, cValue )       INLINE HB_HSet( ::hResHeaders, cName, cValue )
   METHOD cUrl( cValue ) SETGET  // --> cUrl

RESERVED:
   METHOD End()
   METHOD Html()
   METHOD IsJS() INLINE ( ::cContentType == "application/javascript" )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( xUrl ) CLASS ZFetch

   ::cUrl := xUrl

   HB_HCaseMatch( ::hResHeaders, .F. )
   HB_HAutoAdd( ::hResHeaders, .T. )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZFetch

   ::hResHeaders := NIL
   ::hResHeaders := NIL

RETURN Self

//------------------------------------------------------------------------------

METHOD cUrl( cValue ) CLASS ZFetch

   IF PCount() > 0
      IF HB_IsObject( cValue )
         ::oUrl := cValue
      ELSE
         ::oUrl := WTask():Url( cValue )
      ENDIF
   ELSEIF HB_IsObject( ::oUrl )
      RETURN ::oUrl:Html()
   ENDIF

RETURN ""

//------------------------------------------------------------------------------

METHOD Html() CLASS ZFetch

   LOCAL oCtl
   LOCAL hHash := {=>}
   LOCAL cUrl, cTmp

   IF Empty( ::cUrl )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := '"cUrl" not set on object'
         :Operation   := "WFetch:Html"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

   IF !::IsJS()
      IF Empty( ::cTargetId )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := '"cTargetId" not set on object'
            :Operation   := 'WFetch:Html()'
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
         RETURN ""
      ENDIF

      oCtl := Document:SearchControl( ::cTargetId )

      IF Empty( oCtl )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := 'Invalid "cTargetId" on object (' + ::cTargetId + ')'
            :Operation   := 'WFetch:Html()'
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
         RETURN ""
      ENDIF

      IF Empty( ::cCallBack )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := '"cCallBack" property not set'
            :Operation   := 'WFetch:Html:cCallBack (empty)'
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
         RETURN ""
      ENDIF
   ELSE
      IF Empty( ::cMode )
         ::cMode := "cors"
      ENDIF
      IF Empty( ::cReferrerPolicy )
         ::cReferrerPolicy := "strict-origin-when-cross-origin"
      ENDIF
   ENDIF

   ::AddHeader( "Content-type", ::cContentType )

   HB_HSet( hHash, "headers", ::hResHeaders )
   HB_HSet( hHash, "method", ::cMethod )
   IF !Empty( ::cMode )
      HB_HSet( hHash, "mode", ::cMode )
   ENDIF
   IF !Empty( ::cCache )
      HB_HSet( hHash, "cache", ::cCache )
   ENDIF
   IF !Empty( ::cCredentials )
      HB_HSet( hHash, "credentials", ::cCredentials )
   ENDIF

   IF !Empty( ::cRedirect )
      HB_HSet( hHash,"redirect", ::cRedirect )
   ENDIF

   IF !Empty( ::cReferrerPolicy )
      HB_HSet( hHash,"referrerPolicy", ::cReferrerPolicy )
   ENDIF

   IF !Empty( ::cPriority )
      HB_HSet( hHash,"priority", ::cPriority )
   ENDIF

   IF !Empty( ::cBody )
      HB_HSet( hHash,"body", ::cBody )
   ENDIF

   IF ::lKeepAlive
      HB_HSet( hHash,"keepalive", "true" )
   ELSE
      HB_HSet( hHash,"keepalive", "" )
   ENDIF

   IF HB_IsObject( ::cUrl )
      cUrl := ::cUrl:Html()
   ELSE
      cUrl := ::cUrl
   ENDIF

   cTmp := 'xa_fetch' + ::cJsFunction + '( "' +;
           ::cTargetId + '", "' +;
           ::cSourceId + '", "' + ;
           ::cCallBack + '", "' + ;
           cUrl + '",' + ;
           HB_JsonEncode( hHash ) + ;
           ',null,' + HB_JsonEncode( ::hJsParams ) + ');'

RETURN cTmp
