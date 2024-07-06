/*
 * Proyect: XailerWeb framework
 * File: ZFetch.prg
 * Description: JavaScript Fetch operations control class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZFetch

EXPORTED:
   DATA hResHeaders     INIT { => }
   DATA aParams         INIT {}
   DATA cSourceId       INIT ""
   DATA cTargetId       INIT ""
   DATA cUrl            INIT ""
   DATA cBody           INIT ""
   DATA cCallBack       INIT ""
   DATA cJsFunction     INIT "" // always starts with xw_fetch
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
   DATA lService        INIT .T.

   DATA nStatus         INIT 0 READONLY

   METHOD New( cUrl ) CONSTRUCTOR
   METHOD AddParam( cId, cProperty )       INLINE AAdd( ::aParams, { cId, cProperty } )
   METHOD AddHeader( cName, cValue )       INLINE HB_HSet( ::hResHeaders, cName, cValue )


RESERVED:
   METHOD Url()
   METHOD Html()
   METHOD IsJS() INLINE ( ::cContentType == "application/javascript" )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cUrl ) CLASS ZFetch

   DEFAULT cUrl TO ""

   ::cUrl := cUrl

   HB_HCaseMatch( ::hResHeaders, .F. )
   HB_HAutoAdd( ::hResHeaders, .T. )

RETURN Self

//------------------------------------------------------------------------------

METHOD Url() CLASS ZFetch

   LOCAL cUrl

   cUrl := ::cUrl

RETURN cUrl

//------------------------------------------------------------------------------

METHOD Html() CLASS ZFetch

   LOCAL oCtl
   LOCAL aValue
   LOCAL cUrl, cKey, cValue, cJson, cTmp

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

   cJson := '{"method":"' + ::cMethod + '"'

   IF !Empty( ::cMode )
      cJson += ',"mode":"' + ::cMode + '"'
   ENDIF

   IF !Empty( ::cCache )
      cJson += ',"cache":"' + ::cCache + '"'
   ENDIF

   IF !Empty( ::cCredentials )
      cJson += ',"credentials":"' + ::cCredentials + '"'
   ENDIF

   IF !Empty( ::cRedirect )
      cJson += ',"redirect":"' + ::cRedirect + '"'
   ENDIF

   IF !Empty( ::cReferrerPolicy )
      cJson += ',"referrerPolicy":"' + ::cReferrerPolicy + '"'
   ENDIF

   IF !Empty( ::cPriority )
      cJson += ',"priority":"' + ::cPriority + '"'
   ENDIF

   IF !Empty( ::cBody )
      cJson += ',"body":"' + ::cBody + '"'
   ENDIF

   cJson += IIF( ::lKeepAlive, ',"keepalive":"true"', '' ) + ;
            ',"headers": {'

   cTmp := ''

   FOR EACH cKey, cValue IN HB_HKeys( ::hResHeaders ), HB_HValues( ::hResHeaders )
      cTmp += ',"' + cKey + '":"' + cValue + '"'
   NEXT

   cTmp = SubStr( cTmp, 2 ) + '}}'
   cJson += cTmp

   cTmp := ''

   FOR EACH aValue IN ::aParams
      cTmp += "&" + aValue[ 1 ] + "=" + ToString( aValue[ 2 ] )
   NEXT

   cTmp := 'xw_fetch' + ::cJsFunction + '( "' +;
           ::cTargetId + '", "' +;
           ::cSourceId + '", "' + ;
           ::cCallBack + '", "' + ;
           ::Url() + cTmp +'",' + cJson + ');'

RETURN cTmp