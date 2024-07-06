/*
 * Proyect: XailerWeb framework
 * File: HttpCookie.prg
 * Description: HTTP Cookies control class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"

#define INFINITE     "Fri, 31 Dec 9999 23:59:59 GMT"

STATIC aMonths := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }
STATIC aDays   := { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" }

CLASS ZCookie

EXPORT:
   DATA cName        INIT ""
   DATA cValue       INIT ""
   DATA cPath        INIT "/"
   DATA cDomain      INIT ""
   DATA cExpires     INIT ""
   DATA cSameSite    INIT "lax" VALUES "strict", "lax", "none"
   DATA cSystemTime  INIT "GMT"
   DATA nMaxAge      INIT 0  // seconds
   DATA lSecure      INIT .T.
   DATA lHttpOnly    INIT .F.

   METHOD New( cName, cValue, lSecure ) CONSTRUCTOR
   METHOD Render()
   METHOD dExpires( dValue ) SETGET

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cName, cValue, lSecure ) CLASS ZCookie

   DEFAULT lSecure TO .T.

   ::cName   := cName
   ::cValue  := cValue
   ::lSecure := lSecure

RETURN Self

//------------------------------------------------------------------------------

METHOD dExpires( dValue ) CLASS ZCookie

   IF PCount() > 0
      ::cExpires := DateToCookieExpire( dValue, ::cSystemTime )
   ELSE
      dValue := CookieExpireToDate( ::cExpires )
   ENDIF

RETURN dValue

//------------------------------------------------------------------------------

METHOD Render() CLASS ZCookie

   LOCAL cBuffer

   cBuffer := "Set-Cookie: " + ::cName + "=" + ::cValue

   IF !Empty( ::cPath )
      cBuffer += "; Path=" + ::cPath
   ENDIF

   IF !Empty( ::cDomain )
      cBuffer += "; Domain=" + ::cDomain
   ENDIF

   IF !Empty( ::cExpires )
      cBuffer += "; Expires= "+ ::cExpires
   ENDIF

   IF !Empty( ::cSameSite )
      cBuffer += "; SameSite=" + ::cSameSite
   ENDIF

   IF !Empty( ::nMaxAge )
      cBuffer += "; Max-Age=" + ToString( ::nMaxAge )
   ENDIF

   IF ::lSecure
      cBuffer += "; Secure"
   ENDIF

   IF ::lHttpOnly
      cBuffer += "; HttpOnly"
   ENDIF

RETURN cBuffer

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION DateToCookieExpire( dValue, cSysTime )

   LOCAL cValue

   cValue := aDays[ DoW( dValue ) ] + ", " + ;
             aMonths[ Month( dValue ) ] + " " + ;
             Str( Year( dValue ), 4 )

   IF HB_IsDateTime( dValue )
      cValue += " " + HB_TToC( dValue, "", "HH:MM:SS" ) + " " + cSysTime
   ENDIF

RETURN cValue

//------------------------------------------------------------------------------

STATIC FUNCTION CookieExpireToDate( cDate )

   LOCAL cTime
   LOCAL nDay, nMonth, nYear, nHour, nMin, nSec, nFor, nAt

   nDay   := 0
   nMonth := 0
   nYear  := 0
   nHour  := 0
   nMin   := 0
   nSec   := 0

   FOR nFor := 1 TO Len( cDate )
      IF IsDigit( SubStr( cDate, nFor, 1 ) )
         nAt := nFor
         EXIT
      ENDIF
   NEXT

   IF nAt > 0
      nDay := Val( SubStr( cDate, nAt, 2 ) )
      IF nDay > 0
         nAt := AScan( aMonths, {|v| PadC( Lower( v ), 5, " " ) $ Lower( cDate ) } )
         IF nAt > 0
            nMonth := nAt
            nAt := At( PadC( Lower( aMonths[ nAt ] ), 5, " " ), Lower( cDate ) )
            IF nAt > 0
               nYear := Val( SubStr( cDate, nAt + 5, 4 ) )
               cTime := SubStr( cDate, nAt + 10 )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF !Empty( cTime )
      nHour := Val( Left( cTime, 2 ) )
      nMin  := Val( SubStr( cTime, 4, 2 ) )
      nSec  := Val( SubStr( cTime, 8, 2 ) )
   ENDIF

RETURN HB_DateTime( nYear, nMonth, nDay, nHour, nMin, nSec )

//------------------------------------------------------------------------------
