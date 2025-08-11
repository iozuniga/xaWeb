/*
 * Proyecto: xaWeb framework
 * Fichero: ZTask.prg
 * Descripción: Task class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZTask
PUBLISHED:
   DATA aParams      INIT {}
   DATA cType        INIT "url" VALUES "url", "service", "action", "script", "custom"
   DATA cUrl         INIT ""
   DATA cMethod      INIT ""
   DATA cDocument    INIT ""
   DATA lNoCache     INIT .F.

   METHOD Url( cUrl )                        INLINE ::New( "url", cUrl )  // --> Self
   METHOD Action( cMethod, cDocument )       INLINE ::New( "action", cMethod, cDocument ) // --> Self
   METHOD Service( cMethod, cDocument )      INLINE ::New( "service", cMethod, cDocument ) // --> Self
   METHOD Custom( cOperation )               INLINE ::New( "custom", cOperation ) // --> Self
   METHOD Script( cScript )                  INLINE ::New( "script", cScript ) // --> Self
   METHOD AddParam( cName, cValue )          INLINE AAdd( ::aParams, { cName, cValue } ), Self   // --> Self
   METHOD SetParam( nPos, xValue )           INLINE ::aParams[ nPos, 2 ] := xValue  // --> xValue
   METHOD Html( ... )  // --> cUri

RESERVED:
   METHOD New( cType, cMethod, cDocument )   CONSTRUCTOR
   METHOD End()         INLINE ::aParams := {}
   METHOD PreProcess()  VIRTUAL

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cType, cMethod, cDocument ) CLASS zTask

   DEFAULT cType TO "url", cMethod TO ""

   ::cType := cType

   IF cType == "url"
      ::cUrl := cMethod
   ELSE
      IF Empty( cDocument ) .AND. !__objHasMethod( Document, cMethod )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := 'Invalid Method "' + cMethod +'"'
            :Operation   := "WTask:New() [" + cType + "]"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
         RETURN "error"
      ENDIF
      DEFAULT cDocument TO Document:Classname
      ::cMethod    := cMethod
      ::cDocument  := cDocument
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD Html( ... ) CLASS zTask

   LOCAL aParams
   LOCAL cUri
   LOCAL aValue
   LOCAL nFor, nLen

   aParams := hb_Aparams()
   nLen    := Len( aParams )

   IF nLen > 0
      FOR nFor := 1 TO nLen
         ::SetParam( nFor, aParams[ nFor ] )
      NEXT
   ENDIF

   ::PreProcess()

   SWITCH ::cType
   CASE "url"
      cUri := ::cUrl
      IF Len( ::aParams ) > 0
         cUri += "?"
      ENDIF
      IF !Empty( ::aParams )
         FOR EACH aValue IN ::aParams
            cUri +=  "&" + aValue[ 1 ] + "=" + AdaptValue( aValue[ 2 ] )
         NEXT
      ENDIF
      EXIT
   CASE "service"
   CASE "action"
      cUri := Engine:BaseUri() + "?" + ::cType + "=" + Lower(  ::cDocument ) + "-" + ::cMethod
      IF !Empty( ::aParams )
         FOR EACH aValue IN ::aParams
            cUri +=  "&" + aValue[ 1 ] + "=" + AdaptValue( aValue[ 2 ] )
         NEXT
      ENDIF
      IF ::lNoCache
         cUri += "&random=" + HB_NumToHex( HB_RandomInt( 999999 ) )
      ENDIF
      EXIT
   CASE "custom"
      cUri := Engine:BaseUri() + "?" + ::cType + "=CustomProcess" + "-" + ::cMethod
      EXIT
   CASE "script"
      cUri := ::cMethod
      EXIT
   END SWITCH

RETURN cUri

//------------------------------------------------------------------------------

STATIC FUNCTION AdaptValue( xVal )

RETURN Tip_UrlEncode( ToString( xVal ) )

//------------------------------------------------------------------------------
