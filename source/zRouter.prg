/*
 * Proyecto: xaWeb framework
 * Fichero: ZHtmRouter.prg
 * Descripción: HTML router
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZRouter
PUBLISHED:
   DATA oApplication    AS CLASS WApp

   METHOD New( oApp AS CLASS WApp )   CONSTRUCTOR
   METHOD Start( cDefDoc )  // --> NIL

RESERVED:
   METHOD End()
   METHOD CheckDefault( cDoc )
   METHOD CheckParams()
   METHOD RunDoc( hParam1, hParam2, lService )

PROTECTED:
   DATA oDoc

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oApp ) CLASS ZRouter

   ::oApplication := oApp

RETURN Self

//------------------------------------------------------------------------------

METHOD End( oApp ) CLASS ZRouter

   ::oApplication := NIL

RETURN Self

//------------------------------------------------------------------------------

METHOD Start( cDefaultDoc ) CLASS ZRouter

   LOCAL cKey, cVal
   LOCAL nPort, nTimeout
   LOCAL lDebug

   App:cDoc := cDefaultDoc

   SWITCH Engine:nOperType
   CASE OPER_DEFAULT
      IF ::CheckDefault( cDefaultDoc )
         ::RunDoc( Engine:hParams )
      ENDIF
      EXIT
   CASE OPER_ACTION
      IF ::CheckParams()
         ::RunDoc( Engine:hParams, Engine:hEvent )
      ENDIF
      EXIT
   CASE OPER_FORM
      IF ::CheckParams()
         ::RunDoc( Engine:hPost )
      ENDIF
      EXIT
   CASE OPER_SERVICE
      Engine:AddHeader( "Content-type", "application/json;charset=utf-8" )
      IF ::CheckParams()
         ::RunDoc( Engine:hParams,, .T. )
      ENDIF
      EXIT
   CASE OPER_SERVICEJS
      Engine:AddHeader( "Content-type", "application/javascript;charset=utf-8" )
      IF ::CheckParams()
         ::RunDoc( Engine:hParams,, .T. )
      ENDIF
      EXIT
   CASE OPER_CONFIG
      WITH OBJECT Engine
         cKey := :cConfigKey
         IF !Empty( cKey ) .AND. cVal == cKey
            :Config()
            Document := NIL
         ENDIF
      END WITH
      EXIT
   CASE OPER_LISTENING
      WITH OBJECT Engine
         nPort    := Val( HB_HGetDef( :hParams, "listen", "" ) )
         nTimeout := Val( HB_HGetDef( :hParams, "timeout", "" ) )
         lDebug   := ( HB_HGetDef( :hParams, "debug", "n" ) == "y" )
         cKey     := HB_HGetDef( :hParams, "key", "" )
         IF cKey == :cListenKey
            :ListenLoop( nPort, nTimeout, lDebug )
         ENDIF
      END WITH
      EXIT
   CASE OPER_CUSTOM
      IF ::CheckDefault( cDefaultDoc )
         Engine:cRequestMethod := "CustomProcess"
         ::RunDoc( Engine:hParams )
      ENDIF
      EXIT
   END SWITCH

RETURN NIL

//------------------------------------------------------------------------------

METHOD CheckDefault( cDoc ) CLASS ZRouter

   IF Empty( cDoc )
      RETURN .F.
   ENDIF

   TRY
      ::oDoc := HB_ExecFromArray( cDoc ):New( Self )
   CATCH
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'Invalid document: "' + cDoc +'"'
         :Operation   := "WRouter:Start()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   END

RETURN HB_IsObject( ::oDoc )

//------------------------------------------------------------------------------

METHOD CheckParams() CLASS ZRouter

   LOCAL cDoc, cMsg
   LOCAL lError

   lError := .F.

   WITH OBJECT Engine
      cDoc := :cRequestDoc
      cMsg := :cRequestMethod
   END WITH

   IF Empty( cDoc ) .OR. Empty( cMsg )
      RETURN .F.
   ENDIF

   TRY
      ::oDoc := HB_ExecFromArray( cDoc ):New( Self )
   CATCH
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'Invalid document: "' + cDoc +'"'
         :Operation   := "WRouter:Start()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      lError := .T.
   END

   IF !lError.AND. !__objHasMethod( ::oDoc, cMsg )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := 'Invalid method: "' + cMsg +'"'
         :Operation   := "WRouter:Start()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      lError := .T.
   ENDIF

RETURN !lError

//------------------------------------------------------------------------------

METHOD RunDoc( hParam1, hParam2, lSrv ) CLASS ZRouter

   LOCAL oCtl
   LOCAL cMsg := Engine:cRequestMethod
   LOCAL cVal, cKey, cNam

   DEFAULT lSrv TO .F.

   WITH OBJECT ::oDoc
      Engine:DocumentReady( ::oDoc )
      :cAction := cMsg
      :Start()
      IF !Empty( cMsg )
         :RunMethod( Engine:nOperType, cMsg, hParam1, hParam2 )
      ENDIF
      IF !lSrv
         :RenderRegisteredSections()
      ENDIF
   END WITH

   WITH OBJECT Engine
      IF Len( :hPost ) > 0
         FOR EACH cKey, cVal IN HB_HKeys( :hPost ), HB_HValues( :hPost )
            IF "--" $ cKey
               cKey := HB_TokenGet( cKey, 1, "--" )
               cNam := HB_TokenGet( cKey, 2, "--" )
               oCtl := ::oDoc:SearchControl( cKey )
               IF oCtl != NIL .AND. __objHasData( oCtl, cNam )
                  __objSendMsg( oCtl, "_" + cNam, cVal )
               ENDIF
            ENDIF
         NEXT
         IF ::oDoc:lCleanPost
            :CleanPost()
         ENDIF
      ENDIF
   END WITH

RETURN nil

//------------------------------------------------------------------------------

