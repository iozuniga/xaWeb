/*
 * Proyect: XailerWeb framework
 * File: ZHtmRouter.prg
 * Description: HTML router
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZRouter
EXPORTED:
   DATA oApplication    AS CLASS WApp
   DATA aDocs           INIT {}

   METHOD New( oApp )   CONSTRUCTOR
   METHOD Start( cDefDoc )
   METHOD CreateDoc( cDoc )

   EVENT OnInitialize( oSender )

RESERVED:
   METHOD IsDoc( cDoc )

PROTECTED:
   DATA oDoc

ENDCLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZRouter

   ::OnInitialize()

RETURN Self

//------------------------------------------------------------------------------

METHOD IsDoc( cDoc ) CLASS ZRouter

   IF HB_IsObject( ::oDoc ) .AND. ( Lower( ::oDoc:Classname ) == cDoc )
      RETURN .T.
   ENDIF

   ::oDoc := NIL

   IF FunctionExists( cDoc )
      ::oDoc := HB_ExecFromArray( cDoc )
      IF !::oDoc:IsKindOf( "WDoc" )
         ::oDoc := NIL
      ENDIF
   ENDIF

RETURN HB_IsObject( ::oDoc )

//------------------------------------------------------------------------------

METHOD CreateDoc( cDoc ) CLASS ZRouter

   IF !::IsDoc( cDoc )
      RETURN .F.
   ENDIF

   ::oDoc:Create( Self )

   AAdd( ::aDocs, ::oDoc )

RETURN .T.

//------------------------------------------------------------------------------

METHOD Start( cDefDoc ) CLASS ZRouter

   LOCAL oCtl
   LOCAL hParam
   LOCAL cVal, cDoc, cMsg, cKey, cNam

   cVal := Engine:cFirstQuery

   IF !Empty( cVal )
      cDoc  := AllTrim( HB_TokenGet( cVal, 1, "-" ) )
      cMsg  := AllTrim( HB_TokenGet( cVal, 2, "-" ) )

      IF !::IsDoc( cDoc ) .OR. !__objHasMethod( ::oDoc, cMsg )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := 'Invalid document or method: "' + cVal +'"'
            :Operation   := "WRouter:Start()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
            RETURN NIL
         END WITH
      ENDIF

      IF !::CreateDoc( cDoc )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := 'Invalid HTML document: "' + cDoc +'"'
            :Operation   := "WRouter:Start()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
            RETURN NIL
         END WITH
      ENDIF

      SWITCH Engine:nOperType
      CASE OPER_ACTION
         hParam := Engine:hEvent
         EXIT
      CASE OPER_SERVICE
         Engine:AddHeader( "Content-type", "application/json;charset=utf-8" )
         hParam := Engine:hParams
         EXIT
      CASE OPER_SERVICEJS
         Engine:AddHeader( "Content-type", "application/javascript;charset=utf-8" )
         hParam := Engine:hParams
         EXIT
      CASE OPER_FORM
         hParam := Engine:hPost
         FOR EACH cKey, cVal IN HB_HKeys( hParam ), HB_HValues( hParam )
            IF "--" $ cKey
               cKey := HB_TokenGet( cKey, 1, "--" )
               cNam := HB_TokenGet( cKey, 2, "--" )
               oCtl := ::SearchControl( cKey )
               IF oCtl != NIL .AND. __objHasData( oCtl, cNam )
                  __objSendMsg( oCtl, "_" + cNam, cVal )
               ENDIF
            ENDIF
         NEXT
         EXIT
      END SWITCH
   ELSE
      IF !::CreateDoc( cDefDoc )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := 'Invalid HTML document: "' + cDefDoc +'"'
            :Operation   := "WRouter:Start()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
            RETURN NIL
         END WITH
      ENDIF
   ENDIF

   ::oDoc:Start( Engine:nOperType, cMsg, hParam )

RETURN NIL

//------------------------------------------------------------------------------
