/*
 * Proyect: XailerWeb framework
 * File: ZApp.prg
 * Description: Class Application
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "fileio.ch"
#include "error.ch"

REQUEST HB_GT_CGI_DEFAULT
REQUEST HB_CODEPAGE_UTF8EX

// TODO: Quitar cuando no sea necesaria la librería de Xailer

CLASS XApplication
   METHOD Initialize()
ENDCLASS

METHOD Initialize()
   RELEASE Printer
   RELEASE Screen
   RELEASE Appdata
   RELEASE SelfThread
RETURN Self

//--------------------------------------------------------------------------

INIT PROCEDURE InitApplication()

   PUBLIC Application := Nil
   PUBLIC Engine := Nil
   PUBLIC Document := Nil

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )
   HB_CdpSelect( hb_cdpOS() )

   WApp():Initialize()

RETURN

//------------------------------------------------------------------------------

EXIT PROCEDURE TerminateApp()

   IF Application != Nil
      Errorblock( { || Break() } )
      Application:End()
   ENDIF

RETURN

//------------------------------------------------------------------------------

CLASS ZApp

EXPORTED:

   METHOD Initialize() CONSTRUCTOR
   METHOD End()
   METHOD Run()               INLINE ::End()

   METHOD CmdLine()           INLINE HB_CmdLine()
   METHOD ACmdLine()          INLINE HB_ACmdLine()
   METHOD CmdParam( cParam )  INLINE HB_ArgString( cParam )
   METHOD CmdExecutable()     INLINE HB_ArgV( 0 )
   METHOD cTitle( Value )     SETGET

HIDDEN:
   DATA FcTitle   INIT ""

END CLASS

//------------------------------------------------------------------------------

METHOD Initialize() CLASS ZApp

   IF ValType( Application ) != "O"
      Application    := Self
      ::cTitle       := ""
      Engine := WEngine():New()
      ErrorBlock( {| oError, nStack | XW_Error( oError, nStack ) } )
      RETURN Application
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD cTitle( Value ) CLASS ZApp

   IF PCount() > 0
      ::FcTitle := Value
      IF HB_IsObject( Document )
         Document:cTitle := Value
      ENDIF
   ENDIF

RETURN ::FcTitle

//------------------------------------------------------------------------------

METHOD End() CLASS ZApp

   IF HB_IsObject( Engine )
      Engine:End()
      Engine := NIL
   ENDIF

   Application := Nil

RETURN nil

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

FUNCTION XW_Trim( cText, nTabs )

   LOCAL cTabs

   DEFAULT nTabs TO 1

   nTabs := Len( TAB ) * nTabs
   cTabs := Space( nTabs )

   IF HB_LeftEq( cText, cTabs )
     cText := SubStr( cText, nTabs + 1 )
   ENDIF

RETURN cText

//------------------------------------------------------------------------------

FUNCTION XW_RouteEvent( oControl, cMsg, ... )

   LOCAL oDoc

   IF !Empty( cMsg )
      oDoc := Document
      IF HB_IsObject( oDoc )
         IF HB_IsBlock( cMsg )
            Eval( cMsg, oControl, ... )
         ELSEIF HB_IsString( cMsg ) .AND. __objHasMethod( oDoc, cMsg )
            __objSendMsg( oDoc, cMsg, oControl, ...)
         ENDIF
      ENDIF
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

FUNCTION XW_SetDataValue( oControl, cMsg, aValues, uValue )

   LOCAL xVal
   LOCAL cVal
   LOCAL nAt
   LOCAL lOk := .f.

   IF aValues == NIL
      RETURN uValue
   ENDIF

   FOR EACH xVal IN aValues
      IF ( HB_IsString( xVal ) .AND. xVal == "*" ) .OR. VarsEqual( xVal, uValue )
         lOk := .T.
         EXIT
      ENDIF
   NEXT

   IF !lOk
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := "Invalid value for [" + SubStr( cMsg, 2 ) + "]. It should be one of this values: {" + aJoin( aValues, "," ) + "}."
         :Operation   := ""
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ELSE
      __objSendMsg( oControl, "_" + cMsg, uValue )
      IF oControl:IsKindOf( "WStyle" )
         oControl:SetStyle( SubStr( cMsg, 2 ), uValue )
      ENDIF
   ENDIF

RETURN uValue

//------------------------------------------------------------------------------

STATIC FUNCTION aJoin( aData, cSeparator ) // --> cString

   LOCAL cValue, cTemp

   DEFAULT cSeparator TO hb_eol()

   cValue := ""

   FOR EACH cTemp IN aData
      cValue += cTemp
      IF !cTemp:__enumIsLast()
         cValue += cSeparator
      ENDIF
   NEXT

RETURN cValue

//------------------------------------------------------------------------------

STATIC FUNCTION XW_Error( oError, nStack )

   LOCAL cMsg, cProc
   LOCAL lRet

   DEFAULT nStack TO 2

   IF oError:Severity == ES_WARNING
      cMsg := "Warning on "
   ELSE
      cMsg := "Error on "
   ENDIF

   cMsg += Engine:ScriptName + ": "  + ;
           oError:Description() + " / " + oError:operation() + ;
           ": " + oError:Filename()

   IF !Empty( oError:subsystem() )
      cMsg += oError:subsystem() + "[" + LTrim( Str( oError:SubCode() ) ) + "]" + hb_eol()
   ELSE
      IF oError:OSCode() > 0
         cMsg += "OS Code [" + LTrim( Str( oError:OSCode ) ) + "]" + hb_eol()
      ENDIF
   ENDIF

   cMsg += hb_eol() + "Called from:" + hb_eol()

   DO WHILE !Empty( cProc := Trim( ProcName( nStack ) ) )
      cMsg += TAB + cProc + "(" + AllTrim( Str( ProcLine( nStack ) ) ) + ")" + hb_eol()
      nStack++
   ENDDO

   cMsg += hb_eol()

   Engine:LogText( cMsg )

   IF oError:Severity == ES_WARNING
      ErrorLevel( 1 )
      IF HB_IsObject( Document )
         Document:nStatus := 200
      ENDIF
      IF Engine:lHeaderDone
         Engine:JsConsole( oError:operation() + "->" + oError:Description() )
      ENDIF
      lRet := .t.
   ELSE
      Errorblock( { || Break() } )
      Engine:Render( .T. )
      Break( oError )
      lRet := .f.
   ENDIF

RETURN lRet

//------------------------------------------------------------------------------

