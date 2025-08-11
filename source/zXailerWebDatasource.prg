/*
 * Proyecto: xaWeb framework
 * Fichero: zXailerWebDatasource.prg
 * Descripción: Compatibilidad con TWwebDatasource de Xailer
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 *
 * IMPORTANT NOTE:
 * This code must be included in all your PHP files in order to access
 * the simulated $_GET & $_POST global variables:
 * if (php_sapi_name() === 'cli') {
 *   parse_str(implode('&', array_slice($argv, 1)), $_GET);
 *   parse_str(stream_get_contents(STDIN), $_POST);
 * }
 * Use the hashes hGet & cPost to include any parameter you may need
 */

#include "xaWeb.ch"

//------------------------------------------------------------------------------

CLASS ZXailerWebDatasource FROM TWebDataSource
PUBLISHED:
   METHOD cPhpRunner( cValue )   SETGET
   METHOD cPhpPath( cValue )     SETGET
   METHOD cPhpModule( cValue )   SETGET

RESERVED:
   DATA oConnection    AS CLASS WPhpRunner
   DATA lDisplayErrors INIT .F.
   DATA lAbortOnErrors INIT .T.

   METHOD New( oParent, cConnect ) CONSTRUCTOR
   METHOD Connect()                INLINE .T.
   METHOD InternetRequest( cCommand, cData, nValid )

   METHOD MyIP()     INLINE "127.0.0.1"
   METHOD DnloadFile( cFilename, lUtf )
   METHOD RenameFile( cOld, cNew, lUtf )
   METHOD DeleteFile( cFilename, lUtf )
   METHOD SendMail( cTo, cName, cSubject, cBody, lHtml )
   METHOD HttpValid() VIRTUAL
   METHOD RunError()  INLINE ::oConnection:nRunError

END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cConnect ) CLASS ZXailerWebDatasource

   ::Super:New( oParent, cConnect )
   ::oConnection := TInternet():New( Self )

RETURN Self

//------------------------------------------------------------------------------

METHOD cPhpRunner( cValue ) CLASS ZXailerWebDatasource

   IF PCount() > 0
      ::oConnection:cPhpRunner := cValue
   ENDIF

RETURN ::oConnection:cPhpRunner

//------------------------------------------------------------------------------

METHOD cPhpPath( cValue ) CLASS ZXailerWebDatasource

   IF PCount() > 0
      ::oConnection:cPhpPath := cValue
   ENDIF

RETURN ::oConnection:cPhpPath

//------------------------------------------------------------------------------

METHOD cPhpModule( cValue ) CLASS ZXailerWebDatasource

   IF PCount() > 0
      ::oConnection:cPhpModule := cValue
   ENDIF

RETURN ::oConnection:cPhpModule

//------------------------------------------------------------------------------

METHOD InternetRequest( cCommand, cData, nValid ) CLASS ZXailerWebDatasource

   WITH OBJECT ::oConnection
      HB_HSet( :hPost, "data", StringToHex( cData ) )
      HB_HSet( :hGet, "command", Upper( cCommand ) )
      HB_HSet( :hGet, "validate", ToString( nValid ) )
      IF !Empty( ::cDatabase )
         HB_HSet( :hGet, "dbName", ::cDatabase )
      ENDIF
      IF :Run()
         cData := :cResult
      ELSE
         cData := ""
      ENDIF
   END WITH

RETURN cData

//------------------------------------------------------------------------------

METHOD DnloadFile( cFilename, lUtf ) CLASS ZXailerWebDatasource

   LogDebug( "Method ZXailerWebDatasource:DnLoadFile() not implemented" )

RETURN ""

//------------------------------------------------------------------------------

METHOD RenameFile( cOld, cNew, lUtf ) CLASS ZXailerWebDatasource

   LogDebug( "Method ZXailerWebDatasource:RenameFile() not implemented" )

RETURN .F.

//------------------------------------------------------------------------------

METHOD DeleteFile( cFilename, lUtf ) CLASS ZXailerWebDatasource

   LogDebug( "Method ZXailerWebDatasource:DeleteFile() not implemented" )

RETURN .F.

//------------------------------------------------------------------------------

METHOD SendMail( cTo, cName, cSubject, cBody, lHtml ) CLASS ZXailerWebDatasource

   LogDebug( "Method ZXailerWebDatasource:SendMail() not implemented" )

RETURN .F.

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS TInternet FROM WPhpRunner //STATIC
RESERVED:
   DATA Handle    INIT 1
   METHOD End()   VIRTUAL
END CLASS

//------------------------------------------------------------------------------



