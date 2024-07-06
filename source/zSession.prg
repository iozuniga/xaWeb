/*
 * Proyect: XailerWeb framework
 * File: ZSession.prg
 * Description: Basic class for session management
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 *
 * NOTE:
 * This is a very basic session manager. The idea is that you create your
 * own session manager with this simple four methods, that may set and
 * retrieve the sessions data from a database or a file.
 * This session manager saves in JSON format the content of the session on
 * files that are located under the hb_DirBase()/sessions directory.
  */

#include "xailerweb.ch"
#include "directry.ch"

FUNCTION Session()

   STATIC oSession

   IF oSession == NIL
      oSession := WSession():New()
      IF HB_IsObject( Document )
         Document:LoadSession()
      ENDIF
   ENDIF

RETURN oSession

//------------------------------------------------------------------------------

CLASS ZSession
EXPORTED:
   DATA cPath  INIT ""

   METHOD New( oParent )            CONSTRUCTOR
   METHOD Save( hHash )
   METHOD Load() // --> hHash
   METHOD nSessionTTL( nSecs )      SETGET

PROTECTED:
   METHOD Purge()
END CLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZSession

   ::cPath := HB_DirBase() + "sessions" + hb_OsPathSeparator()
   ::Purge()

RETURN Self

//------------------------------------------------------------------------------

METHOD Save( hHash ) CLASS ZSession

   LOCAL cJson

   IF !HB_IsHash( hHash )
      RETURN .F.
   ENDIF

   IF !HB_DirExists( ::cPath )
      IF HB_DirCreate( ::cPath ) != 0
         RETURN .F.
      ENDIF
   ENDIF

   cJson := HB_JsonEncode( hHash )

RETURN HB_MemoWrit( ::cPath + Engine:SessionId() + ".ses", cJson )

//------------------------------------------------------------------------------

METHOD Load() CLASS ZSession

   LOCAL cFile, cJson, hHash

   cFile := ::cPath + Engine:SessionId() + ".ses"

   IF HB_FileExists( cFile )
      cJson := HB_MemoRead( cFile )
      IF HB_JsonDecode(cJson, @hHash ) == 0
         hHash := NIL
      ENDIF
   ENDIF

RETURN hHash

//------------------------------------------------------------------------------

METHOD Purge() CLASS ZSession

   LOCAL aFiles, aFile
   LOCAL dDate
   LOCAL cTime
   LOCAL nSecs

   aFiles := Directory( ::cPath + "*.ses" )
   dDate  := Date()
   cTime  := Time()
   nSecs  := ::nSessionTTL

   FOR EACH aFile IN aFiles
      IF ( aFile[ F_DATE ] < dDate ) .OR. ;
         TimeToSec( cTime ) - TimeToSec( aFile[ F_TIME ] ) > nSecs
         FErase( ::cPath + aFile[ F_NAME ] )
      ENDIF
   NEXT

RETURN nil

//------------------------------------------------------------------------------

METHOD nSessionTTL( nSecs ) CLASS ZSession

   IF PCount() > 0 .AND. HB_IsNumeric( nSecs )
      Engine:nSessionTTL := nSecs
   ENDIF

RETURN Engine:nSessionTTL

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION TimeToSec( cTime )

   LOCAL nHor, nMin, nSec

   nHor := Val( Substr( cTime, 1, 2 ) )
   nMin := Val( Substr( cTime, 4, 2 ) )
   nSec := Val( Substr( cTime, 7, 2 ) )

RETURN ( ( nHor * 3600 ) + ( nMin * 60 ) + nSec )

//------------------------------------------------------------------------------

