
/*
 * Proyecto: xaWeb framework
 * Fichero: ZSession.prg
 * Descripción: Basic class for session management
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 *
 * NOTE:
 * This is a basic session manager. The idea is that you create your
 * own session manager with this simple four methods, that may set and
 * retrieve the sessions data from a database or a file.
 * This session manager saves in JSON format the content of the session on
 * files that are located under the hb_DirBase()/sessions directory.
 */

#include "xaWeb.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbsocket.ch"

FUNCTION Session() // --> WSession

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
PUBLISHED:

   METHOD Save( hHash )  // --> lValue
   METHOD Load( cFile )  // --> hHash
   METHOD ListeningPort( nPort )  // --> nPort
   METHOD nSessionTTL( nSecs )         SETGET
   METHOD RootPath()                   INLINE HB_DirBase() + "sessions" + hb_OsPathSeparator()  // --> cRootPath
   METHOD SessionPath()  // --> cSessionPath
   METHOD SessionFile()  // --> cSessionFile
   METHOD CacheFile( cId )  // --> cCacheFile
   METHOD SaveFile( cSource, cTarget )  // --> lValue
   METHOD IsFile( cFile )              INLINE HB_FileExists( ::SessionPath() + cFile )  // --> lValue
   METHOD IsSessionPath()              INLINE HB_DirExists( ::SessionPath() )  // --> lValue
   METHOD IsSessionFile()              INLINE HB_FileExists( ::SessionFile() )  // --> lValue
   METHOD FlushCache()
   METHOD TempFile( cExt )

RESERVED:
   METHOD New()                        CONSTRUCTOR
   METHOD End()                        VIRTUAL
   METHOD Purge( lForced )
   METHOD CheckPaths()
END CLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZSession

//   ::Purge() // ahora se hace al salir del CGI como última operación

RETURN Self

//------------------------------------------------------------------------------

METHOD SessionPath() CLASS ZSession

RETURN ::RootPath() + Engine:SessionId() + hb_OsPathSeparator()

//------------------------------------------------------------------------------

METHOD SessionFile() CLASS ZSession

RETURN ::SessionPath() + "session.json"

//------------------------------------------------------------------------------

METHOD CacheFile( cId ) CLASS ZSession

   LOCAL cFile

   cFile := ::SessionPath() + Engine:ScriptShortName() + "_" + ;
            cId + "_" + Document:cLang + ".cache"

RETURN cFile

//------------------------------------------------------------------------------

METHOD CheckPaths() CLASS ZSession

   LOCAL cRPath, cSPath

   cRPath := ::RootPath()
   cSPath := ::SessionPath()

   IF !HB_DirExists( cRPath )
      IF HB_DirCreate( cRPath ) != 0
         RETURN .F.
      ENDIF
   ENDIF

   IF !HB_DirExists( cSPath )
      IF HB_DirCreate( cSPath ) != 0
         RETURN .F.
      ENDIF
   ELSE
       HB_FSetDateTime( cSPath, Date(), Time() )
   ENDIF

RETURN .T.

//------------------------------------------------------------------------------

METHOD Save( hHash ) CLASS ZSession

   LOCAL cJson, cRPath, cSPath

   DEFAULT hHash TO {=>}

   IF !::CheckPaths()
      RETURN .F.
   ENDIF

   WITH OBJECT Engine
      HB_HSet( hHash, "session_ttl", :nSessionTTL )
   END WITH

   cJson := HB_JsonEncode( hHash )

RETURN HB_MemoWrit( ::SessionFile(), cJson )

//------------------------------------------------------------------------------

METHOD SaveFile( cSource, cTarget )  CLASS ZSession

   IF !::IsSessionPath()
      ::Save()
   ELSE
      HB_FSetDateTime( ::SessionPath(), Date(), Time() )
   ENDIF

RETURN ( HB_FCopy( cSource, ::SessionPath() + cTarget ) == 0 )

//------------------------------------------------------------------------------

METHOD Load( cFile ) CLASS ZSession

   LOCAL cJson, hHash

   DEFAULT cFile TO ::SessionFile()

   IF HB_FileExists( cFile )
      cJson := HB_MemoRead( cFile  )
      IF HB_JsonDecode(cJson, @hHash ) == 0
         hHash := NIL
      ENDIF
   ENDIF

   IF HB_IsHash( hHash ) .AND. HB_HHasKey( hHash, "socket_port" )
      Engine:SetListening( HB_HGetDef( hHash, "socket_port", 0 ) )
   ENDIF

RETURN hHash

//------------------------------------------------------------------------------

METHOD ListeningPort( nPort ) CLASS ZSession

   LOCAL hHash := ::Load()

   IF PCount() > 0
      HB_HSet( hHash, "socket_port", nPort )
      ::Save( hHash )
   ELSE
      nPort := HB_HGetDef( hHash, "socket_port", 0 )
   ENDIF

RETURN nPort

//------------------------------------------------------------------------------

METHOD Purge( lForced ) CLASS ZSession

   LOCAL hSes
   LOCAL aDirs, aDir, aFiles, aFile
   LOCAL dDate
   LOCAL cRPath, cSPath, cSFile
   LOCAL nTime, nSecs
   LOCAL lError

   DEFAULT lForced TO .F.

   cRPath := ::RootPath()
   aDirs  := Directory( cRPath, "D" )
   dDate  := Date()
   nTime  := TimeToSec( Time() )


   FOR EACH aDir IN aDirs
      cSPath := cRPath + aDir[ F_NAME ] + hb_OsPathSeparator()
      IF "D" $ aDir[ F_ATTR ] .AND. Left( aDir[ F_NAME ] , 1 ) != "."
         cSPath := cRPath + aDir[ F_NAME ] + hb_OsPathSeparator()
         hSes   := ::Load( cSPath + "session.json" )
         IF hSes != NIL .AND. !lForced
            nSecs := HB_HGetDef( hSes, "session_ttl", Engine:nSessionTTL )
         ELSE
            nSecs  := 0 // si no hay session borramos sin más
         ENDIF
         lError := .F.

         IF ( aDir[ F_DATE ] < dDate ) .OR. ( nTime - TimeToSec( aDir[ F_TIME ] ) > nSecs )
            cSPath := cRPath + aDir[ F_NAME ] + hb_OsPathSeparator()
            aFiles := Directory( cSPath + "*.*" )
            FOR EACH aFile IN aFiles
               lError := ( FErase( cSPath + aFile[ F_NAME ] ) != 0 )
               IF lError
                  LogFile( "Error en borrado de archivo " + cSPath + aFile[ F_NAME ] )
                  EXIT
               ENDIF
            NEXT
            IF !lError
               IF HB_DirDelete( cSPath ) == F_ERROR
                  LogFile( "Error de borrado de carpeta " + cSPath )
               ENDIF
            ENDIF
         ENDIF
      ELSE
         nSecs := Engine:nSessionTTL
         IF ( aDir[ F_DATE ] < dDate ) .OR. ( nTime - TimeToSec( aDir[ F_TIME ] ) > nSecs )
            FErase( cRPath + aDir[ F_NAME ] )
         ENDIF
      ENDIF
   NEXT

   WITH OBJECT Engine
      IF !:IsService() .AND. ( :lFromIDE .OR. :lCmdArgs ) .AND. ::CheckPaths()
         cSFile := ::RootPath() + :ScriptShortName() + ".json"
         FErase( cSFile )
         HB_MemoWrit( cSFile, Document:Srv_GetInnerSekeleton() )
      ENDIF
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD nSessionTTL( nSecs ) CLASS ZSession

   IF PCount() > 0 .AND. HB_IsNumeric( nSecs )
      Engine:nSessionTTL := nSecs
   ENDIF

RETURN Engine:nSessionTTL

//------------------------------------------------------------------------------

METHOD FlushCache() CLASS ZSession

   LOCAL cFile

   FOR EACH cFile IN Directory( ::SessionPath() + "*.cache" )
      FErase( cFile )
   NEXT

RETURN nil

//------------------------------------------------------------------------------

METHOD TempFile( cExt ) CLASS ZSession

   LOCAL cName
   LOCAL fFile

   DEFAULT cExt TO ".tmp"

   IF !::CheckPaths()
      RETURN ""
   ENDIF

   IF HB_ISSTRING( cExt ) .AND. !( Left( cExt, 1 ) == "." )
      cExt := "." + cExt
   ENDIF

   fFile := hb_FTempCreateEx( @cName, ::SessionPath(),, cExt )

   IF fFile!= F_ERROR
      FClose( fFile )
      RETURN cName
   ENDIF

RETURN ""

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION TimeToSec( cTime )

   LOCAL nHor, nMin, nSec

   nHor := Val( Substr( cTime, 1, 2 ) )
   nMin := Val( Substr( cTime, 4, 2 ) )
   nSec := Val( Substr( cTime, 7, 2 ) )

RETURN ( ( nHor * 3600 ) + ( nMin * 60 ) + nSec )

//------------------------------------------------------------------------------

