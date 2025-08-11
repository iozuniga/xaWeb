/*
 * Proyecto: xaWeb framework
 * Fichero: ZSqliteDatasource.prg
 * Descripción: Sqlite datasource
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"
#include "hbsqlit3.ch"

CLASS ZSqliteDatasource
   DATA lCreateIfNotExist  INIT .T.
   DATA pDB                READONLY
   DATA lConnected         INIT .F. READONLY

   METHOD New()         CONSTRUCTOR

   METHOD Connect( cDatabase, lCreateIfNotExist )  // --> lValue
   METHOD Disconnect()  INLINE ::lConnected := .F., ::pDB := NIL // lo gestiona GC

   METHOD ErrorNo()     INLINE sqlite3_errcode( ::pDB )  // --> nError
   METHOD Error()       INLINE sqlite3_errmsg( ::pDB )// --> cError

   METHOD Version() INLINE Sqlite3_LibVersion()  // --> cVersion

   METHOD cDatabase( cDb ) SETGET  // --> cDatabase

   METHOD Execute( cSql, BYREF nAffectedRows, BYREF nInsertID )  // --> lSuccess
   METHOD BeginTrans()        INLINE ::Execute( "BEGIN" ) // --> lSuccess
   METHOD CommitTrans()       INLINE ::Execute( "COMMIT" ) // --> lSuccess
   METHOD RollBackTrans()     INLINE ::Execute( "ROLLBACK" ) // --> lSuccess

   METHOD Query( cQuery )  INLINE WSqliteQuery():New( Self, cQuery )  // --> WSqliteQuery
   METHOD QueryArray( cQuery, BYREF aHeader )  // --> aData
   METHOD QueryValue( cQuery, xDefault )  // --> xValue

   METHOD Tables() INLINE ::QueryArray( "SELECT name FROM sqlite_master WHERE type='table'" )  // --> aTables

   METHOD BuildSQLSt( cSql, xParams ) // --> cString

RESERVED:
   METHOD Destroy()     INLINE ::Disconnect()

PROTECTED:
   DATA fcDatabase    INIT ""

END CLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZSqliteDatasource

RETURN Self

//------------------------------------------------------------------------------

METHOD cDatabase( cDb ) CLASS ZSqliteDatasource

   IF PCount() > 0
      ::fcDatabase := cDb
      IF ::lConnected
         ::Disconnect()
         ::Connect( cDb )
      ENDIF
   ENDIF

RETURN ::fcDatabase

//------------------------------------------------------------------------------

METHOD Connect( cDatabase, lCreateIfNotExist ) CLASS ZSqliteDatasource

   HB_Default( @cDatabase, ::cDatabase )
   HB_Default( @lCreateIfNotExist, ::lCreateIfNotExist )

   ::pDB := sqlite3_open( cDatabase, lCreateIfNotExist )

   ::lConnected := ( sqlite3_errcode( ::pDb ) == SQLITE_OK )

RETURN Self

//------------------------------------------------------------------------------

METHOD Execute( cSql, BYREF nAffectedRows, BYREF nInsertID ) CLASS ZSqliteDatasource

   LOCAL lSuccess := .F.

   IF sqlite3_exec( ::pDB, cSql ) == SQLITE_OK
      lSuccess := .T.
      IF PCount() > 1
         nInsertId     := Sqlite3_Last_Insert_RowId( ::pDB )
         nAffectedRows := sqlite3_changes( ::pDB )
      ENDIF
   ENDIF

RETURN lSuccess

//------------------------------------------------------------------------------

METHOD QueryArray( cQuery, aHeader ) CLASS ZSqliteDatasource

   LOCAL aData

   WITH OBJECT ::Query( cQuery )
      aHeader := :Header()
      aData   := :aData
      :Destroy()
   END WITH

RETURN aData

//------------------------------------------------------------------------------

METHOD QueryValue( cQuery, xDefault ) CLASS ZSqliteDatasource

   WITH OBJECT ::Query( cQuery )
      IF Len( :aData ) > 0 .AND. HB_IsArray( :aData[ 1 ] ) .AND. Len( :aData[ 1 ] ) > 0
         xDefault := :aData[ 1, 1 ]
      ENDIF
      :Destroy()
   END WITH

RETURN xDefault

//--------------------------------------------------------------------------

METHOD BuildSQLSt( ... ) CLASS ZSqliteDatasource

   LOCAL aParams
   LOCAL cSql
   LOCAL nFor, nLen, nDec, nLeft, nRight
   LOCAL xPar, nAt, nPos := 1, cValue

   aParams := hb_Aparams()
   nLen    := Len( aParams )

   IF nLen == 0
      RETURN ""
   ENDIF

   cSql := TrimSelect(  aParams[ 1 ] )

   Adel( aParams, 1 )
   Asize( aParams, nLen - 1 )

   IF nLen > 1 .AND. Valtype( aParams[ 1 ] ) == "A"
      aParams := aParams[ 1 ]
   ENDIF

   nLen := Len( aParams )

   FOR nFor := 1 TO nLen
      xPar   := aParams[ nFor ]
      nLeft  := 0
      nRight := 0
      IF ( nAt := hb_At( "?", cSql, nPos ) ) == 0
         EXIT
      ENDIF
      SWITCH Valtype( xPar )
      CASE "D"
         cValue := "'" + HB_DToC( xPar, "YYYY-MM-DD" ) + "'"
         EXIT
      CASE "T"
         cValue := "'" + HB_TToC( xPar, "YYYY-MM-DD", "hh:mm:ss" ) + "'"
         EXIT
      CASE "L"
         cValue := IIF( xPar, "1", "0" )
         EXIT
      CASE "N"
         cValue := ToString( xPar )
         EXIT
      CASE "C"
      CASE "M"
         xPar := Sqlite_Escape_String( xPar )
         IF Substr( cSql, nAt - 1, 1 ) == "%"
            cValue := "'%" + xPar + "'"
            nLeft  := 1
         ELSEIF Substr( cSql, nAt + 1, 1 ) == "%"
            cValue := "'" + xPar + "%'"
            nRight := 1
         ELSE
            cValue := "'" + xPar + "'"
         ENDIF
         EXIT
      OTHERWISE
         cValue := "NULL"
      END SWITCH
      cSql := Left( cSql, nAt - 1 - nLeft ) + cValue + Substr( cSql, nAt + 1 + nRight )
      nPos := nAt + Len( cValue )
   NEXT

RETURN cSql

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZSqliteQuery
   DATA oParent            AS CLASS WSqliteDatasource
   DATA aData   INIT {}
   DATA aStruct INIT {}
   DATA cQuery  INIT ""    READONLY
   DATA cTable  INIT ""    READONLY
   DATA pStmt   INIT NIL   READONLY
   DATA nRecno  INIT 0

   METHOD New( oParent, cQuery ) CONSTRUCTOR

   METHOD RowCount()    INLINE Len( ::aData )  // --> nRows
   METHOD FieldCount()  INLINE Len( ::aStruct )  // --> nFields
   METHOD Header()  // --> aFieldNames
   METHOD Record( nRecno )  // --> hHash
   METHOD SetNullOnDefault( hRecord )  // --> hHash

   METHOD SqlInsert( hRecord, OPTIONAL cTable )  // --> cSql
   METHOD SqlUpdate( hRecord, OPTIONAL cTable, OPTIONAL hOriginal )  // --> cSql
   METHOD SqlDelete( OPTIONAL cTable ) // --> cSql

   METHOD LocalInsert( hRecord, nRecno )  // --> aRow
   METHOD LocalUpdate( hRecord, nRecno )  // --> aRow

RESERVED:
   METHOD Destroy()     INLINE ::pStmt := NIL // lo gestiona GC
   METHOD FieldStruct()
   METHOD GetData()

END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cQuery ) CLASS ZSqliteQuery

   LOCAL pStmt

   ::oParent := oParent
   ::cQuery  := cQuery

   IF Empty( cQuery )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Empty query"
         :Operation   := "WSqliteQuery:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN Self
   ENDIF

   pStmt := sqlite3_prepare( oParent:pDb, cQuery )

   IF !Empty( pStmt )
      ::FieldStruct( pStmt )
      ::GetData( pStmt )
      sqlite3_clear_bindings( pStmt )
      sqlite3_finalize( pStmt )
      pStmt := NIL
   ELSE
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "sqlite3_prepare error: " + oParent:Error()  + ;
                         " on query: " +  cQuery
         :Operation   := "WSqliteQuery:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD FieldStruct( pStmt ) CLASS ZSqliteQuery

   LOCAL aField, aMeta
   LOCAL cName, cType, cTable, cNamAs, cCollation, cHbType, cTemp
   LOCAL nFor, nAt, nFields, nType, nMax, nDec
   LOCAL lNotNullExist, lIsPK, lAutoInc
   LOCAL xDef

   IF sqlite3_step( pStmt ) != SQLITE_ROW
      RETURN NIL
   ENDIF

   nFields := sqlite3_column_count( pStmt )

   FOR nFor := 1 TO nFields
      cName  := sqlite3_column_name( pStmt, nFor )
      nType  := sqlite3_column_type( pStmt, nFor ) // integer, float, text, blob, null
      cType  := sqlite3_column_decltype( pStmt, nFor ) // DDL type
      cNamAs := sqlite3_column_origin_name( pStmt, nFor ) // Id name AS
      cTable := sqlite3_column_table_name( pStmt, nFor ) // table  name

      cCollation    := ""
      lNotNullExist := .F.
      lIsPK         := .F.
      lAutoInc      := .F.

      IF !Empty( cTable )
         IF Empty( ::cTable )
            ::cTable := cTable
         ENDIF
         aMeta := sqlite3_table_column_metadata( ::oParent:pDb,, cTable, cName )
         IF Len( aMeta ) >= 5
            cType         := aMeta[ 1 ]
            cCollation    := aMeta[ 2 ]
            lNotNullExist := aMeta[ 3 ]
            lIsPK         := aMeta[ 4 ]
            lAutoInc      := aMeta[ 5 ]
         ENDIF
      ENDIF

      nAt := At( "(", cType )

      IF nAt > 0
         cTemp := SubStr( cType, nAt + 1 )
         nMax  := Val( cTemp )
         nAt   := At( ",", cTemp )
         IF nAt > 0
            nDec := Val( SubStr( cTemp, nAt + 1 ) )
         ELSE
            nDec := 0
         ENDIF
      ELSE
         nMax := 0
         nDec := 0
      ENDIF

      cHbType := FType( cType, @xDef )
      aField  := { cName, nType, cType, nMax, nDec, cTable, cNamAs, cCollation,;
                   lNotNullExist, lIsPK, lAutoInc, cHbType, xDef }
      AAdd( ::aStruct, aField )
   NEXT

   sqlite3_reset( pStmt )

RETURN nil

//------------------------------------------------------------------------------

METHOD GetData( pStmt ) CLASS ZSqliteQuery

   LOCAL aData, aRow, aStruct
   LOCAL nFor, nCount, nType
   LOCAL xVal

   aData   := {}
   aStruct := ::aStruct
   nCount  := Len( aStruct )

   DO WHILE sqlite3_step( pStmt ) == SQLITE_ROW
      aRow := Array( nCount )
      FOR nFor := 1 TO nCount
         SWITCH aStruct[ nFor, SQLITE_FS_BASIC_TYPE ]
         CASE SQLITE_INTEGER
            xVal := sqlite3_column_int64( pStmt, nFor )
            IF aStruct[ nFor, SQLITE_FS_HB_TYPE ] == "L"
               xVal := !Empty( xVal )
            ENDIF
            EXIT
         CASE SQLITE_FLOAT
            xVal := sqlite3_column_double( pStmt, nFor )
            EXIT
         CASE SQLITE_TEXT
            xVal := sqlite3_column_text( pStmt, nFor )
            SWITCH aStruct[ nFor, SQLITE_FS_HB_TYPE ]
            CASE "T"
               xVal := HB_CToT( xVal, "YYYY-MM-DD", "hh:mm:ss" )
               EXIT
            CASE "D"
               xVal := HB_CToD( xVal, "YYYY-MM-DD" )
               EXIT
            END SWITCH
            EXIT
         CASE SQLITE_BLOB
            xVal := sqlite3_column_blob( pStmt, nFor )
            EXIT
         OTHERWISE
            xVal := NIL
         END SWITCH
         aRow[ nFor ] := xVal
      NEXT
      AAdd( aData, aRow )
   ENDDO

   ::aData := aData

RETURN aData

//------------------------------------------------------------------------------

METHOD Header()  CLASS ZSqliteQuery

   LOCAL aHeader := {}
   LOCAL aField

   FOR EACH aField IN ::aStruct
      AAdd( aHeader, aField[ SQLITE_FS_NAME ] )
   NEXT

RETURN aHeader

//------------------------------------------------------------------------------

METHOD SqlInsert( hRecord, cTable ) CLASS ZSqliteQuery

   LOCAL xVal
   LOCAL cSql, cKey

   HB_Default( @cTable, ::cTable )

   cTable := "[" + cTable + "]"
   cSql   := "INSERT INTO " + cTable + " ("

   FOR EACH cKey, xVal IN HB_HKeys( hRecord ), HB_HValues( hRecord )
      IF !HB_IsNIL( xVal )
         cSql += "[" + cKey + "],"
      ENDIF
   NEXT

   SwapChr( @cSql, Len( cSql ), ")" )
   cSql += " VALUES ("

   FOR EACH cKey, xVal IN HB_HKeys( hRecord ), HB_HValues( hRecord )
      IF !HB_IsNIL( xVal )
         cSql += ValToSql( xVal ) + ","
      ENDIF
   NEXT

   SwapChr( @cSql, Len( cSql ), ")" )

RETURN cSql

//------------------------------------------------------------------------------

METHOD SqlUpdate( hRecord, cTable, hOriginal ) CLASS ZSqliteQuery

   LOCAL xVal1, xVal2
   LOCAL cSql, cKey
   LOCAL lUpd

   HB_Default( @cTable, ::cTable )

   cTable := "[" + cTable + "]"
   cSql   := "UPDATE " + cTable + " SET "

   IF PCount() > 2
      lUpd := .F.
      FOR EACH cKey, xVal1, xVal2 IN  HB_HKeys( hRecord ), HB_HValues( hRecord ), HB_HValues( hOriginal )
         IF !VarsEqual( xVal1, xVal2 )
            cSql += cSql += "[" + cKey + "]=" + ValToSql( xVal1 ) + ","
            lUpd := .T.
         ENDIF
      NEXT
      IF !lUpd
         RETURN ""
      ENDIF
   ELSE
      FOR EACH cKey, xVal1 IN  HB_HKeys( hRecord ), HB_HValues( hRecord )
         cSql += "[" + cKey + "]=" + ValToSql( xVal1 ) + ","
      NEXT
   ENDIF

   SwapChr( @cSql, Len( cSql ), " " )

   cSql += "WHERE " // You must complete the sentence ...

RETURN cSql

//------------------------------------------------------------------------------

METHOD SqlDelete( cTable ) CLASS ZSqliteQuery

   HB_Default( @cTable, ::cTable )

   cTable := "[" + cTable + "]"


RETURN "DELETE FROM " + cTable + " WHERE "

//------------------------------------------------------------------------------

METHOD Record( nRecno ) CLASS ZSqliteQuery

   LOCAL hRec := {=>}
   LOCAL aField

   HB_Default( @nRecno, ::nRecno )

   IF nRecno > 0 .AND. nRecno <= Len( ::aData )
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ SQLITE_FS_NAME ],;
                  ::aData[ nRecno, aField:__EnumIndex() ] )
      NEXT
   ELSEIF nRecno == 0 // Typed initial value. Default value if present
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ SQLITE_FS_NAME ],;
                  aField[ SQLITE_FS_HARBOUR_INIT ] )
      NEXT
   ELSEIF nRecno == -1 // Null values on all fields
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ SQLITE_FS_NAME ], NIL )
      NEXT
   ENDIF

RETURN hRec

//------------------------------------------------------------------------------

METHOD SetNullOnDefault( hRecord ) CLASS ZSqliteQuery

   LOCAL aField
   LOCAL cKey

   FOR EACH aField IN ::aStruct
      cKey := aField[ SQLITE_FS_NAME ]
      IF VarsEqual( HB_HGet( hRecord, cKey ), aField[ SQLITE_FS_HARBOUR_INIT ] )
         HB_HSet( hRecord, cKey, NIL )
      ENDIF
   NEXT

RETURN hRecord

//------------------------------------------------------------------------------

METHOD LocalInsert( hRecord, nRecno ) CLASS ZSqliteQuery

   LOCAL aRow := {}
   LOCAL xVal

   DEFAULT nRecno TO 0

   FOR EACH xVal IN HB_HValues( hRecord )
      AAdd( aRow, xVal )
   NEXT

   HB_AIns( ::aData, nRecno + 1, aRow, .T. )

RETURN aRow

//------------------------------------------------------------------------------

METHOD LocalUpdate( hRecord, nRecno ) CLASS ZSqliteQuery

   LOCAL aRow := {}
   LOCAL xVal

   IF nRecno > 0 .AND. nRecno <= ::RowCount()
      FOR EACH xVal IN HB_HValues( hRecord )
         AAdd( aRow, xVal )
      NEXT
      ::aData[ nRecno ] := aRow
   ENDIF

RETURN aRow

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION FType( cType, xDef )

   LOCAL cHbType

   cType := Lower( cType )

   IF hb_LeftEq( cType, "int" )
      cHbType := "N"
      xDef    := 0
   ELSEIF hb_LeftEq( cType, "decimal" ) .OR. ;
          hb_LeftEq( cType, "currency" ) .OR. ;
          hb_LeftEq( cType, "double" )
      cHbType := "N"
      xDef    := 0.0
   ELSEIF hb_LeftEq( cType, "datetime" ) .OR. ;
          hb_LeftEq( cType, "timestamp" )
      cHbType := "T"
      xDef    := HB_CToT( "" )
   ELSEIF hb_LeftEq( cType, "date" )
      cHbType := "D"
      xDef    := HB_CToD( "" )
   ELSEIF hb_LeftEq( cType, "bool" ) .OR. ;
          hb_LeftEq( cType, "logical" )
      cHbType := "L"
      xDef    := .F.
   ELSEIF hb_LeftEq( cType, "blob" ) .OR. ;
          hb_LeftEq( cType, "memo" )
      cHbType := "M"
      xDef    := ""
   ELSE
      cHbType := "C"
      xDef    := ""
   ENDIF

RETURN cHbType

//------------------------------------------------------------------------------

STATIC FUNCTION TrimSelect( cSelect )

   LOCAL cRet, cChar, cLast
   LOCAL lInString

   cRet      := ""
   cLast     := " "
   lInString := .F.

   FOR EACH cChar IN cSelect
      SWITCH cChar
      CASE " "
         IF cLast != " " .OR. lInString
            cRet += cChar
         ENDIF
         EXIT
      CASE "'"
      CASE '"'
         lInString := !lInString
         cRet += cChar
         EXIT
      OTHERWISE
         cRet += cChar
      END SWITCH
      cLast := cChar
   NEXT

RETURN cRet

//------------------------------------------------------------------------------

STATIC FUNCTION Sqlite_Escape_String( cString )

RETURN StrTran( cString, "'", "''" )

//------------------------------------------------------------------------------

STATIC FUNCTION ValToSql( xVal )

   LOCAL cVal

   SWITCH ValType( xVal )
   CASE "D"
      cVal := "'" + hb_dtoc( xVal, "YYYY-MM-DD" ) + "'"
      EXIT
   CASE "T"
      cVal := "'" + HB_TToC( xVal, "YYYY-MM-DD", "hh:mm:ss" ) + "'"
      EXIT
   CASE "L"
      cVal := IIF( xVal, "1", "0" )
      EXIT
   CASE "N"
      cVal := ToString( xVal )
      EXIT
   CASE "C"
      IF StringComplex( xVal )
         cVal := "X'" + HB_StrToHex( xVal ) + "'"
      ELSE
         cVal := "'" + Sqlite_Escape_String( xVal ) + "'"
      ENDIF
      EXIT
   OTHERWISE
      cVal := "NULL"
   END SWITCH

RETURN cVal

//------------------------------------------------------------------------------

#pragma BEGINDUMP

#include "hbapiitm.h"
#include <hbapierr.h>

HB_FUNC_STATIC( STRINGCOMPLEX ) {

   const char * pString = hb_parc( 1 );
   HB_BOOL lRet = HB_FALSE;

   do
      {
      if( ( *pString < 48 ) && ( *pString > 125 ) )
         {
         lRet = HB_TRUE;
         break;
         }
      }
   while( *pString++ );

   hb_retl( lRet );
}

#pragma ENDDUMP

//------------------------------------------------------------------------------


