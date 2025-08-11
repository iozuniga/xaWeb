/*
 * Proyecto: xaWeb framework
 * Fichero: ZOdbcDatasource.prg
 * Descripción: Odbc datasource
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaweb.ch"
#include "error.ch"
#include "odbc.ch"

//------------------------------------------------------------------------------

CLASS ZOdbcDatasource
   DATA hConnect        INIT {=>}
   DATA cDSN            INIT ""
   DATA cUser           INIT ""
   DATA cPassword       INIT ""
   DATA cQuoteChar      INIT '"'
   DATA nTimeOut        INIT 0
   DATA lShowMoreInfo   INIT .T.

   DATA cConnectOut     INIT ""  READONLY
   DATA cDatabase       INIT ""  READONLY
   DATA lConnected      INIT .F. READONLY

   METHOD New()         CONSTRUCTOR

   METHOD Connect( OPTIONAL cDSN, OPTIONAL cUser, OPTIONAL cPassword )  // --> lValue
   METHOD Disconnect()

   METHOD Version() INLINE hb_odbcVer()   // --> nVersion

   METHOD Execute( cSql, BYREF nAffectedRows )  // --> lSuccess

   METHOD Query( cQuery )  INLINE ZOdbcQuery():New( Self, cQuery )  // --> WOdbcQuery
   METHOD QueryArray( cQuery, BYREF aHeader )  // --> aData
   METHOD QueryValue( cQuery, xDefault )  // --> xValue

   METHOD BeginTrans()        INLINE SqlBeginTransaction( ::hDbc ) // --> lSuccess
   METHOD CommitTrans()       INLINE SqlCommit( ::hDbc ) // --> lSuccess
   METHOD RollBackTrans()     INLINE SqlRollback( ::hDbc ) // --> lSuccess

   METHOD Catalogs()                   INLINE ::SqlTables( 1 ) // --> aCatalogs Info
   METHOD Schemas()                    INLINE ::SqlTables( 2 ) // --> aSchemas Info
   METHOD Tables( OPTIONAL cCatalog, OPTIONAL cSchema )  ;
                                       INLINE ::SqlTables( 3, cCatalog, cSchema )// --> aTables Info

   METHOD BuildSQLSt( cSql, xParams ) // --> cString

RESERVED:
   DATA hEnv                     READONLY
   DATA hDbc                     READONLY

   METHOD Destroy()           INLINE ::Disconnect()
   METHOD OperOK( nRet, cOperation, hStmt )
   METHOD NewStmt()
   METHOD SqlTables( nType, cCatalog, cSchema )

PROTECTED:

END CLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZOdbcDatasource

   LOCAL hEnv, hDbc

   IF SQLAllocEnv( @hEnv ) != SQL_SUCCESS
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "ODBC initialization error (SQLAllocEnv())"
         :Operation   := "WOdbcDatasource:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
   ENDIF

   IF SQLAllocConnect( hEnv, @hDbc ) != SQL_SUCCESS
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "ODBC initialization error (SQLAllocHandle/Connect())"
         :Operation   := "WOdbcDatasource:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN nil
   ENDIF

   ::hEnv := hEnv
   ::hDbc := hDbc

RETURN Self

//------------------------------------------------------------------------------
// Managed by GC

METHOD Disconnect() CLASS ZOdbcDatasource

   ::hDbc := NIL
   ::hEnv := NIL

RETURN nil

//------------------------------------------------------------------------------

METHOD OperOK( nRet, cOper, hStmt ) CLASS ZOdbcDatasource

   LOCAL cErrorClass, cErrorMsg
   LOCAL nError, nRet2

   HB_Default( @cOper, "???" )

   IF nRet < SQL_SUCCESS
      nRet2 := SQLError( ::hEnv, ::hDbc, hStmt, @cErrorClass, @nError, @cErrorMsg )
      IF SQL_SUCCEEDED( nRet2 )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := cErrorClass + " (" + ToString( nError ) + "): " + cErrorMsg
            :Operation   := "WOdbcDatasource:" + cOper
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ELSE
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := "SQL error (" + ToString( nRet ) + ")"
            :Operation   := "WOdbcDatasource:" + cOper
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
      RETURN .F.
   ELSEIF nRet == SQL_SUCCESS_WITH_INFO .AND. PCount() > 1 .AND. ::lShowMoreInfo
      nRet2 := SQLGetDiagRec( SQL_HANDLE_STMT, hStmt, 1, @cErrorClass, @nError, @cErrorMsg )
      IF nRet2 == SQL_SUCCESS
         LogDebug( "WOdbcDatasource:" + cOper + " -- " + cErrorClass + ;
                    " (" + ToString( nError ) + "): " + cErrorMsg )
      ENDIF
   ENDIF

RETURN .T.

//------------------------------------------------------------------------------

METHOD Connect( cDSN, cUser, cPassword ) CLASS ZOdbcDatasource

   LOCAL cConnect := "", cConnectOut, cDatabase, cQuoteChar, cKey, cVal
   LOCAL nRet

   HB_Default( @cDSN, ::cDSN )
   HB_Default( @cUser, ::cUser )
   HB_Default( @cPassword, ::cPassword )

   IF !Empty( cDSN )
      HB_HSET( ::hConnect, "DSN", cDSN )
   ENDIF
   IF !Empty( cUser )
      HB_HSET( ::hConnect, "UID", cUser )
   ENDIF
   IF !Empty( cPassword )
      HB_HSET( ::hConnect, "PWD", cPassword )
   ENDIF

   FOR EACH cKey, cVal IN HB_HKeys( ::hConnect ), HB_HValues( ::hConnect )
      cConnect += cKey + "=" + ToString( cVal ) + ";"
   NEXT

   cConnect := Left( cConnect, Len( cConnect ) - 1 )

   IF !Empty( ::nTimeOut )
      SQLSetConnectAttr( ::hDbc, SQL_LOGIN_TIMEOUT, ::nTimeOut, 0 )
   ENDIF

   nRet := SQLDriverConnect( ::hDbc, cConnect, @cConnectOut )

   IF ::OperOK( nRet, "Connect" )
      ::cConnectOut := cConnectOut
      ::lConnected  := .T.
      SQLGetInfo( ::hDbc, SQL_DATABASE_NAME, @cDatabase )
      SQLGetInfo( ::hDbc, SQL_IDENTIFIER_QUOTE_CHAR, @cQuoteChar )
      SQLGetInfo( ::hDbc, SQL_USER_NAME, @cUser )
      ::cDatabase := cDatabase
      ::cUser := cUser
      ::cQuoteChar := cQuoteChar
   ENDIF

RETURN ::lConnected

//------------------------------------------------------------------------------

METHOD NewStmt() CLASS ZOdbcDatasource

   LOCAL hStmt
   LOCAL nRet

   nRet := SQLAllocStmt( ::hDbc, @hStmt )

   IF !::OperOK( nRet, "NewStmt" )
      RETURN 0
   ENDIF

RETURN hStmt

//------------------------------------------------------------------------------

METHOD Execute( cSql, nAffectedRows )  CLASS ZOdbcDatasource

   LOCAL hStmt
   LOCAL nRet
   LOCAL lRet

   hStmt := ::NewStmt()
   lRet  := .F.

   IF !Empty( hStmt )
      nRet := SQLExecDirect( hStmt, cSql )
      IF ::OperOk( nRet, "Execute: " + cSql, hStmt )
         lRet := .T.
         IF PCount() > 1
            nAffectedRows := SQLRowCount( hStmt )
         ENDIF
      ENDIF
   ENDIF

RETURN lRet

//------------------------------------------------------------------------------

METHOD QueryArray( cQuery, aHeader ) CLASS ZOdbcDatasource

   LOCAL aData

   WITH OBJECT ::Query( cQuery )
      aHeader := :Header()
      aData   := :aData
      :Destroy()
   END WITH

RETURN aData

//------------------------------------------------------------------------------

METHOD QueryValue( cQuery, xDefault ) CLASS ZOdbcDatasource

   WITH OBJECT ::Query( cQuery )
      IF Len( :aData ) > 0 .AND. HB_IsArray( :aData[ 1 ] ) .AND. Len( :aData[ 1 ] ) > 0
         xDefault := :aData[ 1, 1 ]
      ENDIF
      :Destroy()
   END WITH

RETURN xDefault

//------------------------------------------------------------------------------

METHOD SqlTables( nType, cCatalog, cSchema ) CLASS ZOdbcDatasource

   LOCAL aData, aRow
   LOCAL xVal
   LOCAL hStmt
   LOCAL nCount, nFor, nRet

   aData := {}
   hStmt := ::NewStmt()

   SWITCH nType
   CASE 1 // catalogs
      nRet := SqlCatalogs( hStmt )
      EXIT
   CASE 2 // schemas
      nRet := SqlSchemas( hStmt )
      EXIT
   CASE 3 // tables
      nRet := SqlTables( hStmt, cCatalog, cSchema )
      EXIT
   END SWITCH

   IF SQL_SUCCEEDED( nRet )
      SQLNumResultCols( hStmt, @nCount )
      DO WHILE SQL_SUCCEEDED( SQLFetch( hStmt ) )
         aRow := Array( nCount )
         FOR nFor := 1 TO nCount
            SQLGetData( hStmt, nFor, SQL_CHAR,, @xVal )
            aRow[ nFor ] := xVal
         NEXT
         AAdd( aData, aRow )
      ENDDO
   ENDIF

RETURN aData

//------------------------------------------------------------------------------

METHOD BuildSQLSt( ... ) CLASS ZOdbcDatasource

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

CLASS ZOdbcQuery
   DATA oParent            AS CLASS WOdbcDatasource
   DATA aData     INIT {}
   DATA aStruct   INIT {}
   DATA cQuery    INIT ""    READONLY
   DATA cTable    INIT ""    READONLY
   DATA nRecno    INIT 0

   METHOD New( oParent, cQuery )    CONSTRUCTOR
   METHOD NextResult( oQuery )      CONSTRUCTOR

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
   DATA hStmt  READONLY

   METHOD Destroy()     INLINE ::hStmt := NIL, ::aData := {}, ::aStruct := {}
   METHOD FieldStruct()
   METHOD GetData()

END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cQuery ) CLASS ZOdbcQuery

   LOCAL hStmt
   LOCAL nRet

   ::oParent := oParent
   ::cQuery  := cQuery

   IF Empty( cQuery )
      RETURN Self
   ENDIF

   WITH OBJECT oParent
      hStmt := :NewStmt()
      IF !Empty( hStmt )
         nRet := SQLExecDirect( hStmt, cQuery )
         IF :OperOk( nRet, "Query: " + cQuery, hStmt )
            ::FieldStruct( hStmt )
            ::GetData( hStmt )
         ENDIF
         ::hStmt := hStmt
      ENDIF
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD NextResult( oQuery ) CLASS ZOdbcQuery

   LOCAL nRet
   LOCAL hStmt

   ::oParent := oQuery:oParent
   hStmt     := oQuery:hStmt
   nRet      := SqlMoreResults( hStmt )

   IF ::oParent:OperOk( nRet, "NextResult", hStmt )
      ::FieldStruct( hStmt )
      ::GetData( hStmt )
   ENDIF

   ::hStmt := hStmt

RETURN Self

//------------------------------------------------------------------------------

METHOD FieldStruct( hStmt ) CLASS ZOdbcQuery

   LOCAL aStruct, aField
   LOCAL cName, cTable, xHbDef
   LOCAL nCol, nFields, nRet, nNameLen, nDataType, nColSize, nDec, nNulleable, cDef
   LOCAL hDbc
   LOCAL lOk

   nRet := SQLNumResultCols( hStmt, @nFields )
   hDbc := ::oParent:hDbc
   lOk  := .T.

   IF SQL_SUCCEEDED( nRet )
      FOR nCol := 1 TO nFields
         nRet := SqlDescribeCol( hStmt, nCol, @cName, 255, @nNameLen,;
                                 @nDataType, @nColSize, @nDec, @nNulleable )
         IF !SQL_SUCCEEDED( nRet )
            lOk := .f.
            EXIT
         ENDIF

         SqlColAttribute( hStmt, nCol, SQL_DESC_BASE_TABLE_NAME, @cTable )

         aField := Array( 9 )
         aField[ ODBC_FS_NAME ]      := cName
         aField[ ODBC_FS_TABLE ]     := cTable
         aField[ ODBC_FS_TYPE ]      := nDataType
         aField[ ODBC_FS_LENGTH ]    := nColSize
         aField[ ODBC_FS_DECIMALS ]  := nDec
         aField[ ODBC_FS_NULLEABLE ] := ( nNulleable > 0 )
         aField[ ODBC_FS_HB_TYPE ]   := SqlGetHbType( nDataType, @xHbDef )
         aField[ ODBC_FS_HB_INIT ]   := xHbDef

         IF !Empty( cTable )
            IF Empty( ::cTable )
               ::cTable := aField[ ODBC_FS_TABLE ]
            ENDIF
            cDef := SqlColumnDefault( hDbc, cTable, cName )
            IF !Empty( cDef ) .AND. !( "NULL" $  cDef )
               SWITCH aField[ ODBC_FS_HB_TYPE ]
               CASE "N"
                  aField[ ODBC_FS_DEF ] := Val( cDef )
                  EXIT
               CASE "C"
                  aField[ ODBC_FS_DEF ] := cDef
                  EXIT
               CASE "L"
                  aField[ ODBC_FS_DEF ] := ( Val( cDef ) > 0 )
                  EXIT
               CASE "D"
                  aField[ ODBC_FS_DEF ] := HB_OdbcStoD( cDef )
                  EXIT
               END SWITCH
            ENDIF
         ENDIF

         AAdd( ::aStruct, aField )
      NEXT
   ELSE
      lOk := .f.
      ::aStruct := {}
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD GetData( hStmt ) CLASS ZOdbcQuery

   LOCAL aData, aRow, aStruct
   LOCAL nFor, nCount, nType, nRet
   LOCAL xVal

   aData   := {}
   aStruct := ::aStruct
   nCount  := Len( aStruct )

   DO WHILE SQL_SUCCEEDED( SQLFetch( hStmt ) )
      aRow := Array( nCount )
      FOR nFor := 1 TO nCount
         SQLGetData( hStmt, nFor, aStruct[ nFor, ODBC_FS_TYPE ],, @xVal )
         aRow[ nFor ] := xVal
      NEXT
      AAdd( aData, aRow )
   ENDDO

   ::aData := aData

RETURN aData

//------------------------------------------------------------------------------

METHOD Header()  CLASS ZOdbcQuery

   LOCAL aHeader := {}
   LOCAL aField

   FOR EACH aField IN ::aStruct
      AAdd( aHeader, aField[ ODBC_FS_NAME ] )
   NEXT

RETURN aHeader

//------------------------------------------------------------------------------

METHOD Record( nRecno ) CLASS ZOdbcQuery

   LOCAL hRec := {=>}
   LOCAL aField

   HB_Default( @nRecno, ::nRecno )

   IF nRecno > 0 .AND. nRecno <= Len( ::aData )
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ ODBC_FS_NAME ],;
                  ::aData[ nRecno, aField:__EnumIndex() ] )
      NEXT
   ELSEIF nRecno == 0 // Typed initial value. Default value if present
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ ODBC_FS_NAME ],;
                  aField[ ODBC_FS_HB_INIT ] )
      NEXT
   ELSEIF nRecno == -1 // Null values on all fields
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ ODBC_FS_NAME ], NIL )
      NEXT
   ENDIF

RETURN hRec

//------------------------------------------------------------------------------

METHOD SetNullOnDefault( hRecord ) CLASS ZOdbcQuery

   LOCAL aField
   LOCAL cKey

   FOR EACH aField IN ::aStruct
      cKey := aField[ ODBC_FS_NAME ]
      IF VarsEqual( HB_HGet( hRecord, cKey ), aField[ ODBC_FS_HB_INIT ] )
         HB_HSet( hRecord, cKey, NIL )
      ENDIF
   NEXT

RETURN hRecord

//------------------------------------------------------------------------------

METHOD SqlInsert( hRecord, cTable ) CLASS ZOdbcQuery

   LOCAL xVal
   LOCAL cSql, cKey, cQuote

   cQuote := ::cQuoteChar

   HB_Default( @cTable, ::cTable )

   cTable := cQuote + cTable + cQuote
   cSql   := "INSERT INTO " + cTable + " ("

   FOR EACH cKey, xVal IN HB_HKeys( hRecord ), HB_HValues( hRecord )
      IF !HB_IsNIL( xVal )
         cSql += cQuote + cKey + cQuote + ","
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

METHOD SqlUpdate( hRecord, cTable, hOriginal ) CLASS ZOdbcQuery

   LOCAL xVal1, xVal2
   LOCAL cSql, cKey, cQuote
   LOCAL lUpd

   HB_Default( @cTable, ::cTable )

   cQuote := ::cQuoteChar
   cTable := cQuote + cTable + cQuote
   cSql   := "UPDATE " + cTable + " SET "

   IF PCount() > 2
      lUpd := .F.
      FOR EACH cKey, xVal1, xVal2 IN  HB_HKeys( hRecord ), HB_HValues( hRecord ), HB_HValues( hOriginal )
         IF !VarsEqual( xVal1, xVal2 )
            cSql += cSql += cQuote + cKey + cQuote + "=" + ValToSql( xVal1 ) + ","
            lUpd := .T.
         ENDIF
      NEXT
      IF !lUpd
         RETURN ""
      ENDIF
   ELSE
      FOR EACH cKey, xVal1 IN  HB_HKeys( hRecord ), HB_HValues( hRecord )
         cSql += cQuote + cKey + cQuote + "=" + ValToSql( xVal1 ) + ","
      NEXT
   ENDIF

   SwapChr( @cSql, Len( cSql ), " " )

   cSql += "WHERE " // You must complete the sentence ...

RETURN cSql

//------------------------------------------------------------------------------

METHOD SqlDelete( cTable ) CLASS ZOdbcQuery

   HB_Default( @cTable, ::cTable )

   cTable := ::cQuoteChar + cTable + ::cQuoteChar


RETURN "DELETE FROM " + cTable + " WHERE "

//------------------------------------------------------------------------------

METHOD LocalInsert( hRecord, nRecno ) CLASS ZOdbcQuery

   LOCAL aRow := {}
   LOCAL xVal

   DEFAULT nRecno TO 0

   FOR EACH xVal IN HB_HValues( hRecord )
      AAdd( aRow, xVal )
   NEXT

   HB_AIns( ::aData, nRecno + 1, aRow, .T. )

RETURN aRow

//------------------------------------------------------------------------------

METHOD LocalUpdate( hRecord, nRecno ) CLASS ZOdbcQuery

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
         cVal := "'" + xVal + "'"
      ENDIF
      EXIT
   OTHERWISE
      cVal := "NULL"
   END SWITCH

RETURN cVal

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

