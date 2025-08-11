/*
 * Proyecto: xaWeb framework
 * Fichero: ZMariaDbDatasource.prg
 * Descripción: MariaDb datasource
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 *
 * Important note on Linux installations:
 *  MariaDb-devel package must be installed on your system:
 *  -> sudo apt install libmariadb3 libmariadb-dev
 */

#include "xaWeb.ch"
#include "error.ch"
#include "mysql.ch"

CLASS ZMariaDbDatasource
   DATA cHost        INIT ""
   DATA cUser        INIT ""
   DATA cPassword    INIT ""
   DATA nPort        INIT 3306
   DATA nFlags       INIT 0
   DATA nSocket      READONLY
   DATA lSslVerify   INIT .F.
   DATA lConnected   INIT .F. READONLY

   METHOD New()         CONSTRUCTOR

   METHOD Connect( cHost, cUser, cPassword, nPort, nFlags, lSsl )  // --> lValue
   METHOD Disconnect()  // --> lValue

   METHOD Version() INLINE MySql_Get_Server_Version( ::nSocket )  // --> cVersion
   METHOD cDatabase( cDb ) SETGET  // --> cDatabase

   METHOD Query( cQuery )  INLINE WMariaDbQuery():New( Self, cQuery )  // --> WMariaDbQuery
   METHOD QueryArray( cQuery, BYREF aHeader )  // --> aData
   METHOD QueryValue( cQuery, xDefault )  // --> xValue
   METHOD Execute( cSql, BYREF nAffectedRows, BYREF nInsertID )  // --> lSuccess
   METHOD NextResult()        INLINE WMariaDbQuery():New( Self, NIL )  // --> WMariaDbQuery

   METHOD BeginTrans()        INLINE ::Execute( "START TRANSACTION" ) // --> lSuccess
   METHOD CommitTrans()       INLINE ::Execute( "COMMIT" ) // --> lSuccess
   METHOD RollBackTrans()     INLINE ::Execute( "ROLLBACK" ) // --> lSuccess

   METHOD ErrorNo()           INLINE MySql_ErrNo( ::nSocket )  // --> nError
   METHOD Error()             INLINE MySql_Error( ::nSocket )  // --> cError

   METHOD Catalog()           INLINE MySql_List_Dbs( ::nSocket )  // --> aCatalogs
   METHOD Tables()            INLINE MySql_List_Tables( ::nSocket )   // --> aTables
   METHOD HostInfo()          INLINE MySql_Get_Host_Info( ::nSocket )  // --> cInfo
   METHOD ServerInfo()        INLINE MySql_Get_Server_Info( ::nSocket )  // --> cInfo
   METHOD Ping()              INLINE MySql_Ping( ::nSocket )
   METHOD EscapeString( cStr ) INLINE MySql_Real_Escape_String( ::nSocket, cStr )  // --> cString

   METHOD BuildSQLSt( cSql, xParams ) // --> cString

RESERVED:
   METHOD Destroy()     INLINE ::Disconnect()

PROTECTED:
   DATA fcDatabase    INIT ""

END CLASS

//------------------------------------------------------------------------------

METHOD New() CLASS ZMariaDbDatasource

   ::nSocket := mysql_init()

   IF Empty( ::nSocket )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "MariaDb/MySql initialization error (mysql_init())"
         :Operation   := "WMariaDbDatasource:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD Connect( cHost, cUser, cPassword, nPort, nFlags, lSsl ) CLASS ZMariaDbDatasource

   HB_Default( @cHost, ::cHost )
   HB_Default( @cUser, ::cUser )
   HB_Default( @cPassword, ::cPassword )
   HB_Default( @nPort, ::nPort )
   HB_Default( @nFlags, ::nFlags )
   HB_Default( @lSsl, ::lSslVerify )

   IF mysql_real_connect( ::nSocket, cHost, cUser, cPassword, nPort, nFlags, lSsl )
      ::lConnected := .T.
      IF !Empty( ::fcDatabase )
         ::cDatabase := ::fcDatabase
      ENDIF
   ELSE
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Connection error: " + ::Error()
         :Operation   := "WMariaDbDatasource:Connect()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

RETURN ::lConnected

//------------------------------------------------------------------------------

METHOD Disconnect() CLASS ZMariaDbDatasource

   IF ::lConnected
      mysql_close( ::nSocket )
      ::lConnected := .F.
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------

METHOD cDatabase( cDb ) CLASS ZMariaDbDatasource

   IF PCount() > 0
      ::fcDatabase := cDb
      IF ::lConnected .AND. MySql_Select_Db( ::nSocket, cDb ) != 0
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := 'Invalid Database "' + cDb +'"'
            :Operation   := "WMariaDbDatasourcec:cDatabase"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
   ENDIF

RETURN ::fcDatabase

//------------------------------------------------------------------------------

METHOD QueryArray( cQuery, aHeader ) CLASS ZMariaDbDatasource

   LOCAL aData

   WITH OBJECT ::Query( cQuery )
      aHeader := :Header()
      aData   := :aData
      :Destroy()
   END WITH

RETURN aData

//------------------------------------------------------------------------------

METHOD QueryValue( cQuery, xDefault ) CLASS ZMariaDbDatasource

   WITH OBJECT ::Query( cQuery )
      IF Len( :aData ) > 0 .AND. HB_IsArray( :aData[ 1 ] ) .AND. Len( :aData[ 1 ] ) > 0
         xDefault := :aData[ 1, 1 ]
      ENDIF
      :Destroy()
   END WITH

RETURN xDefault

//------------------------------------------------------------------------------

METHOD Execute( cSql, nAffectedRows, nInsertID ) CLASS ZMariaDbDatasource

   LOCAL nResult

   nResult := MySql_query( ::nSocket, cSql )

   IF nResult == 0
      IF PCount() > 1
         nInsertID     := MySql_Insert_Id( ::nSocket )
         nAffectedRows := MySql_Affected_Rows( ::nSocket )
      ENDIF
   ENDIF

RETURN ( nResult == 0 )

//--------------------------------------------------------------------------

METHOD BuildSQLSt( ... ) CLASS ZMariaDbDatasource

   LOCAL aParams
   LOCAL cSql
   LOCAL nFor, nLen, nDec, nLeft, nRight
   LOCAL xPar, nAt, nPos := 1, cValue

   aParams := hb_Aparams()
   nLen    := Len( aParams )

   IF nLen == 0
      RETURN ""
   ENDIF

   cSql := TrimSelect( MySql_Escape_String( aParams[ 1 ] ) )

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

   //logdebug( cSql )

RETURN cSql

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZMariaDbQuery
   DATA oParent            AS CLASS WMariaDbDatasource
   DATA aData   INIT {}
   DATA aStruct INIT {}
   DATA cQuery  INIT ""    READONLY
   DATA cTable  INIT ""    READONLY
   DATA nCursor INIT NIL   READONLY
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
   METHOD Destroy()     INLINE ::nCursor := NIL // lo gestiona GC
   METHOD FieldStruct()
   METHOD GetData()
   METHOD SetTypeOnRow( aRow )
END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent, cQuery ) CLASS ZMariaDbQuery

   LOCAL nResult

   ::oParent := oParent
   ::cQuery  := cQuery

   IF !Empty( cQuery )
      nResult := MySql_Query( oParent:nSocket, cQuery )
   ELSE
      nResult := MySql_Next_Result( oParent:nSocket )
   ENDIF

   IF nResult == 0
      IF !Empty( ::nCursor := mysql_store_result( oParent:nSocket ) )
         ::FieldStruct()
         ::GetData()
         ::nCursor := NIL
      ELSEIF !Empty( cQuery )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := "MySql_Store_Result error: Not a valid SELECT query [" + ;
                            cQuery + "]"
            :Operation   := "WMariaDbQuery:New()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
   ELSEIF !Empty( cQuery )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "MySql_Query error: " + ToString( nResult )  + ;
                         " on query: " +  cQuery
         :Operation   := "WMariaDbQuery:New()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD FieldStruct() CLASS ZMariaDbQuery

   LOCAL aStruct, aField
   LOCAL nFor, nFields

   nFields := mysql_num_fields( ::nCursor )

   FOR nFor := 1 TO nFields
      aField := mysql_fetch_field( ::nCursor )
      IF Empty( ::cTable )
         ::cTable := aField[ MYSQL_FS_TABLE ]
      ENDIF
      AAdd( aField, NIL ) // MYSQL_FS_HARBOUR_INIT
      AAdd( ::aStruct, aField )
   NEXT

RETURN nil

//------------------------------------------------------------------------------

METHOD GetData() CLASS ZMariaDbQuery

   LOCAL aData, aRow
   LOCAL nRows, nRow, nCur

   nCur    := ::nCursor
   nRows   := mysql_num_rows( nCur )
   aData   := Array( nRows )

   FOR nRow := 1 TO nRows
      mysql_data_seek( nCur, nRow - 1 )
      aRow := mysql_fetch_row( nCur )
      ::SetTypeOnRow( aRow )
      aData[ nRow ] := aRow
   NEXT

   ::aData := aData

RETURN aData

//------------------------------------------------------------------------------

METHOD SetTypeOnRow( aRow ) CLASS ZMariaDbQuery

   LOCAL aField
   LOCAL xVal, xDef
   LOCAL cDef

   FOR EACH xVal IN aRow
      aField := ::aStruct[ xVal:__enumIndex() ]
      cDef   := aField[ MYSQL_FS_DEF ]
      SWITCH aField[ MYSQL_FS_TYPE ]
      CASE MYSQL_TYPE_TINY   // BOOLEAN
      CASE MYSQL_TYPE_BIT
         xVal := !Empty( xVal )
         xDef := IIF( Empty( cDef ), .F., Val( cDef ) != 0 )
         EXIT
      CASE MYSQL_TYPE_SHORT
      CASE MYSQL_TYPE_LONG
      CASE MYSQL_TYPE_LONGLONG
      CASE MYSQL_TYPE_INT24
      CASE MYSQL_TYPE_NEWDECIMAL
      CASE MYSQL_TYPE_DOUBLE
      CASE MYSQL_TYPE_FLOAT
         IF !HB_IsNIL( xVal )
            xVal := Val( xVal )
         ENDIF
         xDef := IIF( Empty( cDef ), 0, Val( cDef ) )
         EXIT
      CASE MYSQL_TYPE_DATE
         IF !HB_IsNIL( xVal )
            xVal := HB_CToD( xVal, "YYYY-MM-DD" )
         ENDIF
         xDef := HB_CToD( cDef, "YYYY-MM-DD" )
         EXIT
      CASE MYSQL_TYPE_DATETIME
      CASE MYSQL_TYPE_TIMESTAMP
         IF !HB_IsNIL( xVal )
            xVal := HB_CToT( xVal, "YYYY-MM-DD", "hh:mm:ss" )
         ENDIF
         xDef := HB_CToT( cDef, "YYYY-MM-DD", "hh:mm:ss" )
         EXIT
      CASE MYSQL_TYPE_BLOB
      CASE MYSQL_TYPE_TINY_BLOB
      CASE MYSQL_TYPE_MEDIUM_BLOB
      CASE MYSQL_TYPE_LONG_BLOB
      CASE MYSQL_TYPE_STRING
      CASE MYSQL_TYPE_VAR_STRING
         xDef := cDef
         EXIT
      OTHERWISE
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := "Unknown type (" + ToString( aField[ MYSQL_FS_TYPE ] ) + ;
                            ") from SQL Server Field: " + aField[ MYSQL_FS_NAME ]
            :Operation   := "WMariaDbQuery:New()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDSWITCH
      aField[ MYSQL_FS_HARBOUR_INIT ] := xDef
   NEXT

RETURN aRow

//------------------------------------------------------------------------------

METHOD Header()  CLASS ZMariaDbQuery

   LOCAL aHeader := {}
   LOCAL aField

   FOR EACH aField IN ::aStruct
      AAdd( aHeader, aField[ MYSQL_FS_NAME  ] )
   NEXT

RETURN aHeader

//------------------------------------------------------------------------------

METHOD Record( nRecno ) CLASS ZMariaDbQuery

   LOCAL hRec := {=>}
   LOCAL aField

   HB_Default( @nRecno, ::nRecno )

   IF nRecno > 0 .AND. nRecno <= Len( ::aData )
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ MYSQL_FS_NAME ],;
                  ::aData[ nRecno, aField:__EnumIndex() ] )
      NEXT
   ELSEIF nRecno == 0 // Typed initial value. Default value if present
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ MYSQL_FS_NAME ],;
                  aField[ MYSQL_FS_HARBOUR_INIT ] )
      NEXT
   ELSEIF nRecno == -1 // Null values on all fields
      FOR EACH aField IN ::aStruct
         HB_HSet( hRec, aField[ MYSQL_FS_NAME ], NIL )
      NEXT
   ENDIF

RETURN hRec

//------------------------------------------------------------------------------

METHOD SetNullOnDefault( hRecord ) CLASS ZMariaDbQuery

   LOCAL aField
   LOCAL cKey

   FOR EACH aField IN ::aStruct
      cKey := aField[ MYSQL_FS_NAME ]
      IF VarsEqual( HB_HGet( hRecord, cKey ), aField[ MYSQL_FS_HARBOUR_INIT ] )
         HB_HSet( hRecord, cKey, NIL )
      ENDIF
   NEXT

RETURN hRecord

//------------------------------------------------------------------------------

METHOD SqlInsert( hRecord, cTable ) CLASS ZMariaDbQuery

   LOCAL xVal
   LOCAL aFld
   LOCAL cSql, cKey
   LOCAL nType

   HB_Default( @cTable, ::cTable )

   cTable := "`" + cTable + "`"

   cSql := "INSERT INTO " + cTable + " ("

   FOR EACH cKey, xVal IN HB_HKeys( hRecord ), HB_HValues( hRecord )
      IF !HB_IsNIL( xVal )
         cSql += "`" + cKey + "`,"
      ENDIF
   NEXT

   SwapChr( @cSql, Len( cSql ), ")" )
   cSql += " VALUES ("

   FOR EACH cKey, xVal, aFld IN HB_HKeys( hRecord ), HB_HValues( hRecord ), ::aStruct
      IF !HB_IsNIL( xVal )
         nType := aFld[ MYSQL_FS_TYPE ]
         cSql += ValToSql( xVal, ::oParent:nSocket, nType ) + ","
      ENDIF
   NEXT

   SwapChr( @cSql, Len( cSql ), ")" )

RETURN cSql

//------------------------------------------------------------------------------

METHOD SqlUpdate( hRecord, cTable, hOriginal ) CLASS ZMariaDbQuery

   LOCAL xVal1, xVal2
   LOCAL aFld
   LOCAL cSql, cKey
   LOCAL nType
   LOCAL lUpd

   HB_Default( @cTable, ::cTable )

   cTable := "`" + cTable + "`"

   cSql := "UPDATE " + cTable + " SET "

   IF PCount() > 2
      lUpd := .F.
      FOR EACH cKey, xVal1, xVal2, aFld IN  HB_HKeys( hRecord ), HB_HValues( hRecord ), HB_HValues( hOriginal ), ::aStruct
         IF !VarsEqual( xVal1, xVal2 )
            nType := aFld[ MYSQL_FS_TYPE ]
            cSql += cSql += "`" + cKey + "`=" + ;
                            ValToSql( xVal1, ::oParent:nSocket, nType ) + ","
            lUpd := .T.
         ENDIF
      NEXT
      IF !lUpd
         RETURN ""
      ENDIF
   ELSE
      FOR EACH cKey, xVal1, aFld IN  HB_HKeys( hRecord ), HB_HValues( hRecord ), ::aStruct
         nType := aFld[ MYSQL_FS_TYPE ]
         cSql += "`" + cKey + "`=" + ValToSql( xVal1, ::oParent:nSocket, nType ) + ","
      NEXT
   ENDIF

   SwapChr( @cSql, Len( cSql ), " " )

   cSql += "WHERE " // You must complete the sentence ...

RETURN cSql

//------------------------------------------------------------------------------

METHOD SqlDelete( cTable ) CLASS ZMariaDbQuery

   HB_Default( @cTable, ::cTable )

   cTable := "`" + cTable + "`"

RETURN "DELETE FROM " + cTable + " WHERE "

//------------------------------------------------------------------------------

METHOD LocalInsert( hRecord, nRecno ) CLASS ZMariaDbQuery

   LOCAL aRow := {}
   LOCAL xVal

   DEFAULT nRecno TO 0

   FOR EACH xVal IN HB_HValues( hRecord )
      AAdd( aRow, xVal )
   NEXT

   HB_AIns( ::aData, nRecno + 1, aRow, .T. )

RETURN aRow

//------------------------------------------------------------------------------

METHOD LocalUpdate( hRecord, nRecno ) CLASS ZMariaDbQuery

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

STATIC FUNCTION ValToSql( xVal, nSocket, nType )

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
      IF nType >= MYSQL_TYPE_TINY_BLOB .AND. nType <= MYSQL_TYPE_BLOB
         cVal := "UNHEX('" + HB_StrToHex( xVal ) + "')"
      ELSE
         cVal := "'" + MySql_real_Escape_String( nSocket, xVal ) + "'"
      ENDIF
      EXIT
   OTHERWISE
      cVal := "NULL"
   END SWITCH

RETURN cVal

//------------------------------------------------------------------------------

