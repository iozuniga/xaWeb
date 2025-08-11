/*
 * Proyecto: xaWebHelp
 * Fichero: TBasicGen.prg
 * Descripcion: Parser de PRGs & C para Help de xaWeb
 * Autor: Ignacio ortiz de Zuniga
 */

#include "xa-materialize.ch"
#include "Fileio.ch"
#include "codehelper.ch"

#define CRLF   Chr(13)+Chr(10) // utilizamos CRLF en lectura de fichero porque se creo en Windows

//------------------------------------------------------------------------------

CLASS TBasicGen

   DATA oParent
   DATA hClasses INIT {=>}

   METHOD New( oParent )
   METHOD End()
   METHOD LoadFile( cFile )
   METHOD GetClasses()
   METHOD GetSymbols( aSymbols, lDeep )
   METHOD FindObject( cClass, cSymbol )

RESERVED:
   METHOD HelpClass( cClass )

END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS TBasicGen

   ::oParent := oParent

   HB_HCaseMatch( ::hClasses, .F. )

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS TBasicGen

RETURN Self

//------------------------------------------------------------------------------

METHOD HelpClass( cClass )  CLASS TBasicGen

   LOCAL oHelp

   IF HB_HHasKey( ::hClasses, cClass )
      oHelp := HB_HGet( ::hClasses, cClass )
   ELSE
      oHelp := THelpClass():New( cClass )
      HB_HSet( ::hClasses, cClass, oHelp )
   ENDIF

RETURN oHelp

//------------------------------------------------------------------------------

METHOD GetClasses() CLASS TBasicGen

   LOCAL aClasses := {}
   LOCAL oClass
   LOCAL cValue

   FOR EACH oClass IN HB_HValues( ::hClasses )
      cValue := oClass:cName
      IF Left( cValue, 1 ) == "Z"
         SwapChr(  @cValue, 1, "W" )
      ENDIF
      AAdd( aClasses, cValue )
   NEXT

RETURN aClasses

//------------------------------------------------------------------------------

METHOD GetSymbols( aSymbols, lDeep ) CLASS TBasicGen

   LOCAL cKey, oObj

   DEFAULT aSymbols TO {}, lDeep TO .T.

   IF lDeep
      FOR EACH cKey, oObj IN HB_HKeys( ::hClasses ), HB_HValues( ::hClasses )
         IF Left( cKey, 1 ) == "Z" // No incluimos XA_Functions aposta
            SwapChr( @cKey, 1, "W" )
            AAdd( aSymbols, { cKey , oObj } )
         ENDIF
         oObj:GetSymbols( @aSymbols )
      NEXT
   ELSE
      FOR EACH cKey, oObj IN HB_HKeys( ::hClasses ), HB_HValues( ::hClasses )
         IF Left( cKey, 1 ) == "Z"
            SwapChr( @cKey, 1, "W" )
         ENDIF
         AAdd( aSymbols, { cKey, oObj } )
      NEXT
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------

METHOD FindObject( cClass, cSymbol )  CLASS TBasicGen

   LOCAL oClass, oReturn

   IF Left( cClass, 1 ) == "W"
      SwapChr( @cClass, 1, "Z" )
   ENDIF

   IF HB_HHasKey( ::hClasses, cClass )
      oClass := HB_HGet( ::hClasses, cClass )
      IF Empty( cSymbol )
         oReturn := oClass
      ELSE
         oReturn := HB_HGetDef( oClass:hSymbols, cSymbol, NIL )
      ENDIF
   ENDIF

RETURN oReturn

//------------------------------------------------------------------------------

METHOD LoadFile( cFile ) CLASS TBasicGen

   LOCAL oHelp, oItem
   LOCAL aLine
   LOCAL cText, cLine, cSymbol, cClass
   LOCAL nAt := 1, nOpt

   IF !HB_FileExists( cFile )
      RETURN .F.
   ENDIF

   cText := HB_MemoRead( cFile )

   DO WHILE nAt > 0
      cLine := ReadLine( cText, @nAt, CRLF )
      aLine := HB_ATokens( cLine, ";", .F. )
      IF Len( aLine ) >= 8
         SWITCH Val( aLine[ chTYPE ] )
         CASE chCLASSDEC
            cSymbol := aLine[ chCLASS ]
            IF cSymbol = "Z" .OR. cSymbol = "z"
               WITH OBJECT oHelp := ::HelpClass( cSymbol )
                  :cParent := aLine[ chRETURN ]
                  :cFile := aLine[ chFILE ]
               END WITH
            ENDIF
            EXIT
         CASE chDATA
            cSymbol := aLine[ chSYMBOL ]
            cClass  := aLine[ chCLASS ]
            IF HB_HHasKey( ::hClasses, cClass )
               oHelp := HB_HGet( ::hClasses, cClass )
               WITH OBJECT oHelp:AddData( cSymbol )
                  nOpt :=  Val( aLine[ chRETURN ] )
                  :SetDataInfo( aLine[ chVALTYPE ] )
                  :cFile := aLine[ chFILE ]
                  :lPersistent := (nOpt) == chPERSISTENT
                  :lReadOnly := IS_READONLY( nOpt )
               END WITH
            ENDIF
            EXIT
         CASE chMETHOD
            cSymbol := aLine[ chSYMBOL ]
            cClass  := aLine[ chCLASS ]
            IF HB_HHasKey( ::hClasses, cClass )
               oHelp := HB_HGet( ::hClasses, cClass )
               WITH OBJECT oHelp:AddMethod( cSymbol )
                  :SetMethodInfo( aLine[ chPARAMS ], aLine[ chRETURN ]  )
                  :cFile := aLine[ chFILE ]
               ENDWITH
            ENDIF
            EXIT
         CASE chEVENT
            cSymbol := aLine[ chSYMBOL ]
            cClass  := aLine[ chCLASS ]
            IF HB_HHasKey( ::hClasses, cClass )
               oHelp := HB_HGet( ::hClasses, cClass )
               WITH OBJECT oHelp:AddEvent( cSymbol )
                  :SetMethodInfo( aLine[ chPARAMS ], aLine[ chRETURN ]  )
                  :cFile := aLine[ chFILE ]
               ENDWITH
            ENDIF
            EXIT
         CASE chPRGFUNC // Simulamos una clase xa_Functions, con todas las funciones
            cSymbol := aLine[ chSYMBOL ]
            cClass  := aLine[ chCLASS ]
            WITH OBJECT oHelp := ::HelpClass( cClass )
               :cFile := aLine[ chFILE ]
            END WITH
            WITH OBJECT oHelp:AddMethod( cSymbol )
               :SetMethodInfo( aLine[ chPARAMS ], aLine[ chRETURN ]  )
               :cFile := aLine[ chFILE ]
            END WITH
            EXIT
         END SWITCH
      ENDIF
   ENDDO

RETURN .T.

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS THelpClass STATIC
   DATA cName        INIT ""
   DATA cFile        INIT ""
   DATA cParent      INIT ""
   DATA hSymbols

   METHOD New( oParent )
   METHOD AddData( cData )
   METHOD AddMethod( cMethod )
   METHOD AddEvent( cEvent )
   METHOD GetSymbols( aSymbols, nType, lParent )
   METHOD HtmlInfo( oDiv )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cSymbol ) CLASS THelpClass

   ::cName := cSymbol
   ::hSymbols := HB_Hash()
   HB_HCaseMatch( ::hSymbols, .F. )

RETURN Self

//------------------------------------------------------------------------------

METHOD AddMethod( cMethod ) CLASS THelpClass

   LOCAL oObj

   cMethod := AllTrim( cMethod )

   WITH OBJECT oObj := THelpObject():New( Self )
      :nType := chMETHOD
      :cName := cMethod
   END WITH

   HB_HSet( ::hSymbols, cMethod, oObj )

RETURN oObj

//------------------------------------------------------------------------------

METHOD AddData( cData ) CLASS THelpClass

   LOCAL oObj

   WITH OBJECT oObj := THelpObject():New( Self )
      :nType := chDATA
      :cName := cData
   END WITH

   HB_HSet( ::hSymbols, cData, oObj )

RETURN oObj

//------------------------------------------------------------------------------

METHOD AddEvent( cEvent ) CLASS THelpClass

   LOCAL oObj

   WITH OBJECT oObj := THelpObject():New( Self )
      :nType := chEVENT
      :cName := cEvent
   END WITH

   HB_HSet( ::hSymbols, cEvent, oObj )

RETURN oObj

//------------------------------------------------------------------------------

METHOD GetSymbols( aSymbols, nType, lParent ) CLASS THelpClass

   LOCAL oObj AS CLASS THelpObject
   LOCAL cKey, cParent

   DEFAULT aSymbols TO {}, nType TO chALL, lParent TO .t.

   FOR EACH cKey, oObj IN HB_HKeys( ::hSymbols ), HB_HValues( ::hSymbols )
      IF ( nType == chALL ) .OR. oObj:nType == nType
         cParent := oObj:oParent:cName
         IF Left( cParent, 1 ) == "Z"
            SwapChr( @cParent, 1, "W" )
         ENDIF
         AAdd( aSymbols, { cKey + IIF( lParent, " [" + cParent + "]", "" ), oObj } )
      ENDIF
   NEXT

RETURN nil

//------------------------------------------------------------------------------

METHOD HtmlInfo( oDiv ) CLASS THelpClass

   LOCAL oTask, oObj AS CLASS THelpObject
   LOCAL aTitles, aSymbols, aSymbol
   LOCAL cHtml, cName, cKey, cAncestor, cFile, cSymbol
   LOCAL lTranslate

   cName := ::cName

   IF Left( cName, 1 ) == "Z"
      SwapChr( @cName, 1, "W" )
   ENDIF

   IF cName = "XA_"
      WITH OBJECT oDiv
         ECHO "<h5>" + cName + "</h5>" + hb_eol()
         WITH OBJECT WList():New( SO )
            :oStyle:Line_height := "2em"
            FOR EACH cKey, oObj IN HB_HKeys( ::hSymbols ), HB_HValues( ::hSymbols )
               oTask := Document:Action( "Act_Help" )
               oTask:AddParam( "symbol", cKey + " [" + ::cName + "]")
               :AddItem( cKey, oTask )
            NEXT
         END WITH
      END WITH
      RETURN nil
   ENDIF

   aTitles := { "Datas", "Methods", "Events" }

   IF ( lTranslate := Document:NeedTranslate() )
      aTitles := { "Datas", "Métodos", "Eventos" }
      cAncestor := "Hereda de"
      cFile := "Fichero"
   ELSE
      aTitles := { "Datas", "Methods", "Events" }
      cAncestor := "Inherits from"
      cFile := "File"
   ENDIF

   oTask := Document:Action( "Act_Help" )
   oTask:AddParam( "symbol", "" )

   WITH OBJECT oDiv
      WITH OBJECT WParagraph():New( SO )
         :cText := "<h4>" + cName + "</h4>"
      END WITH

      WITH OBJECT WCollapsible():New( SO )
         :cId := "classInfo"
         aSymbols := {}
         ::GetSymbols( aSymbols, chDATA )
         WITH OBJECT :AddPanel( aTitles[ 1 ], "storage" ):oBody
            WITH OBJECT WList():New( SO )
               :oStyle:padding := "16px 10px"
               :oStyle:Line_height := "2em"
               :oStyle:Font_size := "1.2em"
               FOR EACH aSymbol IN aSymbols
                  cSymbol := aSymbol[ 1 ]
                  oTask:SetParam( 1, cSymbol )
                  cSymbol := Left( cSymbol, At( "[", cSymbol ) - 2 )
                  :AddItem( cSymbol, oTask:Html() )
               NEXT
            END WITH
         END WITH

         aSymbols := {}
         ::GetSymbols( aSymbols, chMETHOD )
         WITH OBJECT :AddPanel( aTitles[ 2 ], "functions" ):oBody
            WITH OBJECT WList():New( SO )
               :oStyle:padding := "16px 10px"
               :oStyle:Line_height := "2em"
               :oStyle:Font_size := "1.2em"
               FOR EACH aSymbol IN aSymbols
                  cSymbol := aSymbol[ 1 ]
                  oTask:SetParam( 1, cSymbol )
                  cSymbol := Left( cSymbol, At( "[", cSymbol ) - 2 )
                  :AddItem( cSymbol, oTask:Html() )
               NEXT
            END WITH
         END WITH

         aSymbols := {}
         ::GetSymbols( aSymbols, chEVENT )
         WITH OBJECT :AddPanel( aTitles[ 3 ], "event" ):oBody
            WITH OBJECT WList():New( SO )
               :oStyle:padding := "16px 10px"
               :oStyle:Line_height := "2em"
               :oStyle:Font_size := "1.2em"
               FOR EACH aSymbol IN aSymbols
                  cSymbol := aSymbol[ 1 ]
                  oTask:SetParam( 1, cSymbol )
                  cSymbol := Left( cSymbol, At( "[", cSymbol ) - 2 )
                  :AddItem( cSymbol, oTask:Html() )
               NEXT
            END WITH
         END WITH
      END WITH

      WITH OBJECT WCollection():New( SO )
         WITH OBJECT :AddItem()
            :oStyle:Padding := "0px 20px"
            :oStyle:Display := "flex"
            :oStyle:Justify_content := "space-between"
            WITH OBJECT WParagraph():New( SO )
               :AddClass( "col" )
               IF Empty ( ::cParent )
                  :cText := cAncestor + ": <b>" + IIF( lTranslate, "Nadie", "None" ) + "</b>"
               ELSE
                  :cText := cAncestor + ": "
                  oTask := Document:Action( "Act_Help" )
                  oTask:AddParam( "symbol", ::cParent )
                  WITH OBJECT WLink():New( SO )
                     :cText := ::cParent
                     :cHRef := oTask
                  END WITH
               ENDIF
            END WITH
            WITH OBJECT WParagraph():New( SO )
               :AddClass( "col" )
               :cText := cFile + ": "
               WITH OBJECT WSpan():New( SO )
                  :oContext:TextColor( "grey" )
                  :cText := ::cFile
               END WITH
            END WITH
         END WITH
      END WITH

      WITH OBJECT WParagraph():New( SO )
         :cText := LOREM_IPSUM
      END WITH

   END WITH

RETURN nil

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS THelpObject STATIC
   DATA oParent
   DATA aValues      INIT {}
   DATA aParams      INIT {} // { {cName, cType, cOptions}, ... }
   DATA cName        INIT ""
   DATA cType        INIT ""
   DATA cInit        INIT NIL
   DATA cReturn      INIT "NIL"
   DATA cFile        INIT ""
   DATA nType        INIT 0
   DATA lPersistent  INIT .F.
   DATA lReadOnly    INIT .F.
   DATA lOptional    INIT .F.
   DATA lByRef       INIT .F.

   METHOD New( oParent )
   METHOD SetDataInfo( cInfo )
   METHOD SetMethodInfo( cInfo, cReturn )
   METHOD GetParamInfo( nParam )
   METHOD GetDataTypeInfo()
   METHOD GetSymbolType()
   METHOD HtmlInfo( oDiv )
   METHOD HtmlInfoData( oDiv )
   METHOD HtmlInfoFunc( oDiv )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS THelpObject

   ::oParent := oParent

RETURN Self

//------------------------------------------------------------------------------

METHOD SetDataInfo( cInfo ) CLASS THelpObject

   LOCAL aValues
   LOCAL cInit, cBeg, cEnd
   LOCAL nAt1, nAt2

   IF Empty( cInfo )
      RETURN nil
   ENDIF

   nAt1 := At( "~", cInfo )
   nAt2 := At( "?", cInfo )

   IF nAt2 > nAt1 .AND. nAt1 > 0
      cInit := SubStr( cInfo, nAt1 + 1, nAt2 - nAt1 - 1 )
      cBeg  := Left( cInit, 1 )
      cEnd  := Right( cInit, 1 )
      IF ( nAt2 - nAt1 == 2 ) .AND. ( ( cBeg == '"' .AND. cEnd != '"' ) .OR. ( cBeg == "{" .AND. cEnd != "}" ) )
         cInit := NIL
      ENDIF
      ::cInit := cInit
      cInfo := SubStr(cInfo, nAt2 + 1 )
   ENDIF

   IF Empty( cInfo )
      RETURN nil
   ENDIF

   IF At( ",", cInfo ) == 0
      IF cInfo = "W"
         ::cType := cInfo
      ENDIF
   ELSE
      aValues := HB_ATokens( cInfo, ",", .T. )
      IF Len( aValues ) > 1
         ::aValues := aValues
      ENDIF
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD SetMethodInfo( cInfo ,cReturn ) CLASS THelpObject

   LOCAL aData
   LOCAL cParam

   IF !Empty( cReturn )
      ::cReturn := cReturn
   ENDIF

   IF Empty( cInfo )
      RETURN nil
   ENDIF

   FOR EACH cParam IN HB_ATokens( cInfo, "#" )
      IF !Empty( cParam )
         aData := HB_ATokens( cParam, ":" )
         AAdd( ::aParams, aData )
      ENDIF
   NEXT

RETURN nil

//------------------------------------------------------------------------------

METHOD GetParamInfo( nParam ) CLASS THelpObject

   LOCAL aParam
   LOCAL cInfo := ""
   LOCAL nOpt

   IF !Empty( nParam ) .AND. nParam <= Len( ::aParams )
      aParam := ::aParams[ nParam ]
      IF Len( aParam ) == 3
         cInfo := " " + aParam[ 1 ]
         IF !Empty( aParam[ 2 ] )
            cInfo += ",type:" + aParam[ 2 ]
         ENDIF
         nOpt := Val( aParam[ 3 ] )
         IF !Empty( nOpt )
            cInfo += ",opt:" + IIF( IS_BYREF( nOpt ), "BYREF ", "" ) + ;
                     IIF( IS_OPTIONAL( nOpt ), "OPTIONAL", "" )
         ENDIF
      ENDIF
   ELSE
      FOR EACH aParam IN ::aParams
         IF Len( aParam ) == 3
            cInfo += " " + aParam[ 1 ]
            IF !Empty( aParam[ 2 ] )
               cInfo += ",type:" + aParam[ 2 ]
            ENDIF
            nOpt := Val( aParam[ 3 ] )
            IF !Empty( nOpt )
               cInfo += ",opt:" + IIF( IS_BYREF( nOpt ), "BYREF ", "" ) + ;
                        IIF( IS_OPTIONAL( nOpt ), "OPTIONAL", "" )
            ENDIF
            cInfo += hb_eol()
         ENDIF
      NEXT
   ENDIF

RETURN cInfo

//------------------------------------------------------------------------------

METHOD GetDataTypeInfo() CLASS THelpObject

   LOCAL cInfo

   IF ::lPersistent
      cInfo := "PERSISTENT"
   ELSE
      cInfo := ::cType
   ENDIF

RETURN cInfo

//------------------------------------------------------------------------------

METHOD GetSymbolType( lWithParent )  CLASS THelpObject

   LOCAL cType

   DEFAULT lWithParent TO .T.

   IF ::oParent:cName = "XA_"
      RETURN "Function"
   ENDIF

   IF lWithParent
      cType := ::oParent:cName + " "
   ELSE
      cType := ""
   ENDIF

   SWITCH ::nType
   CASE chDATA
      cType += "data"
      EXIT
   CASE chMETHOD
      cType += "method"
      EXIT
   CASE chEVENT
      cType += "event"
      EXIT
   OTHERWISE
      cType := "??"
   END SWITCH

RETURN cType

//------------------------------------------------------------------------------

METHOD HtmlInfo( oDiv ) CLASS THelpObject

   LOCAL oTask
   LOCAL aParam
   LOCAL cHtml, cInfo, cParent, cName, cType
   LOCAL nOpt

   SWITCH ::nType
   CASE chDATA
      ::HtmlInfoData( oDiv )
      EXIT
   CASE chMETHOD
      ::HtmlInfoFunc( oDiv, chMETHOD )
      EXIT
   CASE chEVENT
      ::HtmlInfoFunc( oDiv, chEVENT )
      EXIT
   CASE chPRGFUNC
      ::HtmlInfoFunc( oDiv, chPRGFUNC )
      EXIT
   END SWITCH

   RETURN nil

RETURN nil

//------------------------------------------------------------------------------

METHOD HtmlInfoData( oDiv ) CLASS THelpObject

   LOCAL oTask, oTask2
   LOCAL aValues
   LOCAL cParent, cName, cType, cInit, cFirst, cValues
   LOCAL lTrans, lLink

   cParent := ::oParent:cName
   cName   := ::cName
   cType   := ::cType
   cInit   := ::cInit
   cFirst  := Left( cName, 1 )
   lTrans  := Document:NeedTranslate()
   lLink   := .F.

   IF Empty( cType )
      SWITCH cFirst
      CASE "c"
         cType := IIF( lTrans, "Carácter", "Character" )
         EXIT
      CASE "n"
         cType := IIF( lTrans, "Numérico", "Numeric" )
         EXIT
      CASE "d"
         cType := IIF( lTrans, "Fecha", "Date" )
         EXIT
      CASE "t"
         cType := IIF( lTrans, "Fecha-hora", "Date-time" )
         EXIT
      CASE "l"
         cType := IIF( lTrans, "Lógico", "Logical" )
         EXIT
      CASE "a"
         cType := IIF( lTrans, "Matriz", "Array" )
         EXIT
      CASE "o"
         cType := IIF( lTrans, "Objeto", "Object" )
         EXIT
      CASE "h"
         cType := "Hash"
         EXIT
      CASE "b"
         cType := IIF( lTrans, "Bloque de código", "Code-block" )
         EXIT
      OTHERWISE
         cType := IIF( lTrans, "Cualquiera", "Any" )
      END SWITCH
   ELSE
      lLink := .T.
   ENDIF

   IF HB_IsNIL( cInit )
      IF cFirst == "c"
         cInit := '""'
      ELSEIF cFirst == "a"
         cInit := "{}"
      ELSEIF cFirst == "h"
         cInit := "{=>}"
      ELSE
         cInit := "NIL"
      ENDIF
   ELSEIF Left( cInit, 1 ) == "{"
      cInit := StrTran( cInit, ",", ", " )
   ELSEIF cFirst == "C" .AND. !( Left(cInit, 1 ) $ ( Chr( 34 ) + Chr( 39 ) )  )
      cInit := '"' + cInit + '"'
   ENDIF

   IF ::lPersistent .AND. ::lReadOnly
      cType += " (PERSISTENT, READ-ONLY)"
   ELSEIF ::lPersistent
      cType += " (PERSISTENT)"
   ELSEIF ::lReadOnly
      cType += " (READ-ONLY)"
   ENDIF

   oTask := Document:Action( "Act_Help" )
   oTask:AddParam( "symbol", cParent )

   IF Left( cParent, 1 ) == "Z"
      SwapChr( @cParent, 1, "W" )
   ENDIF

   WITH OBJECT WCollection():New( oDiv )
      WITH OBJECT :AddHeader( )
         WITH OBJECT WSpan():New( SO )
            :oStyle:Font_size := "2em;"
            :cText := "DATA "
         END WITH
         WITH OBJECT WLink():New( SO )
            :cText := cParent
            :cHRef := oTask
            :oStyle:Font_size := "2em;"
         END WITH
         WITH OBJECT WSpan():New( SO )
            :oStyle:Font_size := "2em;"
            :cText := ":" + cName
         END WITH
      END WITH
      WITH OBJECT :AddItem()
         :oStyle:Padding := "0px 10px"
         :oStyle:Display := "flex"
         WITH OBJECT WSpan():New( SO )
            :AddClass( "col" )
            :AddStyle( "min-width: 100px;width: 100px; padding: 20px 10px; border-right: 1px solid var(--md-sys-color-outline-variant);" )
            :cText := "<strong>" + IIF( lTrans, "Tipo", "Type" ) + "</strong>"
         END WITH
         IF lLink
            oTask2 := Document:Action( "Act_Help" )
            oTask2:AddParam( "symbol", cType )
            WITH OBJECT WLink():New( SO )
               :AddClass( "col" )
               :oStyle:Padding := "20px 10px"
               :cText := cType
               oTask:SetParam( 1, cType )
               :cHRef := oTask2
            END WITH
         ELSE
            WITH OBJECT WSpan():New( SO )
               :AddClass( "col" )
               :oStyle:Padding := "20px 10px"
               :cText := cType
               :oStyle:Font_size := "1.2em;"
               :oStyle:Color := "var(--md-sys-color-primary)"
            END WITH
         ENDIF
      END WITH
      WITH OBJECT :AddItem()
         :oStyle:Padding := "0px 10px"
         :oStyle:Display := "flex"
         WITH OBJECT WSpan():New( SO )
            :AddClass( "col" )
            :AddStyle( "min-width: 100px; width: 100px; padding: 20px 10px; border-right: 1px solid var(--md-sys-color-outline-variant);" )
            :cText := "<strong>" + IIF( lTrans, "Valor inicial", "Initial value" ) + "</strong>"
         END WITH
         WITH OBJECT WSpan():New( SO )
            :AddClass( "col" )
            :oStyle:Flex_grow := "1"
            :AddStyle( "overflow: hidden; padding: 20px 10px; font-size: 1.2em; color: var(--md-sys-color-primary);" )
            :cText := cInit
         END WITH
      END WITH
      IF Len( ::aValues ) > 0
         WITH OBJECT :AddItem()
            :oStyle:Padding := "0px 10px"
            :oStyle:Display := "flex"
            WITH OBJECT WSpan():New( SO )
               :AddClass( "col" )
               :AddStyle( "min-width: 100px;width: 100px; padding: 20px 10px; border-right: 1px solid var(--md-sys-color-outline-variant);" )
               :cText := "<strong>" + IIF( lTrans, "Valores posibles", "Possible values" ) + "</strong>"
            END WITH
            WITH OBJECT WSpan():New( SO )
               :AddClass( "col" )
               :AddStyle( "overflow: hidden; padding: 20px 10px; font-size: 1.2em; color: var(--md-sys-color-primary);" )
               :cText := aJoin( ::aValues, ", " )
            END WITH
         END WITH
      ENDIF

   END WITH

   WITH OBJECT WParagraph():New( oDiv )
      :cText := LOREM_IPSUM
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD HtmlInfoFunc( oDiv, nType ) CLASS THelpObject

   LOCAL oTask
   LOCAL aParam
   LOCAL cParent, cName, cParam, cAsClass, cOpt, cReturn, cType
   LOCAL nOpt
   LOCAL lTrans

   cParent := ::oParent:cName
   cName   := ::cName
   cReturn := ::cReturn
   lTrans  := Document:NeedTranslate()

   IF cParent = "XA_Functions"
      nType := chPRGFUNC
   ENDIF

   SWITCH nType
   CASE chMETHOD
      cType := IIF( lTrans, "Método", "Method" )
      EXIT
   CASE chEVENT
      cType := IIF( lTrans, "Evento", "Event" )
      EXIT
   OTHERWISE
      cType := IIF( lTrans, "Función", "Function" )
      EXIT
   END SWITCH

   oTask := Document:Action( "Act_Help" )
   oTask:AddParam( "symbol", cParent )

   IF Left( cParent, 1 ) == "Z"
      SwapChr( @cParent, 1, "W" )
   ENDIF

   WITH OBJECT WCollection():New( oDiv )
      WITH OBJECT :AddHeader()
         :oStyle:Padding := "10px 10px"
         WITH OBJECT WSpan():New( SO )
            :oStyle:Font_size := "2em;"
            :cText := cType + " "
         END WITH
         WITH OBJECT WLink():New( SO )
            :cText := cParent
            :cHRef := oTask
            :oStyle:Font_size := "2em;"
         END WITH
         WITH OBJECT WSpan():New( SO )
            :oStyle:Font_size := "2em;"
            :cText := ":" + cName
         END WITH
      END WITH

      WITH OBJECT :AddItem()
         :oStyle:Padding := "0px 0px"
         :oStyle:Display := "flex"
         :oStyle:Gap := "0px"
         WITH OBJECT WSpan():New( SO )
            :AddClass( "col" )
            :AddStyle( "min-width: 100px; width: 100px; padding: 20px 10px; border-right: 1px solid var(--md-sys-color-outline-variant);" )
            :cText := "<strong>" + IIF( lTrans, "Parámetros", "Parameters" ) + "</strong>"
         END WITH

         WITH OBJECT WDiv():New( SO )
            :AddClass( "col" )
            :oStyle:Flex_grow := "1"
            WITH OBJECT WDiv():New( SO )
               WITH OBJECT WDiv():New( SO )
                  :AddClass( "collection-item" )
                  :AddStyle( "padding: 15px 10px;" )
                  IF Len( ::aParams ) == 0
                     WITH OBJECT WSpan():New( SO )
                        :oStyle:Display := "block"
                        :AddStyle( "padding: 6px 0px; font-size: 1.2em; color: var(--md-sys-color-primary);" )
                        :cText := IIF( lTrans, "Ninguno", "None" )
                     END WITH
                  ELSE
                     FOR EACH aParam IN ::aParams
                        cParam   := aParam[ 1 ]
                        cAsClass := aParam[ 2 ]
                        nOpt     := Val( aParam[ 3 ] )
                        cOpt:= " (" + IIF( ::lByRef, "BY REF", "BY VALUE" )
                        IF ::lOptional
                           cOpt += ", OPTIONAL"
                        ENDIF
                        cOpt += ")"

                        WITH OBJECT WDiv():New( SO )
                           :oStyle:Display := "block"
                           :AddStyle( "padding: 0px 0px; font-size: 1.2em; color: var(--md-sys-color-primary);" )
                           WITH OBJECT WSpan():New( SO )
                              :cText := cParam + cOpt
                           END WITH
                           IF !Empty( cAsClass )
                              WITH OBJECT WLink():New( SO )
                                 oTask:SetParam( 1, cAsClass )
                                 :cText := "Class: " + cAsClass
                                 :cHRef := oTask
                              END WITH
                           ENDIF
                        END WITH

                        WITH OBJECT WSpan():New( SO )
                           :cText := LOREM_IPSUM
                        END WITH
                     NEXT
                  ENDIF
               END WITH
            END WITH
         END WITH
      END WITH

      WITH OBJECT :AddItem()
         :oStyle:Padding := "0px 0px"
         :oStyle:Display := "flex"
         :oStyle:Gap := "0px"
         WITH OBJECT WSpan():New( SO )
            :AddClass( "col" )
            :AddStyle( "min-width: 100px; width: 100px; padding: 14px 10px; border-right: 1px solid var(--md-sys-color-outline-variant);" )
            :cText := "<strong>" + IIF( lTrans, "Valor de retorno", "Return value" ) + "</strong>"
         END WITH
         WITH OBJECT WDiv():New( SO )
            :AddClass( "col" )
            :oStyle:Flex_grow := "1"
            :AddStyle( "padding: 15px 10px;" )
            WITH OBJECT WSpan():New( SO )
               :oStyle:Display := "block"
               :AddStyle( "padding: 6px 0px; font-size: 1.2em; color: var(--md-sys-color-primary);" )
               :cText := ::cReturn + IIF( cReturn $"Self;New;Create", " (CONSTRUCTOR)", "" )
            END WITH
            WITH OBJECT WSpan():New( SO )
               :cText := LOREM_IPSUM
            END WITH
         END WITH
      END WITH

   END WITH

   WITH OBJECT WParagraph():New( oDiv )
      :cText := LOREM_IPSUM
   END WITH

RETURN nil

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION ReadLine( cText, nFrom, cEol )

   LOCAL cLine
   LOCAL nAt, nLen

   DEFAULT nFrom TO 1, cEol TO Hb_Eol()

   nLen := Len( cText )

   IF nFrom > nLen
      nFrom := 0
      RETURN ""
   ENDIF

   nAt := hb_At( cEol, cText, nFrom )

   IF nAt == 0
      cLine := Substr( cText, nFrom )
      nFrom := nLen + 1
   ELSE
      cLine := Substr( cText, nFrom, nAt - nFrom  )
      nFrom := nAt + Len( cEol )
   ENDIF

RETURN cLine

//------------------------------------------------------------------------------

