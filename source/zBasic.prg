/*
 * Proyecto: xaWeb framework
 * Fichero: ZBasic.prg
 * Descripción: Base Class for HTML elements
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This class is internal. Should not be instanciated directly
 */

#include "xaWeb.ch"
#include "error.ch"

STATIC oStyle, oContext

CLASS ZBasic
PUBLISHED:
   DATA oParent            AS CLASS WControl
   DATA oOwner             AS CLASS WControl

   DATA cAccessKey         INIT ""
   DATA cDir               INIT ""  VALUES "ltr", "rtl", "auto"
   DATA cDraggable         INIT ""  VALUES "true", "false", "auto"
   DATA cInputMode         INIT ""  VALUES "decimal", "email", "none", "numeric", "search", "tel", "text", "url"
   DATA cCache             INIT ""  VALUES "", "session", "global", "flush" // processed by SetCache( value )
   DATA cEnterKeyHint      INIT ""
   DATA cLang              INIT ""
   DATA cPopOver           INIT ""
   DATA cTitle             INIT ""

   DATA nTabIndex          INIT NIL

   DATA lContentEditable   INIT NIL VALUES NIL, .T., .F.
   DATA lSpellCheck        INIT NIL VALUES NIL, .T., .F.
   DATA lHidden            INIT .F. VALUES .T., .F.
   DATA lInert             INIT .F. VALUES .T., .F.

   METHOD New( oParent AS CLASS WControl,;
               oOwner AS CLASS WControl OPTIONAL,;
               lAutoCreated OPTIONAL ) CONSTRUCTOR
   METHOD Create( oParent AS CLASS WControl,;
                  oOwner AS CLASS WControl OPTIONAL,;
                  lAutoCreated OPTIONAL) // --> Self
   METHOD cId( cValue )       SETGET
   METHOD lTranslate( Value ) SETGET
   METHOD cClass( cClass )    SETGET
   METHOD oStyle( oStyle )    SETGET AS CLASS WStyle
   METHOD oContext( oStyle )  SETGET AS CLASS WContextHelper

   METHOD AddClass( cClass )
   METHOD DelClass( cClass )
   METHOD SwapClass( cOldClass, cNewClass )   // --> lvalue
   METHOD ToggleClass( cOldClass, cNewClass ) INLINE ::SwapClass( cOldClass, cNewClass )
   METHOD IsClass( cClass, nExact ) // --> lValue

   METHOD cStyle( cStyle )    SETGET
   METHOD AddStyle( cStyle )
   METHOD DelStyle( cStyle )   // --> lValue
   METHOD GetStyle( cStyle )  INLINE HB_HGetDef( ::hStyle, cStyle, "" )  // --> cStyle
   METHOD IsStyle( cStyle )  // --> lValue

   METHOD cDataset() SETGET
   METHOD AddDataset( cName, cValue )
   METHOD IsDataset()         INLINE Len( ::hDataset ) > 0  // --> lValue

   METHOD lVisible( lValue ) SETGET

   EVENT OnDeploy( oSender, BYREF cHtml )
   EVENT OnPreprocess( oSender )

   EVENT OnClick( JsEvent )
   EVENT OnDblClick( JsEvent )
   EVENT OnMouseDown( JsEvent )
   EVENT OnMouseMove( JsEvent )
   EVENT OnMouseOut( JsEvent )
   EVENT OnMouseOver( JsEvent )
   EVENT OnMouseUp( JsEvent )
   EVENT OnWheel( JsEvent )

   EVENT OnKeyDown( JsEvent )
   EVENT OnKeyPress( JsEvent )
   EVENT OnKeyUp( JsEvent )

   EVENT OnDrag( JsEvent )
   EVENT OnDragEnd( JsEvent )
   EVENT OnDragEnter( JsEvent )
   EVENT OnDragLeave( JsEvent )
   EVENT OnDragOver( JsEvent )
   EVENT OnDragStart( JsEvent )
   EVENT OnDrop( JsEvent )
   EVENT OnScoll( JsEvent )

   EVENT OnCopy( JsEvent )
   EVENT OnCut( JsEvent )
   EVENT OnPaste( JsEvent )

   EVENT OnBlur( JsEvent )
   EVENT OnChange( JsEvent )
   EVENT OnContextMenu( JsEvent )
   EVENT OnFocus( JsEvent )
   EVENT OnInput( JsEvent )
   EVENT OnInvalid( JsEvent )
   EVENT OnReset( JsEvent )
   EVENT OnSearch( JsEvent )
   EVENT OnSelect( JsEvent )
   EVENT OnSubmit( JsEvent )

   EVENT OnToggle( JsEvent )

RESERVED:
   DATA aComponents     INIT {}
   DATA aParseInfo      INIT {}
   DATA cTag            INIT ""
   DATA lAutoCreated    INIT .F. READONLY
   DATA lProcessed      INIT .F. READONLY

   METHOD End()
   METHOD CustomCss()              INLINE ""
   METHOD ShowSection( cSection )  INLINE '<script>xa_showSection("' + cSection + '", true);</script>'
   METHOD InsertComponent( oComponent )
   METHOD RemoveComponent( oComponent )
   METHOD RunHtml()
   METHOD HtmlTagIni()
   METHOD HtmlTagBody()
   METHOD HtmlTagEnd()
   METHOD Html()                       INLINE ::RunHtml()
   METHOD AddEvent( cEvent, cValue )
   METHOD DataProperties()             INLINE __objGetProperties( Self, .F. )
   METHOD GenJS( aData, aEvent )
   METHOD IsEvent( cEvent )            INLINE HB_HHasKey( ::hEvents, cEvent )
   METHOD IsJsEvent( cEvent )
   METHOD IsHbEvent( cEvent )
   METHOD EventValue( cEvent )         INLINE IIF( HB_HHasKey( ::hEvents, cEvent ),;
                                                   HB_HGet( ::hEvents, cEvent ), NIL )
   METHOD EventDelete( cEvent )        INLINE IIF( HB_HHasKey( ::hEvents, cEvent ),;
                                                   HB_HDel( ::hEvents, cEvent ),.F.)

   METHOD AreJsEvents()
   METHOD ServiceJs( nType, cMsg, ... )
   METHOD ValidId()
   METHOD PreProcess()
   METHOD nIndex()
   METHOD GetForm()        INLINE GetForm( Self )
   METHOD GetSection()     INLINE GetSection( Self )
   METHOD GetClosestControlWithId()   INLINE GetClosestId( Self )
   METHOD IsCached()
   METHOD CacheFile()
   METHOD FlushCache()
   METHOD SetCache( Value )
   METHOD NeedTranslate()

PROTECTED:
   DATA cText           INIT NIL // Overloaded on tags that supported
   DATA lEndTagForced   INIT .F.
   DATA lCreated        INIT .F.

HIDDEN:
   DATA fcId            INIT ""
   DATA flTranslate     INIT NIL
   DATA aClass          INIT {}
   DATA hStyle          INIT NIL
   DATA hEvents         INIT NIL
   DATA hDataset        INIT NIL
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAutoCreated ) CLASS ZBasic

   IF oParent:IsKindOf( "WDoc" ) .AND. !::IsKindOf( "WDocSection" )
      oParent := Document:GetDefaultSection()
   ENDIF

   IF oParent != NIL
      oParent:InsertControl( Self )
   ENDIF

   DEFAULT oOwner TO oParent

   IF oOwner == oParent
      oOwner:InsertComponent( Self )
   ENDIF

   ::oParent := oParent
   ::oOwner  := oOwner

   ::hEvents  := HB_Hash()
   ::hStyle   := HB_Hash()
   ::hDataset := HB_Hash()

   HB_HAutoAdd( ::hEvents, .T. )
   HB_HCaseMatch( ::hEvents, .F. )

   HB_HAutoAdd( ::hStyle, .T. )
   HB_HCaseMatch( ::hStyle, .F. )

   HB_HAutoAdd( ::hDataset, .T. )
   HB_HCaseMatch( ::hDataset, .F. )

   WITH OBJECT Engine
      IF !Empty( lAutoCreated )
         ::lAutoCreated := .T.
      ELSEIF ( :lFromIde .OR. :lCmdArgs )
         ::aParseInfo := ParseInfo( Self )
      ENDIF
   END WITH

RETURN Self

//------------------------------------------------------------------------------

METHOD Create( oParent, oOwner, lAutoCreated ) CLASS ZBasic

   LOCAL hState, hId, cKey, xVal

   IF ::hEvents == NIL
      ::New( oParent, oOwner, lAutoCreated )
   ENDIF

   ::lCreated := .T.

   hState := Engine:hState

   IF !Empty( ::cId ) .AND. HB_HHasKey( hState, ::cId )
      hId := HB_HGet( hState, ::cId )
      FOR EACH cKey, xVal IN HB_HKeys( hId ), HB_HValues( hId )
         __objSendMsg( Self, "_" + cKey, xVal )
      NEXT
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD End()  CLASS ZBasic

   ::oParent := NIL
   ::oOwner  := NIL

   ::aComponents := {}
   ::aParseInfo  := {}

   ::aClass   := {}
   ::hStyle   := NIL
   ::hEvents  := NIL
   ::hDataset := NIL

RETURN nil

//------------------------------------------------------------------------------

METHOD InsertComponent( oComponent ) CLASS ZBasic

   AAdd( ::aComponents, oComponent )

RETURN Len( ::aComponents )

//------------------------------------------------------------------------------

METHOD RemoveComponent( oComponent ) CLASS ZBasic

   LOCAL n

   IF ( n := AScan( ::aComponents, {| Ctl | Ctl == oComponent } ) ) > 0
      ::aComponents[ n ]:oOwner := NIL
      HB_ADel( ::aComponents, n, .T. )
   ENDIF

RETURN ( n > 0 )


//------------------------------------------------------------------------------

METHOD cId( cValue ) CLASS ZBasic

   IF PCount() > 0 .AND. HB_IsString( cValue )
      ::fcId := cValue
      Document:CheckId( Self, cValue )
   ENDIF

RETURN ::fcId

//------------------------------------------------------------------------------

METHOD lTranslate( lValue ) CLASS ZBasic

   IF PCount() > 0 .AND. HB_IsLogical( lValue )
      ::flTranslate := lValue
      Translator()
   ENDIF

RETURN ::flTranslate

//------------------------------------------------------------------------------

METHOD oStyle() CLASS ZBasic

   IF oStyle == NIL
      oStyle := WStyle():New( Self )
   ELSE
      oStyle:SetControl( Self )
   ENDIF

RETURN oStyle

//------------------------------------------------------------------------------

METHOD oContext() CLASS ZBasic

   IF oContext == NIL
      IF HB_IsObject( Document:oContext )
         oContext := Document:oContext:GetHelper( Self )
      ENDIF
   ELSE
      oContext:SetControl( Self )
   ENDIF

RETURN oContext

//------------------------------------------------------------------------------

METHOD ValidId() CLASS ZBasic

   LOCAL oOwner
   LOCAL hId
   LOCAL cPar, cId := ::fcId
   LOCAL nBuild

   IF Empty( cId )
      oOwner := ::GetClosestControlWithId()
      nBuild := 0
      hId    := Document:hId
      IF oOwner != NIL
         cPar := oOwner:cId + "_"
         cId  := cPar + ToString( nBuild++ )
         DO WHILE HB_HHasKey( hId, cId )
            cId := cPar + ToString( nBuild++ )
         ENDDO
         ::fcId :=  cId
         HB_HSet( hId, cId, Self )
      ENDIF
   ENDIF

RETURN cId

//------------------------------------------------------------------------------

METHOD lVisible( lValue ) CLASS ZBasic

   IF PCount() > 0
      ::lHidden := !lValue
   ENDIF

RETURN !::lHidden

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZBasic

   IF ::IsEvent( "OnPreProcess" )
      ::OnPreprocess()
   ENDIF

   ::lProcessed := .T.

RETURN NIL

//------------------------------------------------------------------------------

METHOD RunHtml() CLASS ZBasic

   LOCAL cHtml := "", cFile

   IF !::IsCached()
      IF ::AreJsEvents() .AND. Empty( ::cId )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := "HTML elements with events overloaded must have a valid ID."
            :Operation   := "WBasic:RunHtml()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF

      cHtml += ::HtmlTagIni()
      cHtml += ::HtmlTagBody()
      cHtml += ::HtmlTagEnd()
      IF !Empty( ::cCache )
         cFile := ::CacheFile()
         HB_MemoWrit( cFile, cHtml, .F. )
      ENDIF
   ELSE
      cHtml := HB_MemoRead( ::CacheFile() )
   ENDIF

   ::OnDeploy( @cHtml )

RETURN cHtml

//------------------------------------------------------------------------------

METHOD HtmlTagIni() CLASS ZBasic

   LOCAL cHtml := HTML_SPACES

   cHtml += '<' + ::cTag

RETURN cHtml

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZBasic

   LOCAL cHtml, cText := ::cText

   IF !Empty( cText )
      IF ::NeedTranslate()
         cText := Translator():Translate( cText )
      ENDIF
      cHtml := '>' + cText + '</' + ::cTag + '>' + hb_eol()
   ELSEIF ::lEndTagForced
      cHtml := '></' + ::cTag + '>' + hb_eol()
   ELSE
      cHtml := '>' + hb_eol()
   ENDIF

RETURN cHtml

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZBasic

   LOCAL cHtml, cKey, cValue

   cHtml := ""

   IF !Empty( ::cId )
      cHtml += ' id="' + ::cId + '"'
   ENDIF

   IF Len( ::aClass ) > 0
      cHtml += ' class="' + ::cClass + '"'
   ENDIF

   IF !Empty( ::cAccessKey )
      cHtml += ' accesskey="' + ::cAccessKey + '"'
   ENDIF

   IF HB_IsLogical( ::lContentEditable )
      cHtml += ' contenteditable="' + IIF( ::lContentEditable, "true", "false" ) + '"'
   ENDIF

   IF !Empty( ::cDir )
      cHtml += ' dir="' + ::cDir + '"'
   ENDIF

   IF !Empty( ::cDraggable )
      cHtml += ' draggable="' + ::cDraggable + '"'
   ENDIF

   IF !Empty( ::cEnterKeyHint )
      cHtml += ' enterkeyhint="' + ::cEnterKeyHint + '"'
   ENDIF

   IF ::lHidden
      cHtml += ' hidden'
   ENDIF

   IF ::lInert
      cHtml += ' inert'
   ENDIF

   IF !Empty( ::cInputMode )
      cHtml += ' inputmode="' + ::cInputMode + '"'
   ENDIF

   IF !Empty( ::cLang )
      cHtml += ' lang="' + ::cLang + '"'
   ENDIF

   IF !Empty( ::cPopOver )
      cHtml += ' popover="' + ::cPopOver + '"'
   ENDIF

   IF HB_IsLogical( ::lSpellCheck )
      cHtml += ' spellcheck="' + IIF( ::lSpellCheck, "true", "false" ) + '"'
   ENDIF

   IF HB_IsLogical( ::lTranslate )
      cHtml += ' translate="' + IIF( ::lTranslate, "yes", "no" ) + '"'
   ENDIF

   IF Len( ::hStyle ) > 0
      cHtml += ' style="' + ::cStyle + '"'
   ENDIF

   IF HB_IsNumeric( ::nTabIndex )
      cHtml += ' tabindex="' + LTrim( Str( ::nTabIndex ) )  + '"'
   ENDIF

   IF !Empty( ::cTitle )
      cHtml += ' title="' + ::cTitle + '"'
   ENDIF

   IF ::AreJsEvents() .OR. ::IsKindOf( "WForm" )
      cHtml += ' data-document="' + Document:cName + '"'
   ENDIF

   cHtml += ::cDataset

   FOR EACH cKey, cValue IN HB_HKeys( ::hEvents ), HB_HValues( ::hEvents )
      IF HB_IsObject( cValue )
         cHtml += ' ' + cKey + "='" + cValue:Html() + "'" // comillas al reves
      ELSEIF HB_IsString( cValue )
         IF hB_LeftEqi( cValue, "<script>" )
            cValue := JSReducer( cValue )
            cValue := "(function(){" + cValue + "})()"
            cHtml += ' ' + cKey + '="' + cValue + '"'
         ELSEIF !Empty( cValue )
            IF __objHasMethod( Document, cValue )
               cValue := "@" + cValue
               HB_HSet( ::hEvents, cKey, cValue )
            ENDIF
            cHtml += ' ' + cKey + '="' + cValue + '"'
         ENDIF
      ENDIF
   NEXT

RETURN cHtml

//------------------------------------------------------------------------------

METHOD cClass( xValue ) CLASS ZBasic

   IF PCount() > 0
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := "It is not advisable to set the cClass property to a value directly. Use the AddClass() and DelClass() methods"
         :Operation   := "WBasic:cClass property assignment"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      IF HB_IsArray( xValue )
         ::aClass := xValue
      ELSE
         ::aClass := { xValue }
      ENDIF
   ENDIF

RETURN aJoin( ::aClass, " " )

//------------------------------------------------------------------------------

METHOD AddClass( cClass ) CLASS ZBasic

   LOCAL aClass := ::aClass
   LOCAL cTok
   LOCAL nAt

   FOR EACH cTok IN HB_ATokens( cClass, " " )
      IF ( nAt := AScan( aClass, cTok ) ) == 0
         AAdd( aClass, cTok )
      ENDIF
   NEXT

RETURN NIL

//------------------------------------------------------------------------------

METHOD DelClass( cClass ) CLASS ZBasic

   LOCAL nAt

   IF ( nAt := AScan( ::aClass, cClass ) ) > 0
      HB_ADel( ::aClass, nAt, .T. )
   ENDIF

RETURN ( nAt > 0 )

//------------------------------------------------------------------------------

METHOD SwapClass( cOldClass, cNewClass )

   LOCAL nAt

   IF ( nAt := AScan( ::aClass, cOldClass ) ) > 0
      ::aClass[ nAt ] := cNewClass
   ENDIF

RETURN ( nAt > 0 )

//------------------------------------------------------------------------------

METHOD IsClass( cClass, nExact ) CLASS ZBasic

   LOCAL nLen
   LOCAL lRet, lExa

   DEFAULT nExact TO 0

   SWITCH nExact
   CASE 0
      lRet := ( AScan( ::aClass, cClass ) > 0 )
      EXIT
   CASE 1 // beginning
      lExa := Set( _SET_EXACT, .F. )
      lRet := ( AScan( ::aClass, {|v| cClass = v } ) > 0 )
      Set( _SET_EXACT, lExa )
      EXIT
   CASE -1 // end
      nLen := Len( cClass )
      lRet := ( AScan( ::aClass, {|v| Right( v, nLen ) == cClass } ) > 0 )
      EXIT
   OTHERWISE
      lRet := .F.
   END CASE

RETURN lRet

//------------------------------------------------------------------------------

METHOD AreJsEvents() CLASS ZBasic

   LOCAL xValue
   LOCAL cKey

   FOR EACH cKey, xValue IN HB_HKeys( ::hEvents ), hb_HValues( ::hEvents )
      IF HB_IsString( xValue ) .AND. !Empty( xValue ) .AND. ;
         !HB_LeftEq( xValue, "@" ) .AND. !HB_LeftEqi( xValue, "<script>" )
         RETURN .T.
      ENDIF
   NEXT

RETURN .F.

//------------------------------------------------------------------------------

METHOD IsJsEvent( cEvent ) CLASS ZBasic

   LOCAL xValue

   IF HB_HHasKey( ::hEvents, cEvent )
      xValue := HB_HGet( ::hEvents, cEvent )
      IF HB_IsObject( xValue ) .OR. ;
         ( HB_IsString( xValue ) .AND. !__objHasMethod( Document, xValue ) )
         RETURN .T.
      ENDIF
   ENDIF

RETURN .F.

//------------------------------------------------------------------------------

METHOD IsHbEvent( cEvent ) CLASS ZBasic

   LOCAL xValue

   IF HB_HHasKey( ::hEvents, cEvent )
      xValue := HB_HGet( ::hEvents, cEvent )
      IF HB_IsString( xValue ) .AND. __objHasMethod( Document, xValue )
         RETURN .T.
      ENDIF
   ENDIF

RETURN .F.

//------------------------------------------------------------------------------

METHOD cStyle( cStyle ) CLASS ZBasic

   LOCAL cJoin := "", cValue, cKey
   LOCAL nAt

   IF PCount() > 0
      FOR EACH cValue IN HB_ATokens( cStyle, ";" )
         IF ( nAt := At( ":", cValue ) ) > 0
            HB_HSet( ::hStyle, Left( cValue, nAt - 1 ), SubStr( cValue, nAt + 1 ) )
         ENDIF
      NEXT
   ELSE
      FOR EACH cKey, cValue IN HB_HKeys( ::hStyle ), HB_HValues( ::hStyle )
         cJoin += cKey + ":" + cValue + ";"
      NEXT
   ENDIF

RETURN cJoin

//------------------------------------------------------------------------------

METHOD AddStyle( cStyle ) CLASS ZBasic

   ::cStyle += cStyle

RETURN NIL

//------------------------------------------------------------------------------

METHOD DelStyle( cStyle ) CLASS ZBasic

   IF HB_HHasKey( ::hStyle, cStyle )
      HB_HDel( ::hStyle, cStyle )
      RETURN .T.
   ENDIF

RETURN .F.

//------------------------------------------------------------------------------

METHOD IsStyle( cStyle )

   LOCAL lRet

   IF Empty( cStyle )
      lRet := ( Len( ::hStyle ) > 0 )
   ELSE
      lRet := HB_HHasKey( ::hStyle, cStyle )
   ENDIF

RETURN lRet

//------------------------------------------------------------------------------

METHOD cDataset() CLASS ZBasic

   LOCAL cJoin := "", cValue, cKey

   FOR EACH cKey, cValue IN HB_HKeys( ::hDataset ), HB_HValues( ::hDataset )
      cJoin += ' data-' + cKey + IIF( !Empty( cValue ), '="' + cValue + '"', '')
   NEXT

RETURN cJoin

//------------------------------------------------------------------------------
/*
 Note: Javascript converts all datataset keys to lower case. In order to have
       Camel case, you must use the hyphen. For example: 'my-button' -> myButton
       This transformation is done automatically by xaWeb
*/

METHOD AddDataset( cName, cValue ) CLASS ZBasic

   LOCAL cTmp := "", cChar

   FOR EACH cChar IN cName
      IF IsUpper( cChar )
         cTmp += "-" + Lower( cChar )
      ELSE
         cTmp += cChar
      ENDIF
   NEXT

   HB_HSet( ::hDataset, cTmp, cValue )

RETURN NIL

//------------------------------------------------------------------------------

METHOD GenJS( aData, aEvent ) CLASS ZBasic

   LOCAL aVal
   LOCAL cId, cKey, cValue, cTmp := ""

   cId := ::cId

   IF Empty( cId )
      RETURN nil
   ENDIF

   FOR EACH aVal IN ::DataProperties()
      cTmp += ',"' + Lower( aVal[ 1 ] )+ '":' + ValToJS( aVal[ 2 ] )
   NEXT

   IF !Empty( cTmp )
      IF !::lCreated
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := "HTML elements [" + ::Classname + "]  with PERSISTENT members should call its Create() method, after properties assignment."
            :Operation   := "WBasic:GenJS()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
      AAdd( aData, { cId, SubStr( cTmp, 2 ) } )
   ENDIF

   cTmp := ""

   FOR EACH cKey, cValue IN HB_HKeys( ::hEvents ), HB_HValues( ::hEvents )
      IF HB_IsString( cValue ) .AND. !HB_LeftEqi( cValue, "<" )
         cTmp += ',"' + Lower( cKey ) + '":"' + ToString( cValue ) + '"'
      ENDIF
   NEXT

   IF !Empty( cTmp )
      AAdd( aEvent, { cId, SubStr( cTmp, 2 ) } )
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD ServiceJs( nType, cMsg, ... ) CLASS ZBasic

   AAdd( Document:aServiceJs, { Self, cMsg, ... } )

   IF nType == 0
      cMsg := "_" + cMsg
   ENDIF

RETURN __objSendMsg( Self, cMsg, ... )

//------------------------------------------------------------------------------

METHOD AddEvent( cEvent, cValue ) CLASS ZBasic

   HB_HSet( ::hEvents, Lower( cEvent ), cValue )

RETURN  __objSendMsg( Self, "_ev_" + cEvent, cValue )

//------------------------------------------------------------------------------

METHOD nIndex() CLASS ZBasic

RETURN Ascan( ::oParent:aControls, {|v| v == Self } )

//------------------------------------------------------------------------------

METHOD IsCached() CLASS ZBasic

   IF Empty( ::cCache ) .OR. ::cCache == "flush" .OR. Empty( ::fcId )
      RETURN .F.
   ENDIF

RETURN HB_FileExists( ::CacheFile() )

//------------------------------------------------------------------------------

METHOD CacheFile() CLASS ZBasic

   LOCAL cFile

   SWITCH ::cCache
   CASE "session"
      cFile := Session():CacheFile( ::fcId )
      EXIT
   CASE "global"
      cFile := Document:CacheFile( ::fcId )
      EXIT
   OTHERWISE
      cFile := ""
   END SWITCH

RETURN cFile

//------------------------------------------------------------------------------

METHOD FlushCache() CLASS ZBasic

   LOCAL cFile := ::CacheFile()

   IF !Empty( cFile ) .AND. HB_FileExists( cFile )
      RETURN ( FErase( cFile ) == 0 )
   ENDIF

RETURN .F.

//------------------------------------------------------------------------------

METHOD SetCache( Value ) CLASS ZBasic

   LOCAL cPath

   IF Value == "flush"
      ::FlushCache()
      ::fcCache := ""
      RETURN Value
   ENDIF

   IF !Empty( Value ) .AND. Empty( ::fcId )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Cache can only be set on control with not empty ID"
         :Operation   := "WBasic:cCache assignment"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN ""
   ENDIF

   IF Value == "global"
      cPath :=  HB_DirBase() + "cache" + hb_OsPathSeparator()
      IF !HB_DirExists( cPath ) .AND. ( HB_DirCreate( cPath ) != 0 )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := "Cache directory could not be created"
            :Operation   := "WBasic:cCache assignment"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
         Value := ""
      ENDIF
   ENDIF

   ::fcCache := Value

RETURN Value

//------------------------------------------------------------------------------

METHOD NeedTranslate() CLASS ZBasic

   STATIC lTranslate

   IF lTranslate == NIl
      lTranslate := ( Upper( Document:cLang ) != "EN" )
   ENDIF

RETURN lTranslate .AND. !Empty( ::lTranslate )

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION GetForm( oControl )

   LOCAL oParent := oControl:oParent

   IF HB_IsObject( oParent ) .AND. !oParent:IsKindOf( "WForm" )
      oParent := GetForm( oParent )
   ENDIF

RETURN oParent

//------------------------------------------------------------------------------

STATIC FUNCTION GetSection( oControl )

   LOCAL oParent := oControl:oParent

   IF HB_IsObject( oParent ) .AND. !oParent:IsKindOf( "WDocSection" )
      oParent := GetSection( oParent )
   ENDIF

RETURN oParent

//------------------------------------------------------------------------------

STATIC FUNCTION GetClosestId( oControl )

   LOCAL oParent := oControl:oParent

   IF HB_IsObject( oParent ) .AND. Empty( oParent:cId )
      oParent := GetClosestId( oParent )
   ENDIF

RETURN oParent

//------------------------------------------------------------------------------
