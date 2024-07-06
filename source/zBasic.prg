/*
 * Proyect: XailerWeb framework
 * File: ZBasic.prg
 * Description: Base Class for HTML elements
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Note: This class is internal. Should not be instanciated directly
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZBasic
EXPORTED:
   DATA oParent
   DATA oOwner

   DATA cAccessKey         INIT ""
   DATA cDir               INIT ""  VALUES "ltr", "rtl", "auto"
   DATA cDraggable         INIT ""  VALUES "true", "false", "auto"
   DATA cInputMode         INIT ""  VALUES "decimal", "email", "none", "numeric", "search", "tel", "text", "url"
   DATA cEnterKeyHint      INIT ""
   DATA cLang              INIT ""
   DATA cPopOver           INIT ""
   DATA cTitle             INIT ""

   DATA nTabIndex          INIT NIL

   DATA lContentEditable   INIT NIL
   DATA lSpellCheck        INIT NIL
   DATA lTranslate         INIT NIL
   DATA lHidden            INIT .F.
   DATA lInert             INIT .F.


   METHOD New( oParent, oOwner ) CONSTRUCTOR
   METHOD Create()


   METHOD cId( cValue ) SETGET
   METHOD cClass( cClass ) SETGET
   METHOD oStyle( oStyle ) SETGET AS CLASS WStyle

   METHOD AddClass( cClass )
   METHOD DelClass( cClass )
   METHOD SwapClass( cOldClass, cNewClass )
   METHOD IsClass( cClass )

   METHOD CustomCss()         INLINE ""
   METHOD cStyle( cStyle )    SETGET
   METHOD AddStyle( cStyle )
   METHOD DelStyle( cStyle )
   METHOD GetStyle( cStyle )  INLINE HB_HGetDef( ::hStyle, cStyle, "" )
   METHOD IsStyle( cStyle )

   METHOD cDataset() SETGET
   METHOD AddDataset( cName, cValue )
   METHOD IsDataset()         INLINE Len( ::hDataset ) > 0

   METHOD lVisible( lValue ) SETGET    // Xailer style

   METHOD ShowPage( cPage )  INLINE '<script>xw_showPage("' + cPage + '", true);</script>'

   EVENT OnStartHtml( oSender, BYREF cHtml )
   EVENT OnEndHtml( oSender, BYREF cHtml )
   EVENT OnPreprocess( oSender )

   EVENT OnClick( element )
   EVENT OnDblClick( element )
   EVENT OnMouseDown( element )
   EVENT OnMouseMove( element )
   EVENT OnMouseOut( element )
   EVENT OnMouseOver( element )
   EVENT OnMouseUp( element )
   EVENT OnWheel( element )

   EVENT OnKeyDown( element )
   EVENT OnKeyPress( element )
   EVENT OnKeyUp( element )

   EVENT OnDrag( element )
   EVENT OnDragEnd( element )
   EVENT OnDragEnter( element )
   EVENT OnDragLeave( element )
   EVENT OnDragOver( element )
   EVENT OnDragStart( element )
   EVENT OnDrop( element )
   EVENT OnScoll( element )

   EVENT OnCopy( element )
   EVENT OnCut( element )
   EVENT OnPaste( element )

   EVENT OnBlur( element )
   EVENT OnChange( element )
   EVENT OnContextMenu( element )
   EVENT OnFocus( element )
   EVENT OnInput( element )
   EVENT OnInvalid( element )
   EVENT OnReset( element )
   EVENT OnSearch( element )
   EVENT OnSelect( element )
   EVENT OnSubmit( element )

   EVENT OnToggle( element )

RESERVED:
   DATA aComponents  INIT {}

   METHOD InsertComponent( oComponent )
   METHOD RemoveComponent( oComponent )
   METHOD RunHtml()
   METHOD HtmlTagIni()
   METHOD HtmlTagBody()
   METHOD HtmlTagEnd()
   METHOD Html()                       INLINE ::RunHtml()
   METHOD AddJsEvent( cEvent, cValue )
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
   METHOD RandomId()
   METHOD PreProcess()
   METHOD nIndex()
   METHOD GetForm()     INLINE GetForm( Self )
   METHOD GetPage()     INLINE GetPage( Self )

PROTECTED:
   DATA foStyle         INIT NIL
   DATA cTag            INIT ""
   DATA cText           INIT NIL // Overloaded on tags that supported

HIDDEN:
   DATA fcId            INIT ""
   DATA aClass          INIT {}
   DATA hStyle          INIT NIL
   DATA hEvents         INIT NIL
   DATA hDataset        INIT NIL

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner ) CLASS ZBasic

   IF PCount() < 2
      oOwner := oParent
   ENDIF

   IF oParent != NIL
      IF oParent:IsKindOf( "WDoc" )
         oParent := Document:GetDefaultPage()
      ENDIF
      oParent:InsertControl( Self )
   ENDIF

   IF oOwner != NIL
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

RETURN Self

//------------------------------------------------------------------------------

METHOD Create( oParent ) CLASS ZBasic

   LOCAL hState, hId, cKey, xVal

   IF HB_IsObject( oParent ) .AND. Empty( ::oParent )
      ::New( oParent )
   ENDIF

   hState := Engine:hState

   IF !Empty( ::cId ) .AND. HB_HHasKey( hState, ::cId )
      hId := HB_HGet( hState, ::cId )
      FOR EACH cKey, xVal IN HB_HKeys( hId ), HB_HValues( hId )
         __objSendMsg( Self, "_" + cKey, xVal )
      NEXT
   ENDIF

RETURN Self

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

METHOD oStyle( oValue ) CLASS ZBasic

   IF PCount() > 0 .AND. HB_IsObject( oValue )
      ::foStyle := oValue
   ELSEIF ::foStyle == NIL
      ::foStyle := WStyle():New( Self )
   ENDIF

RETURN ::foStyle

//------------------------------------------------------------------------------

METHOD RandomId() CLASS ZBasic

   ::fcId :=  "xw-" + ToString( HB_RandomInt( 999999 ) )

RETURN ::fcId

//------------------------------------------------------------------------------

METHOD lVisible( lValue ) CLASS ZBasic

   IF PCount() > 0
      ::lHidden := !lValue
   ENDIF

RETURN !::lHidden

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZBasic

//   IF HB_IsObject( ::foStyle )
//      ::foStyle:PreProcess()
//   ENDIF

   ::OnPreprocess()

RETURN NIL

//------------------------------------------------------------------------------

METHOD RunHtml() CLASS ZBasic

   LOCAL cHtml := ""

   IF ::AreJsEvents() .AND. Empty( ::cId )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := "HTML elements with events overloaded must have a valid ID."
         :Operation   := "WBasic:RunHtml()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

   ::OnStartHtml( @cHtml )

   cHtml += ::HtmlTagIni()
   cHtml += ::HtmlTagBody()
   cHtml += ::HtmlTagEnd()

   ::OnEndHtml( @cHtml )

RETURN cHtml

//------------------------------------------------------------------------------

METHOD HtmlTagIni() CLASS ZBasic

   LOCAL cHtml := HTML_SPACES

   cHtml += '<' + ::cTag

RETURN cHtml

//------------------------------------------------------------------------------

METHOD HtmlTagEnd() CLASS ZBasic

   LOCAL cHtml

   IF !Empty( ::cText )
      cHtml := '>' + ::cText + '</' + ::cTag + '>' + hb_eol()
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

   IF ::AreJsEvents()
      cHtml += ' data-document="' + Document:cName + '"'
   ENDIF

   cHtml += ::cDataset

   FOR EACH cKey, cValue IN HB_HKeys( ::hEvents ), HB_HValues( ::hEvents )
      IF HB_IsObject( cValue )
         cHtml += ' ' + cKey + "='" + cValue:Html() + "'" // comillas al reves
      ELSEIF HB_IsString( cValue )
         IF hB_LeftEqi( cValue, "<script>" )
            cValue := JSReducer( cValue )
            cValue := "(function(e){" + cValue + "})()"
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
      IF Engine:lDebug
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_WARNING
            :Description := "It is not advisable to set the cClass property to a value directly. Use the AddClass() and DelClass() methods"
            :Operation   := "WBasic:cClass property assignment"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
      ENDIF
      IF HB_IsArray( xValue )
         ::aClass := xValue
      ELSE
         ::aClass := { xValue }
      ENDIF
   ENDIF

RETURN aJoin( ::aClass, " " )

//------------------------------------------------------------------------------

METHOD AddClass( cClass ) CLASS ZBasic

   LOCAL nAt

   IF ( nAt := AScan( ::aClass, cClass ) ) == 0
      AAdd( ::aClass, cClass )
      RETURN .t.
   ENDIF

RETURN .f.

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

METHOD IsClass( cClass ) CLASS ZBasic

RETURN ( AScan( ::aClass, cClass ) > 0 )

//------------------------------------------------------------------------------

METHOD AreJsEvents() CLASS ZBasic

   LOCAL xValue
   LOCAL cKey

   FOR EACH cKey, xValue IN HB_HKeys( ::hEvents ), hb_HValues( ::hEvents )
      IF HB_IsString( xValue ) .AND. !HB_LeftEq( xValue, "@" )
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
      cJoin += ' data-' + cKey + '="' + cValue + '"'
   NEXT

RETURN cJoin

//------------------------------------------------------------------------------
/*
 Note: Javascript converts all datataset keys to lower case. In order to have
       Camel case, you must use the hyphen. For example: 'my-button' -> myButton
       This transformation is done automatically by XailerWeb
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
      //cTmp += ',"' + Lower( SubStr( aVal[ 1 ], 2 ) ) + '":' + ToJS( aVal[ 2 ] )
      cTmp += ',"' + Lower( aVal[ 1 ] )+ '":' + ValToJS( aVal[ 2 ] )
   NEXT

   IF !Empty( cTmp )
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

METHOD AddJsEvent( cEvent, cValue ) CLASS ZBasic

   HB_HSet( ::hEvents, Lower( cEvent ), cValue )

RETURN  __objSendMsg( Self, "_ev_" + cEvent, cValue )

//------------------------------------------------------------------------------

METHOD nIndex() CLASS ZBasic

RETURN Ascan( ::oParent:aControls, {|v| v == Self } )

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

FUNCTION ValToJS( xValue )

   SWITCH ValType( xValue )
   CASE "C"
      xValue := '"' + StrEncodeEscape( xValue ) + '"'
      EXIT
   CASE "L"
      xValue := IIF( xValue, "true", "false" )
      EXIT
   CASE "N"
      xValue := ToString( xValue )
      EXIT
   CASE "D"
      xValue := "Date(" + Str( Year( xValue ), 4 ) + "," + ;
                StrZero( Month( xValue ), 2 ) + "," + ;
                StrZero( Day( xValue ), 2 ) + ")"
      EXIT
   CASE "T"
      xValue := "Date(" + Str( Year( xValue ), 4 ) + "," + ;
                StrZero( Month( xValue ), 2 ) + "," + ;
                StrZero( Day( xValue ), 2 ) + "," + ;
                StrZero( HB_Hour( xValue ), 2 ) + "," + ;
                StrZero( HB_Minute( xValue ), 2 ) + "," + ;
                StrZero( HB_Sec( xValue ), 2 ) + ")"
      EXIT
   OTHERWISE
      xValue := '""'
   END SWITCH

RETURN xValue

//------------------------------------------------------------------------------

FUNCTION JSReducer( cText )

   LOCAL aLines
   LOCAL cLine, cRet := ""

   cText := StrTran( cText, '"', "'" )

   IF hb_LeftEqi( cText, "<script>" )
      cText := SubStr( cText, 9 )
   ENDIF

   IF Lower( Right( cText, 9 ) ) == "</script>"
      cText := Left( cText, Len( cText ) - 9 )
   ENDIF

   aLines := HB_ATokens( cText, hb_eol() )

   FOR EACH cLine IN HB_ATokens( cText, hb_eol() )
      cLine := AllTrim( cLine )
      IF !Empty( cLine ) .AND. !hb_LeftEqi( cLine, "</script>" )
         cRet += ( cLine + IIF( Right( cLine, 1 ) != ";", ";", "" ) )
      ENDIF
   NEXT

RETURN cRet

//------------------------------------------------------------------------------

STATIC FUNCTION GetForm( oControl )

   LOCAL oParent := oControl:oParent

   IF HB_IsObject( oParent ) .AND. !oParent:IsKindOf( "WForm" )
      oParent := GetForm( oParent )
   ENDIF

RETURN oParent

//------------------------------------------------------------------------------

STATIC FUNCTION GetPage( oControl )

   LOCAL oParent := oControl:oParent

   IF HB_IsObject( oParent ) .AND. !oParent:IsKindOf( "WDocPage" )
      oParent := GetPage( oParent )
   ENDIF

RETURN oParent

//------------------------------------------------------------------------------

