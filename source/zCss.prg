/*
 * Proyecto: xaWeb framework
 * Fichero: ZCss.prg
 * Descripción: CSS control class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZCss

   DATA cName        INIT ""  // if a name is given it will try to render /css/<name>.css
                              // if exists. Code inside css should be deterministic
                              // Is programmer responsability to update de exteranl css file
   DATA cUrl         INIT ""
   DATA cCode        INIT ""
   DATA cIntegrity   INIT ""
   DATA cCrossorigin INIT "" VALUES "anonymous", "use-credentials"
   DATA cMedia       INIT ""
   DATA cId          INIT ""
   DATA lPreload     INIT .F.

   METHOD New( cText, cName OPTIONAL, lUri OPTIONAL )    CONSTRUCTOR
   METHOD AddCode( cCode )              // --> lValue
   METHOD AddStyle( xName, cStyle )     // --> lValue
   METHOD DelStyle( cName )            INLINE HB_HDel( ::hCSS, cName )
   METHOD IsCode()                     INLINE !Empty( ::cCode ) .OR. ( Len( ::hCSS ) > 0 )  // --> lValue
   METHOD IsEmpty()                    INLINE !::IsCode() .AND. !::IsUrl()  // --> lValue
   METHOD IsUrl()                       // --> lValue

RESERVED:
   DATA hCSS         INIT { => }

   METHOD End() VIRTUAL
   METHOD Html()
   METHOD HtmlCode()
   METHOD HtmlUrl()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cText, cName, lUri ) CLASS ZCss

   DEFAULT cName TO ""
   DEFAULT lUri TO !( hb_eol() $ cText ) .AND. !( "{" $ cText )

   IF lUri
      ::cUrl := cText
   ELSEIF !Empty( cText )
      ::cCode := cText
   ENDIF

   HB_HCaseMatch( ::hCSS, .F. )
   HB_HAutoAdd( ::hCSS, .T. )

   ::cName := cName

RETURN Self

//------------------------------------------------------------------------------

METHOD AddCode( cCode ) CLASS ZCss

   IF !Empty( ::cUrl )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Code can only be added to "inline" CSS'
         :Operation   := "WCss:AddCode()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
         RETURN .f.
      END WITH
   ENDIF

   ::cCode += hb_eol() + cCode

RETURN .T.

//------------------------------------------------------------------------------

METHOD AddStyle( xName, cStyle ) CLASS ZCss

   LOCAL cName, hCSS

   hCSS := ::hCSS

   IF !Empty( cStyle ) .AND. Right( cStyle, 1 ) != ";"
      cStyle += ";"
   ENDIF

   IF !Empty( ::cUrl )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Code can only be added to "inline" CSS'
         :Operation   := "WCss:AddToCSS()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
         RETURN .F.
      END WITH
   ENDIF

   IF HB_IsString( xName )
      xName := { xName }
   ENDIF

   FOR EACH cName IN xName
      IF HB_HHasKey( hCSS, cName )
         HB_HSet( hCSS, cName, HB_HGet( hCSS, cName ) + cStyle )
      ELSE
         HB_HSet( hCSS, cName, cStyle )
      ENDIF
   NEXT

RETURN .T.

//------------------------------------------------------------------------------

METHOD Html() CLASS ZCss

   LOCAL cBuffer, cFile

   IF Empty( ::cUrl ) .AND. !Empty( ::cName )
      cFile := "/css/" + ::cName + ".css"
      IF Document:wwwFileExists( cFile )
         ::cUrl := cFile
         ::cCode := ""
      ENDIF
   ENDIF

   cBuffer := ::HtmlUrl()
   cBuffer += ::HtmlCode()

RETURN cBuffer

//------------------------------------------------------------------------------

METHOD HtmlCode() CLASS ZCss

   LOCAL xStyle
   LOCAL cLine, cBuffer, cKey, cProp, cValue, cFile
   LOCAL nIndent, nInd

   IF !::IsCode()
      RETURN ""
   ENDIF

   cBuffer := ""

   IF !Empty( ::cCode )
      nIndent := 0
      Document:nIndent ++
      FOR EACH cLine IN HB_ATokens( ::cCode, hb_eol() )
         cLine := AllTrim( cLine )
         IF !HB_LeftEq( cLine, "//" ) .AND. !Empty( cLine )
            SWITCH Right( cLine, 1 )
            CASE "{"
               cLine := Space( nIndent ) + AllTrim( cLine )
               nIndent += 2
               EXIT
            CASE "}"
               nIndent -= 2
               cLine := Space( nIndent ) + AllTrim( cLine )
               EXIT
            OTHERWISE
               cLine := Space( nIndent ) + AllTrim( cLine )
            END SWITCH
            cBuffer += HTML_SPACES + cLine + hb_eol()
         ENDIF
      NEXT
      Document:nIndent --
   ENDIF

   FOR EACH cKey, xStyle IN HB_HKeys( ::hCSS ), HB_HValues( ::hCSS )
      cBuffer += HTML_SPACES + cKey + " {" + hb_eol()
      Document:nIndent ++
      IF HB_IsArray( xStyle )
         FOR EACH cLine IN xStyle
            cBuffer += HTML_SPACES + cLine + ";" + hb_eol()
         NEXT
      ELSE
         FOR EACH cLine IN HB_ATokens( xStyle, ";" )
            IF !Empty( cLine )
               cBuffer += HTML_SPACES + AllTrim( cLine ) + ";" + hb_eol()
            ENDIF
         NEXT
      ENDIF
      cBuffer += HTML_SPACES + "}" + hb_eol()
      Document:nIndent --
   NEXT

RETURN cBuffer

//------------------------------------------------------------------------------

METHOD IsUrl() CLASS ZCss

   LOCAL cFile

   IF Empty( ::cUrl ) .AND. !Empty( ::cName )
      cFile := "/css/" + ::cName + ".css"
      IF Document:wwwFileExists( cFile )
         ::cUrl := cFile
         ::cCode := ""
      ENDIF
   ENDIF

RETURN !Empty( ::cUrl )

//------------------------------------------------------------------------------

METHOD HtmlUrl() CLASS ZCss

   LOCAL cBuffer

   IF !Empty( ::cUrl )
      IF !::lPreload
         cBuffer := HTML_SPACES + '<link rel="stylesheet" href="' + ::cUrl + '"'
      ELSE
         cBuffer := HTML_SPACES + '<link rel="preload" as="style" href="' + ::cUrl + '"'
      ENDIF
      IF !Empty( ::cIntegrity )
         cBuffer += ' integrity="' + ::cIntegrity + '"'
      ENDIF
      IF !Empty( ::cCrossorigin )
         cBuffer += ' crossorigin="' + ::cCrossorigin + '"'
      ENDIF
      IF !Empty( ::cMedia )
         cBuffer += ' media="' + ::cMedia + '"'
      ENDIF
      IF !Empty( ::cId )
         cBuffer += ' id="' + ::cId + '"'
      ENDIF
      cBuffer += '>' + hb_eol()
   ELSE
      cBuffer := ""
   ENDIF

RETURN cBuffer

//------------------------------------------------------------------------------