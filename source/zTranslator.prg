/*
 * Proyecto: xaWeb framework
 * Fichero: ZTranslator.prg
 * Descripción: Class Translator
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 *
 * Note:
 * The file {CGI_SHORTNAME}.DBF & CDX must exist on the directory
 * /usr/lib/cgi-bin/data (linux) and with the required privilegies
 * See doc about chown & chmod Linux commands
 * The file and the path can be changed by the user
 * File structure: {{ENGLISH},"C", 255, 0 }, {SPANISH},"C", 255, 0 }}
 * Index structure: TAG(1) Name: ENGLISH, Expression: PADR(ENGLISH, 255)
 * The original text must be set in English (EN)
*/

#include "xaWeb.ch"
#include "error.ch"

REQUEST DBFCDX, PADR

STATIC oTranslator
STATIC aLines  := {}
STATIC nLines  := 0
STATIC lReady  := .F.
STATIC lDeploy := .F.

//------------------------------------------------------------------------------

FUNCTION Translator( lCheckExists ) // --> WTranslator

   IF !Empty( lCheckExists )
      RETURN HB_IsObject( oTranslator )
   ENDIF

   IF oTranslator == NIL
      oTranslator := WTranslator():New( Document )
   ENDIF

RETURN oTranslator

//------------------------------------------------------------------------------

EXIT PROCEDURE Translator_Exit

   IF !HB_IsObject( oTranslator )
      RETURN
   ENDIF

   WITH OBJECT oTranslator
      SWITCH :cEngine
      CASE "dbf"
         :CloseDbf()
         EXIT
      CASE "sqlite"
         :CloseSqlite()
         EXIT
      CASE "text"
         :CloseText()
         EXIT
      END SWITCH
   END WITH

RETURN

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZTranslator FROM WPackage
PUBLISHED:
   DATA cPath        INIT ""
   DATA cFile        INIT ""
   DATA cEngine      INIT "none" VALUES "none", "dbf", "sqlite", "text"
   DATA lRebuild     INIT .F. // only for DBF

RESERVED:
   DATA cName        INIT "Translator" READONLY
   DATA lAutoCreated INIT .T.

   METHOD OpenDbf()
   METHOD CloseDbf()
   METHOD OpenSqlite()  VIRTUAL
   METHOD CloseSqlite() VIRTUAL
   METHOD OpenText()
   METHOD CloseText()

   METHOD Translate( cText ) // --> cText
   METHOD TranslateArray( aText ) // --> aText
   METHOD TranslateDbf( cText )
   METHOD TranslateSqlite( cText ) VIRTUAL
   METHOD TranslateText( cText )

   METHOD PreProcess()
   METHOD JsBottom()  // --> cText

END CLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTranslator

   SWITCH ::cEngine
   CASE "dbf"
      lReady := ::OpenDbf()
      EXIT
   CASE "sqlite"
      lReady := ::OpenSqlite()
      EXIT
   CASE "text"
      lReady := ::OpenText()
      EXIT
   END SWITCH

   IF !lDeploy .OR. Document:lRender2
      ::AddScript( ::JsBottom() )
   ENDIF

RETURN ::Super:Preprocess()

//------------------------------------------------------------------------------

METHOD OpenDbf() CLASS ZTranslator

   LOCAL cPath, cFile

   IF Empty( ::cPath )
      cPath := HB_DirBase() + "data" + hb_OsPathSeparator()
   ELSE
      cPath := ::cPath
   ENDIF

   IF !HB_DirExists( cPath )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Path does not exist. Translator stopped."
         :Operation   := "WTranslator:OpenDbf()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN .F.
   ENDIF

   IF Empty( ::cFile )
      cFile :=  cPath + Engine:ScriptShortName() + ".dbf"
   ELSE
      cFile := ::cFile
   ENDIF

   IF !HB_FileExists( cFile )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "File does not exist: [" + cFile + "]. Translator stopped."
         :Operation   := "WTranslator:OpenDbf()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN .F.
   ENDIF

   IF !::lRebuild
      USE (cFile) ALIAS Translator SHARED VIA "DBFCDX" NEW
   ELSE
      USE (cFile) ALIAS Translator EXCLUSIVE VIA "DBFCDX" NEW
      Translator->( OrdListRebuild() )
   ENDIF

   SET ORDER TO TAG "ENGLISH"
   SET DELETED ON

   ::cPath := cPath
   ::cFile := cFile

RETURN .T.

//------------------------------------------------------------------------------

METHOD CloseDbf() CLASS ZTranslator

   IF Select( "Translator" ) > 0
      Translator->(DBCloseArea())
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------

METHOD OpenText() CLASS ZTranslator

   LOCAL cPath, cFile

   IF Empty( ::cPath )
      cPath := HB_DirBase() + "data" + hb_OsPathSeparator()
   ELSE
      cPath := ::cPath
   ENDIF

   IF !HB_DirExists( cPath )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Path does not exist. Translator stopped."
         :Operation   := "WTranslator:OpenText()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN .F.
   ENDIF

   IF Empty( ::cFile )
      cFile :=  cPath + Engine:ScriptShortName() + ".txt"
   ELSE
      cFile := ::cFile
   ENDIF

   IF !HB_FileExists( cFile ) .AND. !HB_MemoWrit( cFile, "" )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "Can not create Fichero: [" + cFile + "]. Translator stopped."
         :Operation   := "WTranslator:OpenText()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
      RETURN .F.
   ENDIF

   ::cPath := cPath
   ::cFile := cFile

   aLines := HB_ATokens( HB_MemoRead( cFile ), Hb_eol() )
   nLines := Len( aLines )

RETURN .T.

//------------------------------------------------------------------------------

METHOD CloseText() CLASS ZTranslator

   LOCAL cLine, cText

   IF Len( aLines ) == nLines
      RETURN .t.
   ENDIF

   cText := ""

   ASort( aLines,,, {|x,y| Upper( x ) < Upper( y ) } )

   FOR EACH cLine IN aLines
      cText += cLine + hb_eol()
   NEXT

RETURN HB_MemoWrit( ::cFile, cText, .F. )

//------------------------------------------------------------------------------

METHOD JsBottom() CLASS ZTranslator

   LOCAL cJs

   TEXT INTO cJs
      const currentLang = localStorage.getItem('lang') || document.documentElement.lang.toUpperCase() ;
      const switchLang = document.querySelector('#lang-switch');

      const setLang = (lang) => {
        switchLang.innerText = lang;
        localStorage.setItem('lang', lang);
        xa_setCookie("lang", lang, {'Expires': "Fri, 31 Dec 9999 23:59:59 GMT"});
      }

      if (switchLang) {
       setLang(currentLang);
       switchLang.addEventListener('click', e => {
         const lang = ( switchLang.innerText == 'EN' ? 'ES' : 'EN');
         e.preventDefault();
         setLang(lang);
         location.reload();
        });
      }
   ENDTEXT

RETURN cJs

//------------------------------------------------------------------------------

METHOD Translate( cText ) CLASS ZTranslator

   IF lReady .AND. !Empty( cText )
      SWITCH ::cEngine
      CASE "dbf"
         cText := ::TranslateDbf( cText )
         EXIT
      CASE "sqlite"
         cText := ::TranslateSqlite( cText )
         EXIT
      CASE "text"
         cText := ::TranslateText( cText )
         EXIT
      END SWITCH
   ENDIF

RETURN cText

//------------------------------------------------------------------------------

METHOD TranslateDbf( cText ) CLASS ZTranslator

   LOCAL nTimeOut := 5
   LOCAL lOk := .F.

   IF Translator->( DBSeek( cText ) )
      IF !Empty( Translator->Spanish )
         cText := Translator->Spanish
      ENDIF
   ELSE
      DO WHILE nTimeOut > 0
         Translator->( DBAppend() )
         IF !NetErr()
            lOk := .t.
            EXIT
         ENDIF
         hb_idleSleep( 0.5 )
         nTimeOut -= 0.5
      ENDDO
      IF lOk
         Translator->English := cText
         Translator->( DBGoTo( RecNo() ) )
      ENDIF
   ENDIF

RETURN cText

//------------------------------------------------------------------------------

METHOD TranslateText( cText ) CLASS ZTranslator

   LOCAL cTmp
   LOCAL nAt

   nAt := AScan( aLines, {|v| v = cText + "==" } )

   IF nAt > 0
      cTmp:= HB_TokenGet( aLines[ nAt ], 2, "==" )
      IF !Empty( cTmp ) // not translated yet
         cText := cTmp
      ENDIF
   ELSEIF !Empty( cText )
      AAdd( aLines, cText + "==" )
   ENDIF

RETURN cText

//------------------------------------------------------------------------------

METHOD TranslateArray( aText ) CLASS ZTranslator

   LOCAL cText

   FOR EACH cText IN aText
      cText := ::Translate( cText )
   NEXT

RETURN aText

//------------------------------------------------------------------------------
