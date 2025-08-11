/*
 * Proyecto: xaWeb framework
 * Fichero: ZScript.prg
 * Descripción: Scripts control class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
*/

#include "xaWeb.ch"
#include "error.ch"

CLASS ZScript
PUBLISHED:
   DATA lAsync          INIT .F.
   DATA cCrossOrigin    INIT ""  VALUES "anonymous", "use-credentials"
   DATA lDefer          INIT .F.
   DATA lBottom         INIT .T.
   DATA lNoModule       INIT .F.
   DATA cIntegrity      INIT ""
   DATA cName           INIT ""  // if a name is given it will try to render /js/<name>.js
                                 // if exists. Code inside Script should be deterministic
                                 // Is programmer responsability to update de exteranl JS file

   DATA cReferrerPolicy INIT ""  VALUES "no-referrer",;
                                        "no-referrer-when-downgrade",;
                                        "origin",;
                                        "origin-when-cross-origin",;
                                        "same-origin",;
                                        "strict-origin",;
                                        "strict-origin-when-cross-origin",;
                                        "unsafe-url"
   DATA cUrl            INIT ""
   DATA cCode           INIT ""
   DATA cType           INIT ""  VALUES "text/javascript", "module",;
                                        "importmap", "speculationrules"

   METHOD New( cText, cName OPTIONAL, lUri OPTIONAL) CONSTRUCTOR
   METHOD AddCode( cSource, lTop )  // --> lValue

RESERVED:
   DATA cSha1           INIT ""
   METHOD Html()
   METHOD End() VIRTUAL

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cText, cName, lUri ) CLASS ZScript

   DEFAULT cName TO ""
   DEFAULT lUri  TO !( hb_eol() $ cText ) .AND. !( "FUNCTION" $ Upper( cText ) )

   IF lUri
      ::cUrl := cText
   ELSE
      ::cCode := cText
   ENDIF

   ::cSha1 := HB_SHA1( cText )
   ::cName := cName

RETURN Self

//------------------------------------------------------------------------------

METHOD AddCode( cSource, lTop ) CLASS ZScript

   DEFAULT lTop TO .f.

   IF !Empty( ::cUrl )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := 'Source code can only be added to "inline" scripts'
         :Operation   := "WScript:AddCode()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
         RETURN .f.
      END WITH
   ENDIF

   IF lTop
      ::cCode := cSource + ::cCode
   ELSE
      ::cCode += cSource
   ENDIF

RETURN .T.

//------------------------------------------------------------------------------

METHOD Html() CLASS ZScript

   LOCAL cFile, cLine, cBuffer :=  HTML_SPACES + '<script'

   IF ::lNoModule
      cBuffer += " nomodule"
   ENDIF

   IF !Empty( ::cType )
      cBuffer += ' type="' + ::cType + '"'
   ENDIF

   IF !::lBottom
      IF ::lDefer
         cBuffer += ' defer'
      ELSEIF ::lAsync
         cBuffer += ' async'
      ENDIF
   ENDIF

   IF !Empty( ::cCrossOrigin )
      cBuffer += ' crossorigin="' + ::cCrossOrigin + '"'
   ENDIF

   IF !Empty( ::cIntegrity )
      cBuffer += ' integrity="' + ::cIntegrity + '"'
   ENDIF

   IF !Empty( ::cReferrerPolicy )
      cBuffer += ' referrerpolicy="' + ::cReferrerPolicy + '"'
   ENDIF

   IF !Empty( ::cName )
      cFile := "/js/" + ::cName + ".js"
      IF Document:wwwFileExists( cFile )
         ::cUrl  := cFile
         ::cCode := ""
      ENDIF
   ENDIF

   IF !Empty( ::cUrl )
      cBuffer += ' src="' + ::cUrl + '">'
   ELSE
      cBuffer += '>' + hb_eol()
   ENDIF

   IF !Empty( ::cCode )
      Document:nIndent ++
      IF ::lDefer
         cBuffer += HTML_SPACES + 'addEventListener("DOMContentLoaded", (e) => {' + hb_eol()
         Document:nIndent ++
      ENDIF

      FOR EACH cLine IN HB_ATokens( ::cCode, hb_eol() )
         IF !Empty( cLine ) .AND. !Hb_LeftEq( LTrim( cLine ), "//" )
            cBuffer += HTML_SPACES + cLine + hb_eol()
         ENDIF
      NEXT

      IF ::lDefer
         Document:nIndent --
         cBuffer += HTML_SPACES +"});" + hb_eol()
      ENDIF

      Document:nIndent --
      cBuffer += HTML_SPACES + '</script>' + hb_eol()
   ELSE
      cBuffer += '</script>' + hb_eol()
   ENDIF

RETURN cBuffer

//------------------------------------------------------------------------------