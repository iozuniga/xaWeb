/*
 * Proyect: XailerWeb framework
 * File: ZScript.prg
 * Description: Scripts control class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
*/

#include "xailerweb.ch"
#include "error.ch"

CLASS ZScript
EXPORTED:
   DATA lAsync          INIT .F.
   DATA cCrossOrigin    INIT ""  VALUES "anonymous", "use-credentials"
   DATA lDefer          INIT .F.
   DATA lBottom         INIT .T.
   DATA cIntegrity      INIT ""
   DATA lNoModule       INIT NIL
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

   METHOD New( cText, cName, lUri ) CONSTRUCTOR
   METHOD AddCode( cSource )

RESERVED:
   METHOD Html()

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

   ::cName := cName

RETURN Self

//------------------------------------------------------------------------------

METHOD AddCode( cSource ) CLASS ZScript

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

   ::cCode += cSource

RETURN .T.

//------------------------------------------------------------------------------

METHOD Html() CLASS ZScript

   LOCAL cFile, cLine, cBuffer :=  HTML_SPACES + '<script'

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

   IF ::lNoModule != NIL
      cBuffer += ' nomodule="' + IIF(::lNoModule, 'true', 'false' ) + '"'
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