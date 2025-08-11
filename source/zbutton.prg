/*
 * Proyecto: xaWeb framework
 * Fichero: ZButton.prg
 * Descripción: HTML button class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZButton FROM WControl

PUBLISHED:
   DATA cType           INIT "button" VALUES "button", "reset", "submit", "cancel", "hidden"
   DATA cText           INIT "Click Me!"
   DATA cValue          INIT ""
   DATA cName           INIT ""
   DATA cForm           INIT ""
   DATA cFormAction     INIT ""
   DATA cFormEncType    INIT "" VALUES "application/x-www-form-urlencoded",;
                                       "multipart/form-data", "text/plain"
   DATA cFormMethod     INIT "" VALUES "get", "post"
   DATA cFormTarget     INIT "" VALUES "_blank", "_self", "_parent", "_top",;
                                       "framename"
   DATA cPopOverTarget  INIT ""
   DATA cPopOverTargetAction INIT "" VALUES "show", "hide", "toggle"

   DATA lDisabled       INIT .F. PERSISTENT
   DATA lFormNoValidate INIT .F.

   METHOD ShowSection( cSection )  INLINE '<script>xa_showSection("' + cSection + '", true);</script>'

RESERVED:
   METHOD Preprocess()
   METHOD HtmlTagBody()

PROTECTED:
   DATA cTag            INIT "button"

ENDCLASS

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS ZButton

   LOCAL oForm, oSection

   IF ::cType == "cancel"
      ::cType := "button"
      IF !::IsEvent( "onclick" )
         IF !Empty( ::cForm )
            oForm := Document:SearchControl( ::cForm )
         ELSE
            oForm := ::GetForm()
         ENDIF
         IF HB_IsObject( oForm )
            oSection := oForm:GetSection()
            IF HB_IsObject( oSection ) .AND. oForm:lFetch
               ::OnClick := '<script>xa_showSection("' + oSection:cId + '", false, "' + oForm:cId + '");</script>'
            ELSE
               ::OnClick := '<script>history.back();</script>'
            ENDIF
         ENDIF
      ENDIF
   ENDIF

RETURN ::Super:Preprocess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZButton

   LOCAL cHtml

   cHtml := ' type="' + ::cType + '"'

   IF !Empty( ::cValue )
      cHtml += ' value="' + ::cValue + '"'
   ENDIF

   IF !Empty( ::cForm )
      cHtml += ' form="' + ::cForm + '"'
   ENDIF

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

   IF !Empty( ::cFormAction )
      cHtml += ' formaction="' + ::cFormAction + '"'
   ENDIF

   IF !Empty( ::cFormEncType )
      cHtml += ' formenctype="' + ::cFormEncType + '"'
   ENDIF

   IF !Empty( ::cFormMethod )
      cHtml += ' formmethod="' + ::cFormMethod + '"'
   ENDIF

   IF !Empty( ::cFormTarget )
      cHtml += ' formtarget="' + ::cFormTarget + '"'
   ENDIF

   IF !Empty( ::cPopOverTarget )
      cHtml += ' popovertarget="' + ::cPopOverTarget + '"'
   ENDIF

   IF !Empty( ::cPopOverTargetAction )
      cHtml += ' popovertargetaction="' + ::cPopOverTargetAction + '"'
   ENDIF

   IF ::lFormNoValidate
      cHtml += ' formnovalidate'
   ENDIF

   IF ::lDisabled
      cHtml += ' disabled'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------
