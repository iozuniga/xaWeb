/*
 * Proyect: XailerWeb framework
 * File: ZForm.prg
 * Description: Form HTML class
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZForm FROM WContainer
EXPORTED:
   DATA oContainer      AS CLASS WDiv
   DATA cAcceptCharset  INIT "" VALUES "utf-8", "iso-8859-1"
   DATA cAction         INIT ""
   DATA cEncType        INIT "" VALUES "application/x-www-form-urlencoded",;
                                       "multipart/form-data",;
                                       "text/plain"
   DATA cMethod         INIT "get" VALUES "get", "post"
   DATA cName           INIT ""
   DATA cRel            INIT "" VALUES "external", "help", "license", "next",;
                                       "nofollow", "noopener", "noreferrer",;
                                       "opener", "prev", "search"

   DATA cTarget         INIT "" VALUES "_blank", "_self", "_parent", "_top"
   DATA cTableID        INIT ""

   DATA lAutoComplete   INIT NIL
   DATA lNoValidate     INIT .F.
   DATA lModal          INIT .F.
   DATA lFetch          INIT .F.

   METHOD New( oParent ) CONSTRUCTOR

   METHOD SubmitToService( cService, lJson )

PROTECTED:
   DATA cTag            INIT "form"

RESERVED:
   METHOD Preprocess()
   METHOD HtmlTagBody()
   METHOD ModalCss()    INLINE ModalCss()
   METHOD GetSubmit()   INLINE GetSubmit( Self )

END CLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZForm

   ::oContainer := WDiv():New( oParent, nil )
   ::Super:New( ::oContainer, Self )

   ::AddClass( "xw-form__form" )
   ::oContainer:AddClass( "xw-form" )

RETURN Self

//------------------------------------------------------------------------------

METHOD SubmitToService( cService, lJson ) CLASS ZForm

   LOCAL hData := { => }

   DEFAULT lJson TO .F.

   HB_HSet( hData, "service", cService )
   HB_HSet( hData, "content", IIF( lJson, "application/json", "application/javascript" ) )

   ::lFetch := .T.

RETURN '<script>return ' + HB_JsonEncode( hData ) + '</script>'

//------------------------------------------------------------------------------

METHOD Preprocess() CLASS ZForm

   IF Empty( ::cName )
      ::cName := ::cId
   ENDIF

   IF Empty( ::cName )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_WARNING
         :Description := "HTML forms should have a UNIQUE tag 'name' set."
         :Operation   := "WForm:Html()"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

   ::cId := ::cName

   Document:AddScript( 'const fv_' + ::cName + ' = new xw_FormValidator("#' +;
                       ::cId + '");' + hb_eol(),"init", .F. )

   IF ::lModal
      Document:AddCss( ::ModalCss() )
   ENDIF

   IF !Empty( ::cTableId )
      ::AddDataset( "table", ::cTableId )
   ENDIF

RETURN ::Super:Preprocess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZForm

   LOCAL cHtml := ""

   IF !Empty( ::cAcceptCharset )
      cHtml += ' accept-charset="' + ::cAcceptCharset + '"'
   ENDIF

   IF !Empty( ::cAction )
      cHtml += ' action="' + ::cAction + '"'
   ELSEIF !::IsJsEvent( "OnSubmit" )
      cHtml += ' action="' + Engine:BaseUri()+ '?form=' + Document:cName + '-' + ::cName + '"'
      ::OnSubmit := "xw_submit"
   ENDIF

   IF !Empty( ::cEncType )
      cHtml += ' enctype="' + ::cEncType + '"'
   ENDIF

   IF !Empty( ::cMethod )
      cHtml += ' method="' + ::cMethod + '"'
   ENDIF

   IF !Empty( ::cName )
      cHtml += ' name="' + ::cName + '"'
   ENDIF

   IF !Empty( ::cRel )
      cHtml += ' rel="' + ::cRel + '"'
   ENDIF

   IF !Empty( ::cTarget )
      cHtml += ' target="' + ::cTarget + '"'
   ENDIF

   IF HB_IsLogical( ::lAutoComplete )
      cHtml += ' autocomplete="' + IIF( ::lAutoComplete, "on", "off" ) + '"'
   ENDIF

   IF ::lNoValidate
      cHtml += ' novalidate'
   ENDIF

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

STATIC FUNCTION GetSubmit( oControl )

   LOCAL oCtl, oRet

   FOR EACH oCtl IN oControl:aControls
      IF oCtl:IsKindOf( "WButton" ) .AND. oCtl:cType == "submit"
         oRet := oCtl
         EXIT
      ELSEIF oCtl:IsKindOf( "WContainer" )
         oRet := GetSubmit( oCtl )
         IF oRet != NIL
            EXIT
         ENDIF
      ENDIF
   NEXT

RETURN oRet

//------------------------------------------------------------------------------

STATIC FUNCTION ModalCss()

   LOCAL cCss

   TEXT INTO cCss
   .xw-form {
     --gap: 15px;
     position: fixed;
     top: 0;
     left: 0;
     width: 100vw;
     height: 100vh;
     display: flex;
     align-items: center;
     justify-content: center;
     box-sizing: border-box;
     padding: var(--gap,15px);
     background: rgba(0, 0, 0, 0.5);
   }

   .xw-form--hidden {
      display: none;
   }

   .xw-form__form {
     background: var(--body-alt-color, #ffffff);
     color: var(--body-text-color, #000000);
     max-width: 100%;
     max-height: 100svh;
     overflow-y:auto;
     border-radius: 4px;
     padding: 10px 10px;
     box-shadow: 2px 2px 2px 1px rgba(0, 0, 0, 0.4);
   }
   ENDTEXT

RETURN cCss
