/*
 * Proyect: XailerWeb framework
 * File: zModalMsgBtn.prg
 * Description: Modal Message-button package
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 */

#include "xailerweb.ch"
#include "error.ch"

CLASS ZModalMsgBtn FROM WPackage
EXPORTED:
   DATA cName INIT "xw_ModalMsgBtn" READONLY

   METHOD New( oDoc, nPos )   CONSTRUCTOR

   METHOD ShowModal( cTitle, cBody, aButtons )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oDoc, nPos ) CLASS ZModalMsgBtn

   IF !oDoc:IsKindOf( "WDoc" )
      WITH OBJECT ErrorNew()
         :Subsystem   := ERROR_SUBSYSTEM
         :Severity    := ES_ERROR
         :Description := "oDoc parameter should be a WDoc object"
         :Operation   := "WModalMsgBtn:New( oDoc, nPos )"
         Eval( ErrorBlock(), :__WithObject(), 3 )
      END WITH
   ENDIF

   ::Super:New( oDoc, nPos )

   IF ::nIndex > 0
      ::AddCSS( "https://fonts.googleapis.com/icon?family=Material+Icons" )
      ::AddCSS( Css(), ::cName )
      ::AddScript( Script(), ::cName )
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD ShowModal( cTitle, cBody, aButtons ) CLASS ZModalMsgBtn

   LOCAL obj := ZModalObject():New( Self )

   DEFAULT cTitle TO "Tittle", cBody TO "body", aButtons TO {}

   WITH OBJECT obj
      :cTitle   := cTitle
      :cBody    := cBody
      :aButtons := aButtons
   END WITH

RETURN obj

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZModalObject STATIC
EXPORTED:
   DATA oParent   INIT NIL
   DATA cTitle    INIT ""
   DATA cBody     INIT ""
   DATA aButtons  INIT {}
   DATA aOnClick  INIT {}
   DATA aOnClose  INIT {}

   METHOD New( oParent )      CONSTRUCTOR

   METHOD OnClick( nButton )
   METHOD OnClose( nButton )

RESERVED:
   METHOD GenJs( cEvent )
   METHOD Html()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZModalObject

   ::oParent := oParent

RETURN Self

//------------------------------------------------------------------------------

METHOD OnClick( nButton, xValue ) CLASS ZModalObject

   IF Len( ::aOnClick ) < nButton
      ::aOnClick := ASize( ::aOnClick, nButton )
   ENDIF

   ::aOnClick[ nButton ] := xValue

RETURN nil

//------------------------------------------------------------------------------

METHOD OnClose( nButton, lClose ) CLASS ZModalObject

   IF Len( ::aOnClose ) < nButton
      ::aOnClose := ASize( ::aOnClose, nButton )
   ENDIF

   ::aOnClose[ nButton ] := lClose

RETURN nil

//------------------------------------------------------------------------------

METHOD GenJs( cEvent ) CLASS ZModalObject

   LOCAL cHtml

   IF Empty( cEvent )
      RETURN "onClick:(e)=>{}"
   ENDIF

   IF HB_IsObject( cEvent )
      cEvent := cEvent:Html()
   ENDIF

   IF hB_LeftEqi( cEvent, "<script>" )
      cEvent := JSReducer( cEvent )
      cEvent := StrTran( cEvent, "'", "`" )
      cEvent := StrTran( cEvent, '"', "`" )
      cHtml := "onClick:(e)=>{" + cEvent + "}"
   ELSE
      IF __objHasMethod( Document, cEvent )
         cHtml := 'onClick:(e)=>{xw_runAction(null,`' + cEvent + '`, e);}'
      ELSEIF !( Right( cEvent, 1 ) $ "});" )
         cHtml := 'onClick:(e)=>{' + cEvent + '();}'
      ELSE
         cHtml := 'onClick:(e)=>{' + cEvent + '}'
      ENDIF
   ENDIF

RETURN cHtml

//------------------------------------------------------------------------------

METHOD Html() CLASS ZModalObject

   LOCAL xValue
   LOCAL cJs, cEvent
   LOCAL nLen
   LOCAL lClose

   nLen       := Len( ::aButtons )
   ::aOnClick := ASize( ::aOnClick, nLen )
   ::aOnClose := ASize( ::aOnClose, nLen )

   AEval( ::aOnClose, {|v,e| IIF( !HB_IsLogical(v), ::aOnClose[e] := .T., ) } )

   cJs := 'xw_showModal("' + ::cTitle + '", "' + ::cBody + '", ['

   FOR EACH xValue, cEvent, lClose IN ::aButtons, ::aOnClick, ::aOnClose
      IF !Empty( xValue )
         IF HB_IsObject( xValue )
            xValue := xValue:Html()
         ENDIF
         cJs += '{label:"' + xValue + '",' + ;
                 ::GenJs( cEvent ) + ;
                 ',triggerClose: ' + IIF( lClose, "true", "false" ) + '}'
         IF !xValue:__enumIsLast()
            cJs += ","
         ENDIF
      ENDIF
   NEXT

   cJs += + '])'

RETURN cJs

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

// Credits: https://dev.to/dcodeyt

STATIC FUNCTION Css()

   LOCAL cCss

   TEXT INTO cCss
   .modal {
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
     padding: var(--gap) || 15px;
     background: rgba(0, 0, 0, 0.5);
   }

   .modal__inner {
     background: #ffffff;
     color: DimGray;
     width: 100%;
     max-width: 800px;
     overflow: hidden;
     border-radius: 4px;
   }

   .modal__top {
     display: flex;
     align-items: center;
     background-color: #eeeeee;
     color: black;
   }

   .modal__title {
     flex-grow: 1;
     padding: 0 var(--gap);
     font-size: 20px;
     color: black;
   }

   .modal__close {
     display: flex;
     align-items: center;
     cursor: pointer;
     padding: var(--gap);
     background: none;
     border: none;
     outline: none;
     color: black;
   }

   .modal__content {
     padding: 0 var(--gap);
     line-height: 1.5;
   }

   .modal__bottom {
     text-align: right;
     padding: 0 var(--gap) var(--gap) var(--gap);
   }

   .modal__button {
     display: inline-block;
     padding: 6px 12px;
     background: var(--primary-color, #009578);
     color: var(--primary-text-color, #fffff);
     border: none;
     outline: none;
     border-radius: 3px;
     cursor: pointer;
     font-size: 18px;
   }

   .modal__button:not(:last-child) {
     margin-right: var(--gap);
   }

   .modal__button:hover {
     background: var(--primary-hover-color, #008066);
   }
   ENDTEXT

RETURN cCss

//------------------------------------------------------------------------------

STATIC FUNCTION Script()

   LOCAL cScript

   TEXT INTO cScript
   function xw_showModal(titleHtml, contentHtml, buttons) {
     const modal = document.createElement("div");

     modal.classList.add("modal");
     modal.innerHTML = `
           <div class="modal__inner">
               <div class="modal__top">
                   <div class="modal__title">${titleHtml}</div>
                   <button class="modal__close" type="button">
                       <span class="material-icons">close</span>
                   </button>
               </div>
               <div class="modal__content">${contentHtml}</div>
               <div class="modal__bottom"></div>
           </div>
       `;

     for (const button of buttons) {
       const element = document.createElement("button");

       element.setAttribute("type", "button");
       element.classList.add("modal__button");
       element.textContent = button.label;
       element.addEventListener("click", () => {
         if (button.triggerClose) {
           document.body.removeChild(modal);
         }

         button.onClick(modal);
       });

       modal.querySelector(".modal__bottom").appendChild(element);
     }

     modal.querySelector(".modal__close").addEventListener("click", () => {
       document.body.removeChild(modal);
     });

     document.body.appendChild(modal);
   }
   ENDTEXT

RETURN cScript
