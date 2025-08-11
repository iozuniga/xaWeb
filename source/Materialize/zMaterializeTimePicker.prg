/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeDatePicker.prg
 * Descripción: class for Materialize time picker
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatTimePicker


STATIC hI18n := { => }
STATIC lDeploy := .F.

CLASS ZTimePicker FROM WBasicEdit
PUBLISHED:
   DATA nDuration       INIT 350
   DATA lShowClearBtn   INIT .F.
   DATA cDefaultTime    INIT "now"
   DATA nFromNow        INIT 0
   DATA lAutoClose      INIT .F.
   DATA lTwelveHour     INIT .F.
   DATA lVibrate        INIT .T.
   DATA cOnOpenStart    INIT nil
   DATA cOnOpenEnd      INIT nil
   DATA cOnCloseStart   INIT nil
   DATA cOnCloseEnd     INIT nil
   DATA cOnSelect       INIT nil

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
   METHOD PreProcess()
   METHOD SetLanguage()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZTimePicker

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "timepicker" )

   ::SetLanguage()

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTimePicker

   LOCAL hOpt := { => }
   LOCAL cId, cJs, cCss

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   HB_HSet( hOpt, "i18n", hI18n )

   IF ::nDuration != 350
      HB_HSet( hOpt, "duration", ::nDuration )
   ENDIF
   IF ::lShowClearBtn
      HB_HSet( hOpt, "showClearBtn", ::lShowClearBtn )
   ENDIF
   IF ::cDefaultTime != "now"
      HB_HSet( hOpt, "defaultTime", ::cDefaultTime )
   ENDIF
   IF ::nFromNow != 0
      HB_HSet( hOpt, "fromNow", ::nFromNow )
   ENDIF
   IF !::lTwelveHour
      HB_HSet( hOpt, "twelveHour", ::lTwelveHour )
   ENDIF
   IF !::lVibrate
      HB_HSet( hOpt, "vibrate", ::lVibrate )
   ENDIF
   IF ::lAutoClose
      HB_HSet( hOpt, "autoClose", ::lAutoClose )
   ENDIF
   IF !Empty( ::cOnOpenStart )
      HB_HSet( hOpt, "onOpenStart", ::cOnOpenStart )
   ENDIF
   IF !Empty( ::cOnOpenEnd )
      HB_HSet( hOpt, "onOpenEnd", ::cOnOpenEnd )
   ENDIF
   IF !Empty( ::cOnCloseStart )
      HB_HSet( hOpt, "onCloseStart", ::cOnCloseStart )
   ENDIF
   IF !Empty( ::cOnCloseEnd )
      HB_HSet( hOpt, "onCloseEnd", ::cOnCloseEnd )
   ENDIF
   IF !Empty( ::cOnSelect )
      HB_HSet( hOpt, "onSelect", ::cOnSelect )
   ENDIF

   cJs := " M.Timepicker.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

   IF !lDeploy .OR. Document:lRender2 // bug en materialize  2.1.1
      TEXT INTO cCss TABS 3
         @media only screen and (max-width: 600px) {
            .timepicker-digital-display {
            width: 100%;
            }
         }
         @media only screen and (min-width: 601px) {
            .timepicker-digital-display {
            width: 40%;
            }
         }
         .timepicker-text-container input[type=text] {
         color: var(--md-sys-color-on-background);
         border-bottom: 0px!important;
         font-size: 3rem;
         padding: 0px 2px!important;
         }
         .timepicker-display-am-pm {
            right: 0rem;
         }
      ENDTEXT
      Document:AddCss( cCss )
      lDeploy := .T.
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD SetLanguage() CLASS ZTimePicker

   SWITCH Document:oContext:cLanguage
   CASE "en"
      hb_hset( hI18n, "cancel", "Cancel" )
      hb_hset( hI18n, "clear", "Clear" )
      hb_hset( hI18n, "done", "Ok" )
      EXIT
   CASE "es"
      hb_hset( hI18n, "cancel", "Cancelar" )
      hb_hset( hI18n, "clear", "Borrar" )
      hb_hset( hI18n, "done", "Aceptar" )
      EXIT
   END SWITCH

RETURN hI18n

//------------------------------------------------------------------------------

