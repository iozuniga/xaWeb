/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeDatePicker.prg
 * Descripción: class for Materialize date picker
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatDatePicker

/*
Date format options:
Key	Description	Output
d	   Date of the month.	1-31
dd	   Date of the month (2 digits).	01-31
ddd	Day of the week in short form set by the i18n option.	Sun-Sat
dddd	Day of the week in full form set by the i18n option.	Sunday-Saturday
m	   Month of the year.	1-12
mm	   Month of the year (2 digits).	01-12
mmm	Month of the year in short form set by the i18n option.	Jan-Dec
mmmm	Month of the year in full form set by the i18n option.	January-December
yy	   2-digit year.	17
yyyy	4-digit year.	2017
*/

//STATIC cLanguage := "en" // The same language for the app
STATIC hI18n := { => }
STATIC lDeploy := .F.

CLASS ZDatePicker FROM WBasicEdit
PUBLISHED:
   DATA lAutoClose            INIT .F.
   DATA cFormat               INIT ""
   DATA dDefaultDate          INIT NIL
   DATA lDefaultDate          INIT .F.
   DATA lDisableWeekends      INIT .F.
   DATA cDisableDayFn         INIT NIL
   DATA nFirstDay             INIT -1
   DATA dMinDate              INIT nil
   DATA dMaxDate              INIT nil
   DATA nYearRange            INIT 10
   DATA lYearRangeReverse     INIT .F.
   DATA lIsRTL                INIT .F.
   DATA lShowMonthAfterYear   INIT .F.
   DATA lShowDaysInNextAndPreviousMonths INIT .F.
   DATA lShowClearBtn         INIT .F.
   DATA cOnSelect             INIT nil
   DATA cOnOpen               INIT nil
   DATA cOnClose              INIT nil
   DATA cOnDraw               INIT nil

   METHOD i18n()              INLINE hI18n

RESERVED:
   METHOD New( oParent, oOwner, lAuto ) CONSTRUCTOR
PROTECTED:
   METHOD PreProcess()
   METHOD SetLanguage()
ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oOwner, lAuto ) CLASS ZDatePicker

   ::Super:New( oParent, oOwner, lAuto )
   ::AddClass( "date-picker" )

   ::SetLanguage()

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZDatePicker

   LOCAL hOpt := { => }
   LOCAL cId, cJs, cCss

   IF Empty( ::cId )
      cId := ::ValidId()
   ELSE
      cId := ::cId
   ENDIF

   HB_HSet( hOpt, "i18n", hI18n )

   IF !Empty( ::cFormat )
      HB_HSet( hOpt, "format", ::cFormat )
   ENDIF
   IF ::nFirstDay >= 0
      HB_HSet( hOpt, "firstDay", ::nFirstDay )
   ENDIF
   IF ::lAutoClose
      HB_HSet( hOpt, "autoClose", ::lAutoClose )
   ENDIF
   IF ::dDefaultDate != NIL
      HB_HSet( hOpt, "defaultDate", ValToJS( ::dDefaultDate ) )
   ENDIF
   IF ::lDefaultDate
      HB_HSet( hOpt, "defaultDate", ::lDefaultDate )
   ENDIF
   IF ::lDisableWeekends
      HB_HSet( hOpt, "disableWeekends", ::lDisableWeekends )
   ENDIF
   IF !Empty( ::cDisableDayFn )
      HB_HSet( hOpt, "disableDayFn", ::cDisableDayFn )
   ENDIF
   IF !Empty( ::dMinDate )
      HB_HSet( hOpt, "minDate", ValtoJs( ::dMinDate ) )
   ENDIF
   IF !Empty( ::dMaxDate )
      HB_HSet( hOpt, "maxnDate", ValtoJs( ::dMaxDate ) )
   ENDIF
   IF ::nYearRange != 10
      HB_HSet( hOpt, "yearRange", ::nYearRange )
   ENDIF
   IF ::lYearRangeReverse
      HB_HSet( hOpt, "yearRangeReverse", ::lYearRangeReverse )
   ENDIF
   IF ::lIsRTL
      HB_HSet( hOpt, "isRTL", ::lIsRTL )
   ENDIF
   IF ::lShowMonthAfterYear
      HB_HSet( hOpt, "showMonthAfterYear", ::lShowMonthAfterYear )
   ENDIF
   IF ::lShowDaysInNextAndPreviousMonths
      HB_HSet( hOpt, "showDaysInNextAndPreviousMonths", ::lShowDaysInNextAndPreviousMonths )
   ENDIF
   IF ::lShowClearBtn
      HB_HSet( hOpt, "showClearBtn", ::lShowClearBtn )
   ENDIF
   IF !Empty( ::cOnSelect )
      HB_HSet( hOpt, "onSelect", ::cOnSelect )
   ENDIF
   IF !Empty( ::cOnOpen )
      HB_HSet( hOpt, "onOpen", ::cOnOpen )
   ENDIF
   IF !Empty( ::cOnClose )
      HB_HSet( hOpt, "onClose", ::cOnClose )
   ENDIF
   IF !Empty( ::cOnDraw )
      HB_HSet( hOpt, "onDraw", ::cOnDraw )
   ENDIF

   cJs := " M.Datepicker.init(document.getElementById('" + cId + "')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
   Document:oContext:AddCodeOnLoad( cJs )

   IF !lDeploy .OR. Document:lRender2 // bug en materialize 2.1.1
      TEXT INTO cCss TABS 3
         .datepicker-controls .select-month input {
            padding: 20px 0px 0px;
         }
         .datepicker-controls .select-year input {
            padding: 20px 2px 0px;
         }
      ENDTEXT
      Document:AddCss( cCss )
      lDeploy := .T.
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD SetLanguage() CLASS ZDatePicker

   SWITCH Document:oContext:cLanguage
   CASE "en"
      hb_hset( hI18n, "cancel", "Cancel" )
      hb_hset( hI18n, "clear", "Clear" )
      hb_hset( hI18n, "done", "Ok" )
      hb_hset( hI18n, "previousMonth", "<" )
      hb_hset( hI18n, "nextMonth", "<" )
      hb_hset( hI18n, "months", {'January','February','March','April','May','June','July','August','September','October','November','December'} )
      hb_hset( hI18n, "monthsShort", {'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'} )
      hb_hset( hI18n, "weekDays", {'Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'} )
      hb_hset( hI18n, "weekdaysShort", {'Sun','Mon','Tue','Wed','Thu','Fri','Sat'} )
      hb_hset( hI18n, "weekdaysAbbrev", {'S','M','T','W','T','F','S'} )
      IF Empty( ::cFormat )
         ::cFormat := "mmm dd, yyyy"
      ENDIF
      IF ::nFirstDay == -1
         ::nFirstDay := 0
      ENDIF
      EXIT
   CASE "es"
      hb_hset( hI18n, "cancel", "Cancelar" )
      hb_hset( hI18n, "clear", "Borrar" )
      hb_hset( hI18n, "done", "Aceptar" )
      hb_hset( hI18n, "previousMonth", "<" )
      hb_hset( hI18n, "nextMonth", "<" )
      hb_hset( hI18n, "months", {'Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto','Septiembre','Octubre','Noviembre','Diciembre'} )
      hb_hset( hI18n, "monthsShort", {'Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dec'} )
      hb_hset( hI18n, "weekDays", {'Domingo','Lunes','Martes','Miércoles','Jueves','Viernes','Sabado'} )
      hb_hset( hI18n, "weekdaysShort", {'Dom','Lun','Mar','Mie','Jue','Vie','Sab'} )
      hb_hset( hI18n, "weekdaysAbbrev", {'D','L','M','M','J','V','S'} )
      IF Empty( ::cFormat )
         ::cFormat := "dd mmm, yyyy"
      ENDIF
      IF ::nFirstDay == -1
         ::nFirstDay := 1
      ENDIF
      EXIT
   END SWITCH

RETURN hI18n

//------------------------------------------------------------------------------

