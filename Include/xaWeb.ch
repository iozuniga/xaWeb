/*
 * Proyecto: xaWeb framework
 * Fichero: xaWeb.ch
 * Descripción: Cabecera para proyectos xaWeb
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
*/

#ifndef _XAWEB_CH_
#define _XAWEB_CH_

#define XA_VER_STRING      "1.0.2"
#define XA_VER_MAJOR       1
#define XA_VER_MINOR       0
#define XA_VER_REVISION    2
#define XA_VER_BUILD       102
#define XA_VER_MONTH       7        // Mes de publicacion
#define XA_VER_YEAR        2025     // Año de publicacion

#define ERROR_SUBSYSTEM  "XAWEB"
#define TAB "   "       // Actual IDE Xailer TAB on editor
#define TAB_LENGTH      2
#define HTML_SPACES     Space( TAB_LENGTH * Document:nIndent )
#define SLASH           HB_OsPathSeparator()
#define WIN_EOL         Chr( 13 ) + Chr( 10 )

#define OPER_DEFAULT    0
#define OPER_ACTION     1
#define OPER_FORM       2
#define OPER_SERVICE    3
#define OPER_SERVICEJS  4
#define OPER_CONFIG     5
#define OPER_LISTENING  6
#define OPER_CUSTOM     7

#xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand THROW(<oErr>) => (Eval(ErrorBlock(), <oErr>), Break(<oErr>))
#xcommand FINALLY => ALWAYS
#xcommand TEXT INTO <v> [TABS <nTabs> ] => #pragma __text|<v>+=XA_Trim(%s, <nTabs> )+hb_eol();<v>:=""
#xcommand TEXT INTO <v> ADDITIVE => #pragma __text|<v>+=XA_Trim(%s)+hb_eol()
#xcommand FILE <f> INTO <v> => #pragma __binarystreaminclude <f>|<v>:=%s
#xcommand ECHO <x> INTO <o> => WText():New( <o> ):cText := <x>
#xcommand ECHO <x> => WText():New( SO ):cText := <x>

#xtranslate LogDebug( <Params,...> )   => Engine:LogForDebug( <Params> )
#xtranslate LogConsole( <Params,...> ) => Engine:JsConsole( <Params> )
#xtranslate ToString( <x> ) => hb_ValToStr( <x>, .T. )
#xtranslate RESERVED FUNCTION => FUNCTION
#xtranslate PUBLISHED:        => EXPORT:
#xtranslate RESERVED:         => EXPORT:
#xtranslate OVERLOADED:       => EXPORT: // No need to include in help system
#xtranslate BYREF =>
#xtranslate OPTIONAL =>
#xtranslate AS CLASS <!name!> =>
#xtranslate SO                =>  :__WithObject()       // Stack object
#xtranslate SLASH             => hb_OsPathSeparator()

MEMVAR App      AS CLASS TApp
MEMVAR Engine   AS CLASS TEngine
MEMVAR Document AS CLASS TDoc

#include "hbclass.ch"
#include "common.ch"

#xcommand EVENT <!cEvent!>( [ <Params,...> ] ) => ;
   _HB_MEMBER {ev_<cEvent>} ;;
   _HB_MEMBER __HB_CLS_ASFUNC(<cEvent>) ;;
   _HB_MEMBER __HB_CLS_ASFUNC(_<cEvent>) ;;
   oClass:AddMultiData(NIL, NIL, HB_OO_CLSTP_EXPORTED,{"ev_" + <"cEvent">}, __HB_CLS_NOINI );;
   oClass:AddInline( "_" + <(cEvent)>, {|Self, Param| Self:AddEvent(<"cEvent">, Param ) }, HB_OO_CLSTP_EXPORTED ) ;;
   oClass:AddInline( <(cEvent)>, {|Self, ...| XA_RouteEvent( Self, Self:ev_<cEvent> , ...) }, HB_OO_CLSTP_EXPORTED )

#xtranslate _DATAVALUES_( <value> ) => <"value">

#xtranslate DATA <cData> [ <More1> ] VALUES <value1> [, <valueN> ] [ <MoreN> ] => ;
            DATA <cData> _VALUES_ { <value1> [ , <valueN> ]  } [ <More1> ] [ <MoreN> ]

#xcommand DATA <!DataName!> _VALUES_ <Values> ;
                             [ AS <type> ] ;
                             [ INIT <uValue> ] ;
                             [ <export: EXPORTED, VISIBLE>] ;
                             [<protect: PROTECTED>] ;
                             [<hidde: HIDDEN>] ;
                             [<ro: READONLY, RO>] ;
                             [<persistent: PERSISTENT, PROPERTY>] ;
                             [<sync: SYNC>] => ;
      _HB_MEMBER {enum_<DataName>} ;;
      _HB_MEMBER {[ AS <type>] f<DataName> } ;;
      _HB_MEMBER __HB_CLS_ASFUNC(<DataName>) ;;
      _HB_MEMBER __HB_CLS_ASFUNC(_<DataName>) ;;
      oClass:AddMultiClsData(NIL, [ <Values> ], __HB_CLS_SCOPE( .F., .F., .F. ),{"enum_" + <"DataName">}, __HB_CLS_NOINI );;
      oClass:AddMultiData( <(type)>, <uValue>, __HB_CLS_SCOPE( <.export.>, <.protect.>, <.hidde.> ) + iif( <.ro.>, HB_OO_CLSTP_READONLY, 0 ) + iif( <.persistent.>, __HB_CLS_SCOPE( .F., .F., .F. ), 0 ) + iif( <.sync.>, HB_OO_CLSTP_SYNC, 0 ), {"f"+<"DataName">}, __HB_CLS_NOINI );;
      oClass:AddInline( "_" + <(DataName)>, {|Self, Param| XA_SetDataValue( Self, "f"+<"DataName">, Self:enum_<DataName>, Param ) }, __HB_CLS_SCOPE( .F., .F., .F. ) );;
      oClass:AddInline(  <(DataName)>, {|Self| Self:f<DataName> }, __HB_CLS_SCOPE( .F., .F., .F. ) ) ;;

REQUEST XA_UTIL

#endif