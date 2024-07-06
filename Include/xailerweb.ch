/*
 * Proyecto: XailerWeb framework
 * Fichero: xailerweb.ch
 * Descripción: Cabecera para proyectos xailerweb
 * Autor: Ignacio Ortiz de Zuñiga
 * Copyright 2024 Ignacio Ortiz de Zúñiga
 * Copyright 2024 Oz Software
 */

#ifndef _XAILERWEB_CH_
#define _XAILERWEB_CH_

#define STDIN   0
#define STDOUT  1
#define ERROR_SUBSYSTEM  "XAILERWEB"
#define TAB "   " // Actual IDE Xailer TAB on editor
#define TAB_LENGTH      2
#define HTML_SPACES     Space( TAB_LENGTH * Document:nIndent )

#define OPER_DEFAULT    0
#define OPER_ACTION     1
#define OPER_FORM       2
#define OPER_SERVICE    3
#define OPER_SERVICEJS  4

#xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand THROW(<oErr>) => (Eval(ErrorBlock(), <oErr>), Break(<oErr>))
#xcommand FINALLY => ALWAYS
#xcommand TEXT INTO <v> [TABS <nTabs> ] => #pragma __text|<v>+=XW_Trim(%s, <nTabs> )+hb_eol();<v>:=""
#xcommand TEXT INTO <v> ADDITIVE => #pragma __text|<v>+=XW_Trim(%s)+hb_eol()
#xcommand FILE <f> INTO <v> => #pragma __binarystreaminclude <f>|<v>:=%s
#xcommand ECHO <x> INTO <o> => WText():New( <o> ):cText := <x>
#xcommand ECHO <x> => WText():New( SO ):cText := <x>

// todo: cambiar TEXT .. INTO por HTMLTEXT INTO cuando lo soporte el IDE

#xtranslate LogDebug( <Params,...> )   => Engine:LogForDebug( <Params> )
#xtranslate LogConsole( <Params,...> ) => Engine:JsConsole( <Params> )
#xtranslate StdOut( <x> ) => FWrite( STDOUT, <x> )
#xtranslate RESERVED FUNCTION => FUNCTION
#xtranslate RESERVED: => EXPORT:
#xtranslate PUBLIC: => EXPORT:
#xtranslate AS CLASS <!name!> =>
#xtranslate SO =>  :__WithObject()       // Stack object

MEMVAR Application AS CLASS TApp
MEMVAR Engine      AS CLASS TEngine
MEMVAR Document    AS CLASS TDoc

#include "hbclass.ch"
#include "common.ch"

#xcommand EVENT <!cEvent!>( [ <Params,...> ] ) => ;
   _HB_MEMBER {ev_<cEvent>} ;;
   _HB_MEMBER __HB_CLS_ASFUNC(<cEvent>) ;;
   _HB_MEMBER __HB_CLS_ASFUNC(_<cEvent>) ;;
   oClass:AddMultiData(NIL, NIL, HB_OO_CLSTP_EXPORTED,{"ev_" + <"cEvent">}, __HB_CLS_NOINI );;
   oClass:AddInline( <(cEvent)>, {|Self, ...| XW_RouteEvent( Self, Self:ev_<cEvent>, ...) }, HB_OO_CLSTP_EXPORTED ) ;;
   oClass:AddInline( "_" + <(cEvent)>, {|Self, Param| Self:AddJsEvent(<"cEvent">, Param ) }, HB_OO_CLSTP_EXPORTED )


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
      oClass:AddInline( "_" + <(DataName)>, {|Self, Param| XW_SetDataValue( Self, "f"+<"DataName">, Self:enum_<DataName>, Param ) }, __HB_CLS_SCOPE( .F., .F., .F. ) );;
      oClass:AddInline(  <(DataName)>, {|Self| Self:f<DataName> }, __HB_CLS_SCOPE( .F., .F., .F. ) ) ;;

#endif