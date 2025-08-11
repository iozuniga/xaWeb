/*
 * Proyecto: xaWeb framework
 * Fichero: Util.prg
 * Descripción: General purpose routines
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "hbsocket.ch"
#include "error.ch"

#define EOT                         hb_BChar( 4 )

#xtranslate DECLARE FUNCTION <name>( <Params,...> ) =>
#xtranslate ToString( <x> ) => hb_ValToStr( <x>, .T. )

//------------------------------------------------------------------------------

FUNCTION SocketRequest( cAddress, nPort, cCommand, nTimeout, lAbort )  // --> cString

   LOCAL hSocket
   LOCAL aAddress
   LOCAL cBuffer, cTxt
   LOCAL nTime, nEnd, nLen, nAt

   HB_Default( @lAbort, .T. )
   HB_Default( @nTimeout, 200 )

   IF !HB_TokenCount( cAddress, "." ) == 4
      aAddress := HB_SocketResolveINetAddr( cAddress, nPort )
   ELSE
      aAddress := { HB_SOCKET_AF_INET, cAddress, nPort }
   ENDIF

   IF HB_IsNIL( aAddress )
      IF lAbort
         WITH OBJECT ErrorNew()
            :Severity    := ES_ERROR
            :Description := "Invalid address " + cAddress
            :Operation   := "SocketRequest()"
            Eval( ErrorBlock(), :__WithObject(), 1 )
         END WITH
      ELSE
         cCommand := "Invalid address " + cAddress
      ENDIF
      RETURN ""
   ENDIF

   hSocket := hb_socketOpen()

   IF Empty( hSocket )
      IF lAbort
         WITH OBJECT ErrorNew()
            :Severity    := ES_ERROR
            :Description := "socket create error " + HB_SocketErrorString()
            :Operation   := "SocketRequest()"
            Eval( ErrorBlock(), :__WithObject(), 1 )
         END WITH
      ELSE
         cCommand := "socket create error " + HB_SocketErrorString()
      ENDIF
      RETURN ""
   ENDIF

   IF !hb_socketConnect( hSocket, aAddress )
      IF lAbort
         WITH OBJECT ErrorNew()
            :Severity    := ES_ERROR
            :Description := "socket connect error on port: " + ToString( nPort )
            :Operation   := "SocketRequest()"
            Eval( ErrorBlock(), :__WithObject(), 1 )
         END WITH
      ELSE
         cCommand := "socket connect error on port: " + ToString( nPort )
      ENDIF
      hb_socketClose( hSocket )
      RETURN ""
   ENDIF

   hb_socketSend( hSocket, cCommand + EOT )

   nTime := hb_MilliSeconds()
   nEnd := nTime + nTimeout
   cTxt := ""
   nAt  := 0

   cBuffer := Space( 65535 )

   DO WHILE nAt == 0 .AND. ( nEnd > nTime )
      nLen := hb_socketRecv( hSocket, @cBuffer,,, 400 )
      IF nLen > 0
         cTxt += Left( cBuffer, nLen )
         nAt := At( EOT, cTxt )
         EXIT
      ENDIF
      nTime += 400
   ENDDO

   hb_socketShutdown( hSocket )
   hb_socketClose( hSocket )

   IF nAt > 0
      cTxt := SubStr( cTxt, 1, nAt - 1 )
   ENDIF

RETURN cTxt

//------------------------------------------------------------------------------

FUNCTION aJoin( aData, cSeparator ) // --> cString

   LOCAL cValue, cTemp

   HB_Default( @cSeparator, hb_eol() )

   cValue := ""

   FOR EACH cTemp IN aData
      cValue += cTemp
      IF !cTemp:__enumIsLast()
         cValue += cSeparator
      ENDIF
   NEXT

RETURN cValue

//------------------------------------------------------------------------------

DECLARE FUNCTION Echo( cHtml )  // --> NIL

//------------------------------------------------------------------------------

DECLARE FUNCTION ToString( xValue )  // --> NIL

//------------------------------------------------------------------------------

DECLARE FUNCTION LogDebug( ... )  // --> NIL

//------------------------------------------------------------------------------

DECLARE FUNCTION LogConsole( ... )  // --> NIL

//------------------------------------------------------------------------------

DECLARE FUNCTION SO()  // --> object

//------------------------------------------------------------------------------

#pragma BEGINDUMP

#include <hbset.h>
#include <hbapi.h>
#include <hbapiitm.h>
#include <hbvm.h>
#include <hbdate.h>
#include <hbstack.h>
#include <hbapicls.h>
#include <hbapierr.h>

//--------------------------------------------------------------------------

HB_FUNC( VARSEQUAL )
{
   int iResult;

   if( hb_itemCompare( hb_param( 1, HB_IT_ANY ),
                       hb_param( 2, HB_IT_ANY ),
                       HB_TRUE, &iResult ) )
      hb_retl( iResult == 0 );
   else
      hb_retl( HB_FALSE );
}

//--------------------------------------------------------------------------

HB_FUNC( FUNCTIONEXISTS )
{
   hb_retl( hb_dynsymFindName( hb_parc( 1 ) ) != NULL );
}

//------------------------------------------------------------------------------

/*
 * TIP Class oriented Internet protocol library
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

//------------------------------------------------------------------------------

HB_FUNC( TIP_URLENCODE )
{
   const char * pszData = hb_parc( 1 );

   if( pszData )
   {
      HB_ISIZ nLen = hb_parclen( 1 );

      if( nLen )
      {
         HB_BOOL bComplete = hb_parldef( 2, HB_TRUE );
         HB_ISIZ nPos = 0, nPosRet = 0;

         /* Giving maximum final length possible */
         char * pszRet = ( char * ) hb_xgrab( nLen * 3 + 1 );

         while( nPos < nLen )
         {
            char cElem = pszData[ nPos ];

            if( cElem == ' ' )
            {
               pszRet[ nPosRet ] = '+';
            }
            else if( ( cElem >= 'A' && cElem <= 'Z' ) ||
                     ( cElem >= 'a' && cElem <= 'z' ) ||
                     ( cElem >= '0' && cElem <= '9' ) ||
                     cElem == '.' || cElem == ',' || cElem == '&' ||
                     cElem == '/' || cElem == ';' || cElem == '_' )
            {
               pszRet[ nPosRet ] = cElem;
            }
            else if( ! bComplete && ( cElem == ':' || cElem == '?' || cElem == '=' ) )
            {
               pszRet[ nPosRet ] = cElem;
            }
            else /* encode! */
            {
               HB_UINT uiVal;
               pszRet[ nPosRet++ ] = '%';
               uiVal = ( ( HB_UCHAR ) cElem ) >> 4;
               pszRet[ nPosRet++ ] = ( char ) ( ( uiVal < 10 ? '0' : 'A' - 10 ) + uiVal );
               uiVal = ( ( HB_UCHAR ) cElem ) & 0x0F;
               pszRet[ nPosRet ] = ( char ) ( ( uiVal < 10 ? '0' : 'A' - 10 ) + uiVal );
            }

            nPosRet++;
            nPos++;
         }

         hb_retclen_buffer( pszRet, nPosRet );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL,
                     HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}

//------------------------------------------------------------------------------

HB_FUNC( TIP_URLDECODE )
{
   const char * pszData = hb_parc( 1 );

   if( pszData )
   {
      HB_ISIZ nLen = hb_parclen( 1 );

      if( nLen )
      {
         HB_ISIZ nPos = 0, nPosRet = 0;

         /* maximum possible length */
         char * pszRet = ( char * ) hb_xgrab( nLen );

         while( nPos < nLen )
         {
            char cElem = pszData[ nPos ];

            if( cElem == '%' && HB_ISXDIGIT( pszData[ nPos + 1 ] ) &&
                                HB_ISXDIGIT( pszData[ nPos + 2 ] ) )
            {
               cElem = pszData[ ++nPos ];
               pszRet[ nPosRet ]  = cElem - ( cElem >= 'a' ? 'a' - 10 :
                                            ( cElem >= 'A' ? 'A' - 10 : '0' ) );
               pszRet[ nPosRet ] <<= 4;
               cElem = pszData[ ++nPos ];
               pszRet[ nPosRet ] |= cElem - ( cElem >= 'a' ? 'a' - 10 :
                                            ( cElem >= 'A' ? 'A' - 10 : '0' ) );
            }
            else
               pszRet[ nPosRet ] = cElem == '+' ? ' ' : cElem;

            nPos++;
            nPosRet++;
         }

         /* this function also adds a zero */
         /* hopefully reduce the size of pszRet */
         hb_retclen_buffer( ( char * ) hb_xrealloc( pszRet, nPosRet + 1 ), nPosRet );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL,
                     HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}


 /* hb_ValToStr() function
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * Second parameter: lAlltrim (Oz Software)
 */

HB_FUNC( HB_VALTOSTR )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem )
   {
      HB_SIZE nLen;
      HB_BOOL bFreeReq, bTrim = HB_ISLOG( 2 );
      char * buffer = hb_itemString( pItem, &nLen, &bFreeReq );
      char * pos = buffer;
      if( bTrim )
      {
         nLen = hb_strRTrimLen( buffer, nLen, HB_TRUE );
         while( nLen && HB_ISSPACE( *pos ) )
         {
            pos++;
            nLen--;
         }
      }

      if( bFreeReq )
         {
         if( pos == buffer )
            hb_retclen_buffer( pos, nLen );
         else
            {
            hb_retclen( pos, nLen );
            hb_xfree( buffer );
            }
         }
      else
         hb_retclen( pos, nLen );

   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1099, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#pragma ENDDUMP
