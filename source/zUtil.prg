/*
 * Proyect: XailerWeb framework
 * File: Util.prg
 * Description: General purpose routines (some borrowed from Xailer)
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 José Giménez
 * Copyright 2024 Oz Software
 */

 #include "xailerweb.ch"

//------------------------------------------------------------------------------

FUNCTION aJoin( aData, cSeparator ) // --> cString

   LOCAL cValue, cTemp

   DEFAULT cSeparator TO hb_eol()

   cValue := ""

   FOR EACH cTemp IN aData
      cValue += cTemp
      IF !cTemp:__enumIsLast()
         cValue += cSeparator
      ENDIF
   NEXT

RETURN cValue

//------------------------------------------------------------------------------

#pragma BEGINDUMP

#include <hbset.h>
#include <hbapi.h>
#include <hbapiitm.h>
#include <hbvm.h>
#include <hbdate.h>
#include <hbstack.h>
#include <hbapicls.h>

//------------------------------------------------------------------------------

HB_FUNC( TOSTRING )     // ToString( <xParam>, [<cDefault>], [<lTrimAllDecimals>] ) --> cResult
{
   PHB_ITEM param = hb_param( 1, HB_IT_ANY );
   char cString[ 65 ], *p;
   HB_ULONG nLen;
   HB_BOOL lFree;
   int nInt, nDec;

   if( param )
      {
      switch( hb_itemType( param ) )
         {
         case HB_IT_LOGICAL:
            hb_retc( hb_parl( 1 ) ? ".T." : ".F." );
            break;

         case HB_IT_BLOCK:
            hb_retc( "{|| ... }" );
            break;

         case HB_IT_INTEGER:
         case HB_IT_LONG:
         case HB_IT_DOUBLE:
            hb_itemGetNLen( param, &nInt, &nDec );
            if( hb_itemStrBuf( cString, param, nDec ? 64 : nInt, nDec ? 16 : 0 ) )
               {
               if( nDec )
                  {
                  // Quitar ceros por la derecha (decimales) hasta dejar
                  // <nDec> decimales o ninguno si <lTrimAllDecimals> = .T.
                  nDec = hb_parl( 3 ) ? 16 : 16 - nDec;
                  p = &cString[ 63 ];
                  while( nDec-- && ( *p == '0' ) )
                     *p-- = 0;
                  if( *p == '.' )
                     *p = 0;
                  }
               // Quitar espacios por la izquierda
               p = cString;
               while( *p == ' ' )
                  p++;
               hb_retc( p );
               }
            else
               hb_retc( "" );
            break;

         case HB_IT_NIL:
            hb_retc( hb_pcount() > 1 ? hb_parc( 2 ) : "NIL" );
            break;

         default:
            p = hb_itemString( param, &nLen, &lFree );
            hb_retclen( p, nLen );
            if( lFree )
               hb_xfree( p );
         }
      }
   else
      hb_retc( "NIL" );
}

//------------------------------------------------------------------------------

static HB_BYTE HexToChar( char **pInput )
{
   HB_BYTE c = **pInput;

   (*pInput)++;
   if( c == '%' ) {
      if( ( **pInput >= '0' ) && ( **pInput <= '9' ) )
         c = ( **pInput - '0' ) << 4;
      else if( ( **pInput >= 'A' ) && ( **pInput <= 'F' ) )
         c = ( **pInput - 'A' + 10 ) << 4;
      else if( ( **pInput >= 'a' ) && ( **pInput <= 'f' ) )
         c = ( **pInput - 'a' + 10 ) << 4;
      else
         return '%';

      (*pInput)++;
      if( ( **pInput >= '0' ) && ( **pInput <= '9' ) )
         c |= ( **pInput - '0' );
      else if( ( **pInput >= 'A' ) && ( **pInput <= 'F' ) )
         c |= ( **pInput - 'A' + 10 );
      else if( ( **pInput >= 'a' ) && ( **pInput <= 'f' ) )
         c |= ( **pInput - 'a' + 10 );
      else {
         (*pInput)--;
         return '%';
         }
      (*pInput)++;
   }

   return c;
}

//------------------------------------------------------------------------------

char *stpcpy( char * dest, const char * src )
{
   while( ( *dest = *src++ ) != 0 )
      dest++;

   return dest;
}

//------------------------------------------------------------------------------

HB_FUNC( XA_URLENCODE ) // XA_UrlEncode( <cUrl> ) --> <cUrlUtf8>
{
   static char cDest[][10] = { "%E2%82%AC", "%81", "%E2%80%9A", "%C6%92", "%E2%80%9E", "%E2%80%A6", "%E2%80%A0", "%E2%80%A1", "%CB%86", "%E2%80%B0", "%C5%A0", "%E2%80%B9", "%C5%92", "%C5%8D", "%C5%BD", "%8F",
                               "%C2%90", "%E2%80%98", "%E2%80%99", "%E2%80%9C", "%E2%80%9D", "%E2%80%A2", "%E2%80%93", "%E2%80%94", "%CB%9C", "%E2%84", "%C5%A1", "%E2%80", "%C5%93", "%9D", "%C5%BE", "%C5%B8",
                               "%C2%A0", "%C2%A1", "%C2%A2", "%C2%A3", "%C2%A4", "%C2%A5", "%C2%A6", "%C2%A7", "%C2%A8", "%C2%A9", "%C2%AA", "%C2%AB", "%C2%AC", "%C2%AD", "%C2%AE", "%C2%AF",
                               "%C2%B0", "%C2%B1", "%C2%B2", "%C2%B3", "%C2%B4", "%C2%B5", "%C2%B6", "%C2%B7", "%C2%B8", "%C2%B9", "%C2%BA", "%C2%BB", "%C2%BC", "%C2%BD", "%C2%BE", "%C2%BF",
                               "%C3%80", "%C3%81", "%C3%82", "%C3%83", "%C3%84", "%C3%85", "%C3%86", "%C3%87", "%C3%88", "%C3%89", "%C3%8A", "%C3%8B", "%C3%8C", "%C3%8D", "%C3%8E", "%C3%8F",
                               "%C3%90", "%C3%91", "%C3%92", "%C3%93", "%C3%94", "%C3%95", "%C3%96", "%C3%97", "%C3%98", "%C3%99", "%C3%9A", "%C3%9B", "%C3%9C", "%C3%9D", "%C3%9E", "%C3%9F",
                               "%C3%A0", "%C3%A1", "%C3%A2", "%C3%A3", "%C3%A4", "%C3%A5", "%C3%A6", "%C3%A7", "%C3%A8", "%C3%A9", "%C3%AA", "%C3%AB", "%C3%AC", "%C3%AD", "%C3%AE", "%C3%AF",
                               "%C3%B0", "%C3%B1", "%C3%B2", "%C3%B3", "%C3%B4", "%C3%B5", "%C3%B6", "%C3%B7", "%C3%B8", "%C3%B9", "%C3%BA", "%C3%BB", "%C3%BC", "%C3%BD", "%C3%BE", "%C3%BF" };
   char *pIn, *pOut, *cOut = hb_xgrab( hb_parclen( 1 ) * 9 + 1 );

   pIn = (char *) hb_parc( 1 );
   pOut = cOut;
   while( *pIn ) {
      if( (HB_BYTE) *pIn >= 0x80 )
         pOut = stpcpy( pOut, cDest[ ( (HB_BYTE) *pIn ) - 0x80 ] );
      else if( *pIn == 0x20 )
         pOut = stpcpy( pOut, "%20" );
      else if( *pIn == '%' )
         pOut = stpcpy( pOut, "%25" );
      else if( *pIn > 0x20 )
         *pOut++ = *pIn;
      pIn++;
      }
   *pOut = 0;

   hb_retc_buffer( cOut );
}

//------------------------------------------------------------------------------

HB_FUNC( XA_URLDECODE ) // XA_UrlDecode( <cUrlEncoded> ) --> <cUrlUtf8>
{
   char *pIn = (char *) hb_parc( 1 );
   char *cOut = hb_xgrab( hb_parclen( 1 ) + 1 ), *pOut = cOut;

   while( *pIn ) {
      if( *pIn == '+' ) {
         *pOut++ = ' ';
         pIn++;
         }
      else
         *pOut++ = HexToChar( &pIn );
      }
   *pOut = 0;

   hb_retc_buffer( cOut );
}


//------------------------------------------------------------------------------

HB_FUNC( STRENCODEESCAPE ) // StrEncodeEscape( <cString>, <lForSave> ) --> cResult
{
   int nStrLen = hb_parclen( 1 ), nLen = 0;
   const char *cText = hb_parc( 1 );
   char *cString = hb_xgrab( nStrLen * 2 + 4 ), *pText = cString, c;
   HB_BOOL lForSave = hb_parl( 2 ), lExtended = HB_FALSE;

   if( lForSave )
      {
      *pText++ = 'e';
      *pText++ = '"';
      nLen += 2;
      }

   while( nStrLen-- )
      {
      c = *cText++;
      switch( c )
         {
         case '"':
            if( lForSave )
               {
               lExtended = HB_TRUE;
               *pText++ = '\\';
               nLen++;
               }
            break;
         case 13:
            lExtended = HB_TRUE;
            *pText++ = '\\';
            c = 'r';
            nLen++;
            break;
         case 10:
            lExtended = HB_TRUE;
            *pText++ = '\\';
            c = 'n';
            nLen++;
            break;
         case 9:
            lExtended = HB_TRUE;
            *pText++ = '\\';
            c = 't';
            nLen++;
            break;
         case '\\':
            lExtended = HB_TRUE;
            *pText++ = c;
            nLen++;
            break;
         case 0:
            lExtended = HB_TRUE;
            *pText++ = '\\';
            c = '0';
            nLen++;
            break;
         }
      *pText++ = c;
      nLen++;
      }

   if( lForSave )
      {
      *pText++ = '"';
      nLen++;
      }
   *pText = 0;

   pText = cString;
   if( lForSave && !lExtended )
      {
      nLen--;
      pText++;
      }

   hb_retclen( pText, nLen );
   hb_xfree( cString );
}

//--------------------------------------------------------------------------

HB_FUNC( STRDECODEESCAPE ) // StrDecodeEscape( <cString> ) --> cResult
{
   const char *cString = hb_parc( 1 ), *cText = cString;
   char *cOut = hb_xgrab( hb_parclen( 1 ) + 1 ), *pText = cOut, c;
   int nLen = 0;

   while( ( c = *cText++ ) != 0 )
      {
      if( c == '\\' )
         switch( *cText )
            {
            case '"':
               c = '"';
               cText++;
               break;
            case 'n':
               c = 10;
               cText++;
               break;
            case 'r':
               c = 13;
               cText++;
               break;
            case 't':
               c = 9;
               cText++;
               break;
            case '\\':
               cText++;
               break;
            case '0':
               c = 0;
               cText++;
               break;
            }
      *pText++ = c;
      nLen++;
      }
   *pText = 0;

   hb_retclen_buffer( cOut, nLen );
}

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

#pragma ENDDUMP
