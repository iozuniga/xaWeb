

#define _HB_API_INTERNAL_
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbvm.h"
#include "hbstack.h"

static PHB_ITEM pBlock = NULL;
void (*__msgSetData)() = NULL;

//------------------------------------------------------------------------------

static void x__msgSetData()
{
   __msgSetData();
   if( pBlock ) {
      PHB_ITEM pSelf = hb_stackSelfItem();
      const char *szData = hb_stackBaseItem()->item.asSymbol.value->szName;
      PHB_ITEM pValue = hb_param( 1, HB_IT_ANY );

      hb_vmPushEvalSym();
      hb_vmPush( pBlock );
      hb_vmPush( pSelf );
      hb_vmPushString( &szData[ 1 ], strlen( szData ) - 1 );
      hb_vmPush( pValue );
      hb_vmDo( 3 ) ;
      hb_itemReturn( pValue );
   }
}

//------------------------------------------------------------------------------

HB_FUNC( SETCLASSHOOK )
{
   PHB_ITEM pNewBlock = hb_param( 1, HB_IT_BLOCK );

   hb_itemReturn( pBlock );
   if( pNewBlock || hb_pcount() )
      {
      if( pBlock )
         hb_gcGripDrop( pBlock );
      pBlock = HB_ISNIL( 1 ) ? NULL : hb_gcGripGet( pNewBlock );

      if( pBlock && ( __msgSetData == NULL ) )
         {
         PHB_ITEM pObj;
         PHB_DYNS pSym;
         PHB_SYMB pSymbol, pMethod;

         pSym = hb_dynsymFind( "HBCLASS" );
         pSymbol = hb_dynsymFindSymbol( "_HCLASS" );
         if( pSym && pSymbol )
            {
            hb_vmPushDynSym( pSym );
            hb_vmPushNil();
            hb_vmDo( 0 );
            pObj = hb_param( -1, HB_IT_OBJECT );
            if( pObj )
               {
               pMethod = hb_objGetMethod( pObj, pSymbol, NULL );
               if( pMethod )
                  {
                  __msgSetData = pMethod->value.pFunPtr;
                  pMethod->value.pFunPtr = &x__msgSetData;
                  }
               }
            }
         }
      }
}

//------------------------------------------------------------------------------
