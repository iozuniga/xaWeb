
#include "xaWeb.ch"

ANNOUNCE HB_GtSys // prevents GT_WIN from loading, evita que se cargue GT_WIN

PROCEDURE Main()

   #pragma TEXTHIDDEN(1)
   App:cTitle := "MatCollapsibleWin"
   App:cSerial := ""
   App:cAppKey := ""
   #pragma TEXTHIDDEN(0)
   WRouter():New( App ):Start( "WDocMain" )
   App:Run()

RETURN
