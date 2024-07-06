
#include "XailerWeb.ch"

ANNOUNCE HB_GtSys // evita que se cargue GT_WIN

PROCEDURE Main()

   Application:cTitle := "Simple CSS"
   WRouter():New( Application ):Start( "WDocMain" )
   Application:Run()

RETURN

