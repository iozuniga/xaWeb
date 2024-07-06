
#include "XailerWeb.ch"

ANNOUNCE HB_GtSys // evita que se cargue GT_WIN

PROCEDURE Main()

   Application:cTitle := "Login"
   WRouter():New( Application ):Start( "WDocMain" )
   Application:Run()

RETURN

