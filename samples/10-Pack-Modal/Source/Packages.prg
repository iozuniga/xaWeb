#include "XailerWeb.ch"

ANNOUNCE HB_GtSys // evita que se cargue GT_WIN

PROCEDURE Main()

   Application:cTitle := "Packages"
   WRouter():New( Application ):Start( "WDocMain" )
   Application:Run()

RETURN

