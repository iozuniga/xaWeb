

#include "XailerWeb.ch"

ANNOUNCE HB_GtSys

PROCEDURE Main()
   Application:cTitle := "SimpleFormWin"
   WRouter():New( Application ):Start( "WDocMain" )
   Application:Run()
RETURN

//------------------------------------------------------------------------------


