

#include "XailerWeb.ch"

ANNOUNCE HB_GtSys

PROCEDURE Main()
   Application:cTitle := "ModalForm"
   WRouter():New( Application ):Start( "WDocMain" )
   Application:Run()
RETURN

//------------------------------------------------------------------------------


