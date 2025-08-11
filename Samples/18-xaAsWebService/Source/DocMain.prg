#include "xaWeb.ch"

/*
 This CGI just exposes a dummy Web server. To test the result call this URL:

 http://localhost/cgi-bin/webservice.cgi?service=wdocmain-Srv_Test

 Notes on the URL:
 1) The first part of the URL (Before '?') points to the CGI itself
 2) The '?' starts the CGI GET parameters
 3) The first parameter 'service' indicates that is a SERVICE operation
 4) The value of first parameter is the classname of the WDoc plus "-" plus the
    name of the method to Execute
 5) The Web service returns a simple JSON object

 Note on Windows Proyecto:
 You will need Apache for Windows instaled on your system
*/

CLASS WDocMain FROM WDoc

   METHOD Srv_Test( hParams, hEvent )

END CLASS

//------------------------------------------------------------------------------

METHOD Srv_Test( hParams, hEvent ) CLASS WDocMain

   LOCAL hHash := {=>}

   HB_HSet( hHash, "name", "ignacio" )
   HB_HSet( hHash, "alias", "iozuniga" )


RETURN HB_JsonEncode( hHash )

//------------------------------------------------------------------------------

