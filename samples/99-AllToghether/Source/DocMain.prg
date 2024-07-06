// https://test.Oz Software/cgi-bin/xailerweb/alltoghether.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cJs

   Engine:lDebug := .t.

   TEXT INTO cJs
      function myfunction(e) {
         const val = "https://test.Oz Software/cgi-bin/xailerweb/" + e.currentTarget.value;
         window.location = val;
      }
   ENDTEXT

   ::AddScript( cJs )

   wBasicContext():New( Self )

   WITH OBJECT WText():New( Self )
      :cText := "<h2>Xailerweb: All toghether!</h2>"
      :Create()
   END WITH

   WITH OBJECT WListbox():New( Self )
      :aItems := Modulos()
      :cId    := "listbox1"
      :nSize  := 22
      :oStyle:cWidth := "400px"
      :Onchange := "myfunction"
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

STATIC FUNCTION Modulos()

   LOCAL cModulos := { { "hello.cgi", "01-Hello world!"},;
                       { "buttons.cgi", "02-Buttons"},;
                       { "containers.cgi", "03-Containers"},;
                       { "simpleform.cgi", "04-Simple form"},;
                       { "listboxes.cgi", "05-Listboxes"},;
                       { "image.cgi", "06-Images"},;
                       { "tables.cgi", "07-Tables"},;
                       { "nav.cgi", "08-Nav"},;
                       { "login.cgi", "09-Login"},;
                       { "modal.cgi", "10-Package Modal"},;
                       { "watercss.cgi", "11-Context Water.css"},;
                       { "simplecss.cgi", "12-Context Simple.css"},;
                       { "modalform.cgi", "13-Modal form" },;
                       { "modalformfetch.cgi", "13-Modal form via Fetch" },;
                       { "tablesresponsive.cgi","20-Tables Responsive"},;
                       { "tablesautosort.cgi", "21-Tables Autosort"},;
                       { "tablesrowclick.cgi", "22-Tables Row click"},;
                       { "tablesrowfilter.cgi", "23-Tables Auto filter"},;
                       { "tablesloaddata.cgi", "24-Tables Load data"},;
                       { "tablescrud.cgi", "25-Tables CRUD"},;
                       { "custombutton.cgi", "30-Icon-button / Spinner-button"};
                       }

RETURN cModulos

//------------------------------------------------------------------------------







