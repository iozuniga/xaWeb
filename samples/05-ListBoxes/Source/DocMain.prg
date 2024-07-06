// https://test.Oz Software/cgi-bin/xailerweb/listboxes.cgi

/*
Nota importante: La operación fetch de un servicio del propio CGI no funciona
                 si se ejecuta directamente desde el IDE de Xailer. Es necesario
                 que el CGI este alojado en un servidor Web.
                 Con WSL+Apache2 funciona correctamente.
*/

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oBtn

   METHOD CreateDoc()
   METHOD Srv_Provincias( hParams )
   METHOD Srv_Magic( hParams )

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oFetch
   LOCAL cJS, cCode

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   TEXT INTO cJS
      function myFill(element, data) {
         element.options.length = 0;
         data.forEach((row) => {
            element.options[element.options.length] = new Option(row[1], row[0]);
         });
      }
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJS )

   #IFDEF _LINUX_
      Engine:cLogFile := "/mnt/c/xailerweb/samples/05-ListBoxes/error.log"
   #ENDIF

   Engine:lDebug := .t.

   ECHO "<h1>XailerWeb - Listboxes</h1>" INTO Self

   ::AddStyle( "#listbox1", "vertical-align: top;" )

   WITH OBJECT WLabel():New( Self )
      :cText := "Listbox sample:"
      :cFor := "listbox1"
      :Create()
   END WITH

   WITH OBJECT WListbox():New( Self )
      :aItems := Provincias()
      :cId    := "listbox1"
      :nSize  := 10
      :Create()
   END WITH

   WITH OBJECT WLabel():New( Self )
      :cText := "ComboBox filled by fetch operation:"
      :cFor  := "listbox2"
      :Create()
   END WITH

   WITH OBJECT WListbox():New( Self )
      :cId := "listbox2"
      :Create()
   END WITH

   WITH OBJECT oFetch := WFetch():New()
      :cUrl      := ::Service( "Srv_Provincias" )
      :cTargetId := "listbox2"
      :cSourceId := "button1"
      :cCallBack := "myFill"
      :cContentType := "application/json"
   END WITH

   WITH OBJECT WButton():New( Self )
      :cId := "button1"
      :cText := "Fill"
      :Onclick := oFetch
   END WITH

   WITH OBJECT oFetch := WFetch():New()
      :cUrl      := ::Service( "Srv_Magic" )
      :cSourceId := "button2"
      :cContentType := "application/javascript"
   END WITH

   WITH OBJECT ::oBtn := WButton():New( Self )
      :cId := "button2"
      :cText := "Some magic! (does not work on built-in Xailer navigator)"
      :Onclick := oFetch
   END WITH

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   HB_MemoWrit( "c:\temp\test.txt", cCode )

   WITH OBJECT WTextArea():New( Self )
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Srv_Provincias( hParams ) CLASS WDocMain

RETURN HB_JsonEncode( Provincias() )

//------------------------------------------------------------------------------

METHOD Srv_Magic( hParams ) CLASS WDocMain

   ::oBtn:cText := "Button changed within Harbour code!"

RETURN NIL

//------------------------------------------------------------------------------

STATIC FUNCTION Provincias()

RETURN { {"02","Albacete"},;
         {"03","Alicante/Alacant"},;
         {"04","Almería"},;
         {"01","Araba/Álava"},;
         {"33","Asturias"},;
         {"05","Ávila"},;
         {"06","Badajoz"},;
         {"07","Balears, Illes"},;
         {"08","Barcelona"},;
         {"48","Bizkaia"},;
         {"09","Burgos"},;
         {"10","Cáceres"},;
         {"11","Cádiz"},;
         {"39","Cantabria"},;
         {"12","Castellón/Castelló"},;
         {"13","Ciudad Real"},;
         {"14","Córdoba"},;
         {"15","Coruña, A"},;
         {"16","Cuenca"},;
         {"20","Gipuzkoa"},;
         {"17","Girona"},;
         {"18","Granada"},;
         {"19","Guadalajara"},;
         {"21","Huelva"},;
         {"22","Huesca"},;
         {"23","Jaén"},;
         {"24","León"},;
         {"25","Lleida"},;
         {"27","Lugo"},;
         {"28","Madrid"},;
         {"29","Málaga"},;
         {"30","Murcia"},;
         {"31","Navarra"},;
         {"32","Ourense"},;
         {"34","Palencia"},;
         {"35","Palmas, Las"},;
         {"36","Pontevedra"},;
         {"26","Rioja, La"},;
         {"37","Salamanca"},;
         {"38","Santa Cruz de Tenerife"},;
         {"40","Segovia"},;
         {"41","Sevilla"},;
         {"42","Soria"},;
         {"43","Tarragona"},;
         {"44","Teruel"},;
         {"45","Toledo"},;
         {"46","Valencia/València"},;
         {"47","Valladolid"},;
         {"49","Zamora"},;
         {"50","Zaragoza"},;
         {"51","Ceuta"},;
         {"52","Melilla" } }
