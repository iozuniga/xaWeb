// https://test.ozs.es/cgi-bin/xaWeb/select.cgi

/*
Nota importante: La operación fetch de un servicio del propio CGI no funciona
                 si se ejecuta directamente desde el IDE de Xailer. Es necesario
                 que el CGI este alojado en un servidor Web.
                 Con WSL+Apache2 funciona correctamente.
*/

#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oBtn

   METHOD CreateDoc()
   METHOD Srv_Provincias( hParams )
   METHOD Srv_Magic( hParams )

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oFetch
   LOCAL cJS, cCode

   Engine:lDebug := .t.

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
      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.value);
            ele.hidden = false;
            document.getElementById("btnsource").style.display = "none";
         }
      }
   ENDTEXT

   ::AddScript( cJS )

   ECHO "<h1>xaWeb - Select</h1>" INTO Self

   WITH OBJECT WLabel():New( Self )
      :cText := "Select sample:"
      :cFor := "select1"
      :Create()
   END WITH

   WITH OBJECT WSelect():New( Self )
      :oStyle:Vertical_align:= "top"
      :cId    := "select"
      :cCache := "global" // "session"
      IF !:IsCached()
         :aItems := Provincias()
      ELSEIF !Engine:IsService()
         LogDebug( "Select items Cached!", :cCache )
      ENDIF
      :nSize  := 10
      :Create()
   END WITH

   WITH OBJECT WLabel():New( Self )
      :cText := "ComboBox filled by fetch operation:"
      :cFor  := "select2"
      :Create()
   END WITH

   WITH OBJECT WSelect():New( Self )
      :cId := "select2"
      :Create()
   END WITH

   WITH OBJECT oFetch := WFetch():New()
      :cUrl      := ::Service( "Srv_Provincias" )
      :cTargetId := "select2"
      :cSourceId := "button1"
      :cCallBack := "myFill"
      :cContentType := "application/json"
   END WITH

   WITH OBJECT WButton():New( Self )
      :cId := "button1"
      :cText := "Fill"
      :Onclick := oFetch
      :Create()
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
      :Create()
   END WITH

   WITH OBJECT WDiv():New( Self )
      WITH OBJECT WSelect():New( SO )
         :AddItem( "01", "One" )
         :AddItem( "02", "Two" )
         :AddItem( "03", "Three" )
         :cId    := "select3"
         :nSize  := 3
         :Create()
      END WITH
   END WITH

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WButton():New( Self )
      :cText := "This button shows xaWeb source code"
      :Onclick := "showCode"
      :cId := "btnsource"
      :Create()
   END WITH

   WITH OBJECT WTextArea():New( Self )
      :oStyle:Margin_top := "20px"
      :oStyle:Font_Family := "monospace"
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

//   Engine:LogEnv()

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
