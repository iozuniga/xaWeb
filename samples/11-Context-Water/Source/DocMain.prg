
#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs

   Engine:lDebug := .t.

   WITH OBJECT WWaterContext():New( Self )
      :cTheme := "dark"
   END WITH

   ECHO "<h1>XailerWeb - Water.css</h1>" INTO Self

   ECHO "<p>Water.css is a drop-in collection of CSS styles to make simple "+;
        "websites like this just a little bit nicer.</p>"  INTO Self

   ECHO "<p>Now you can write your simple static site with nice semantic html,"+;
        " and Water.css will manage the styling for you.<p>" INTO Self

   WITH OBJECT WDiv():New( Self )
      :AddClass( "row" )
      WITH OBJECT WDiv():New( SO )
         WITH OBJECT WLink():New( SO )
            :cHref := "#installation"
            :cText := "<b>Get it already!</b>"
         END WITH
         WITH OBJECT WLink():New( SO )
            :cHref := "https://github.com/kognise/water.css"
            :cText := "<b>GitHub</b>"
         END WITH
      END WITH
      WITH OBJECT WLink():New( SO )
         :cHref := "https://www.producthunt.com/posts/water-css?utm_source=badge-featured&utm_medium=badge&utm_souce=badge-water-css"
         :cTarget := "_blank"
         WITH OBJECT WImage():New( SO )
            :cId  := "product-hunt"
            :cSrc := "https://api.producthunt.com/widgets/embed-image/v1/top-post-badge.svg?post_id=150490&theme=dark&period=daily"
            :cAlt :="Water.css - Make your tiny website just a little nicer | Product Hunt Embed"
            :AddStyle( "width:250px;height:54px;" )
         END WITH
      END WITH
   END WITH

   ECHO "<h2>Table view with Water.css</h2>" INTO Self

   WITH OBJECT WTable():New( Self )
      :cId := "table"
      :oStyle:cWidth := "600px"
      :LoadData( { { "Code", "Name" } } )
      :LoadData( Provincias() )
      :LoadData( { { "foot", "foot" } } )
      :nHeader := 1
      :nFooter := 1
   END WITH

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

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

