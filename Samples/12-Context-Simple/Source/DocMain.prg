
#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs

   //Engine:lDebug := .t.

   WSimpleContext():New( Self )

   WITH OBJECT WHeader():New( Self )
      ECHO "<h1>xaWeb - Simple.css Demo</h1>"
      ECHO "<p>A showcase of Simple.css formatting in action and how to use it.</p>"
   END WITH

   WITH OBJECT WMain():New( Self )
      WITH OBJECT WParagraph():New( SO )
         :AddClass( "notice" )
         ECHO "This demo page details a select set of elements that are meant to show off Simple.css’s styling, and provide HTML to help you get started easily. If you want a comprehensive demonstration of elements that may or may not be styled by Simple.css, please see "
         WITH OBJECT WLink():New( SO )
            :cHref := "https://test.simplecss.org"
            :cText := "our test page"
         END WITH
         ECHO "."
      END WITH
      WITH OBJECT WParagraph():New( SO )
         ECHO "This page is a demonstration of the elements that can be formatted using Simple.css. Each section includes a code block on how to include it in your site’s design."
      END WITH
      WITH OBJECT WParagraph():New( SO )
         ECHO "This may be a little basic for some people, but I wanted the barrier for entry to be as low as possible for this project."
      END WITH
      WITH OBJECT WParagraph():New( SO )
         ECHO "This may be a little basic for some people, but I wanted the barrier for entry to be as low as possible for this project."
      END WITH
      WITH OBJECT WText():New( SO )
         :cTag := "h2"
         :cId := "basic-typography"
         :cText := "Basic Typography"
         :Create()
      END WITH
      WITH OBJECT WParagraph():New( SO )
         ECHO "All the typography of Simple.css uses "
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "rem"
            :Create()
         END WITH
         ECHO " for sizing. This means that accessibility is maintained for those who change their browser font size. The "
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "body"
            :Create()
         END WITH
         ECHO " element has a size of "
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "1.15rem"
            :Create()
         END WITH
         ECHO "  which makes all the standard font sizes slightly larger. This equates to "
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "18.4px"
            :Create()
         END WITH
         ECHO " for paragraph text, instead of the standard "
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "16px"
            :Create()
         END WITH
      END WITH
      ECHO "The heading elements also have an increased top margin in order to break blocks of text up better."
      WITH OBJECT WText():New( SO )
         :cTag := "h1"
         :cId := "heading-1-3rem"
         :cText := "Heading 1 "
         :Create()
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "3rem"
            :Create()
         END WITH
      END WITH
      WITH OBJECT WText():New( SO )
         :cTag := "h2"
         :cId := "heading-2-26rem"
         :cText := "Heading 2 "
         :Create()
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "2.6rem"
         END WITH
      END WITH
      WITH OBJECT WText():New( SO )
         :cTag := "h3"
         :cId := "heading-3-2rem"
         :cText := "Heading 3 "
         :Create()
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "2rem"
            :Create()
         END WITH
      END WITH
      WITH OBJECT WText():New( SO )
         :cTag := "h4"
         :cId := "heading-4-144rem"
         :cText := "Heading 4 "
         :Create()
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "1.44rem"
            :Create()
         END WITH
      END WITH
      WITH OBJECT WText():New( SO )
         :cTag := "h5"
         :cId := "heading-5-115rem"
         :cText := "Heading 5 "
         :Create()
         WITH OBJECT WText():New( SO )
            :cTag := "code"
            :AddClass( "language-plaintext highlighter-rouge" )
            :cText := "1.15rem"
            :Create()
         END WITH
      END WITH
      WITH OBJECT WDiv():New( SO )
         :AddClass( "highlight" )
         WITH OBJECT WDiv():New( SO )
            :cTag := "pre"
            :AddClass( "highlight" )
            WITH OBJECT WText():New( SO )
               :cTag := "code"
               :cText := "Sample code"
               :Create()
            END TEXT
         END WITH
      END WITH
      WITH OBJECT WText():New( SO )
         :cTag := "h3"
         :cId := "links--buttons"
         :cText := "Links & Buttons"
         :Create()
      END WITH
      WITH OBJECT WText():New( SO )
         :cText := "Links are formatted very simply on Simple.css (shock horror). They use the accent  CSS variable and are underlined. There is a  effect that removes the underline."
         :Create()
      END WITH
      WITH OBJECT WParagraph():New( SO )
         WITH OBJECT WLink():New( SO )
            :cHref := "https://example.com"
            :cText := "I'm a hyperlink"
            :Create()
         END WITH
      END WITH
      WITH OBJECT WParagraph():New( SO )
         WITH OBJECT WButton():New( SO )
            :cText := "I'm a button"
            :Create()
         END WITH
      END WITH
      ECHO "<h2>Table view with Simple.css</h2>"

      WITH OBJECT WTable():New( SO )
         :cId := "table"
         :cMaxHeight := "600px"
         :LoadData( { { "Code", "Name" } } )
         :LoadData( Provincias() )
         :nHeader := 1
         :Create()
      END WITH
   END WITH

   WITH OBJECT WFooter():New( Self )
      ECHO "<h1>Simple.css Demo</h1>"
      ECHO "<p>A showcase of Simple.css formatting in action and how to use it.</p>"
   END WITH

   TEXT INTO cJs
      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.value);
            ele.hidden = false;
            ele.style.display = "block";
            document.getElementById("btnsource").style.display = "none";
         }
      }
   ENDTEXT

   ::AddScript( cJs )

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
         :oStyle:Display := "none"
         :nCols := 80
         :cId   := "source"
         :nRows := 40
         :cText := HB_Base64Encode( cCode )
         :lReadOnly := .t.
         :lVisible := .f.
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

