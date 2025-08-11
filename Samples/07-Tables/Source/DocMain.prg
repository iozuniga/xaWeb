//https://test.ozs.es/cgi-bin/xaWeb/tables.cgi


#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA aData1, aData2

   METHOD CreateDoc()
   METHOD Table1()
   METHOD Table2()
   METHOD Table3()
   METHOD Table4()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::aData1 := { { "First Name", "Last Name", "Credit" },;
               { "Ignacio", "Ortiz", "5000" },;
               { "Ernesto", "Mate", "2000" },;
               { "Perico", "De los Palotes", "100" } }

   ::aData2 := { { "Name", "Ignacio" },;
                 { "Phone", "456 789 215" },;
                 { NIL,  "456 781 237 " }  }

   ECHO "<h1>xaWeb - Simple tables demo</h1>" INTO Self

   ::Table1()
   WParagraph():New( Self )
   ::Table2()
   WParagraph():New( Self )
   ::Table3()
   WParagraph():New( Self )
   ::Table4()

   TEXT INTO cJs
      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.value);
            ele.hidden = false;
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

METHOD Table1() CLASS WDocMain

   WITH OBJECT WTable():New( Self )
      :cId := "table1"
      :cCaption := "Simple table"
      :LoadData( ::aData1 )
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Table2() CLASS WDocMain

   ::AddStyle( "#table2 tr:nth-child(even)", "background-color: #D6EEEE;" )

   WITH OBJECT WTable():New( Self )
      :cId := "table2"
      :cCaption := "Col groups"
      :LoadData( ::aData1 )
      :AddColGroup(1):AddStyle( "width: 200px" )
      :AddColGroup(1):AddStyle( "width: 300px" )
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Table3() CLASS WDocMain

   WITH OBJECT WTable():New( Self )
      :cId := "table3"
      :cCaption := "Header left with rowspan"
      :LoadData( ::aData2,, .T. )
      :AddColGroup(1):AddStyle( "width: 200px" )
      :AddColGroup(1):AddStyle( "width: 300px" )
      :aRows[ 2 ]:aCells[ 1 ]:nRowSpan := 2
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Table4() CLASS WDocMain

   WITH OBJECT WTable():New( Self )
      :cId := "table5"
      :oStyle:Width := "400px"
      :cHeaderBkColor := "green"
      :cFooterBkColor := "green"
      :cMaxHeight := "600px"
      :LoadData( { { "Code", "Name" } } )
      :LoadData( Provincias() )
      :LoadData( { { "foot", "foot" } } )
      :nHeader := 1
      :nFooter := 1
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------
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

