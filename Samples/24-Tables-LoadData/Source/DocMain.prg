/*
This sample shows how to easily update any Table data.

IMPORTANT NOTE:

If you are trying to see this sample through a web server, the file "data.json"
SHOULD BE present on your /cgi-bin/data directory.

It does not work correctly on Xailer IDE CGI runner, since it does not
support Fetch operations a this moment.

To retrieve the data we use the method WTable:LoadFromService()

oBtn:OnClick := oTable:LoadFromService( cService )

Where cService is a method on current WDocMain objet.

This method should return a multi-dimension array in Json Format:

   a := { ;
         {"row1.data1", "row1.data2", ..., "row1.dataN" },;
         ... ,;
         {"rowN.data1", "rowN.data2", ..., "rowN.dataN" },;
        }

   RETURN hb_JsonEncode( a )
*/

#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA oTable

   METHOD CreateDoc()

   METHOD Srv_TableData( hParam )
   METHOD Srv_TableEdit( hParam )

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oBtn1, oBtn2
   LOCAL aHeader
   LOCAL cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   TEXT INTO cJs
   function jsrowClick(event) {
      const ele = document.getElementById( "rowInfo" );
      if (ele) {
         ele.innerHTML = "<p>Recno: " + event.detail.recno + " (resolved in Javascript)</p>"
      }
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

   ::AddScript( cJs )

   aHeader := { {"Id","First","Last","Street","City"} }

   ECHO "<h2>xaWeb: Table Load data</h2>" INTO Self

   WITH OBJECT WParagraph():New (Self )
      :cId := "rowInfo"
      :cText := "Click on any row"
      :Create()
   END WITH

#ifdef __LINUX__
   WITH OBJECT oBtn1 := WCmpButtonSpinner():New( Self )
      :cText := "Load 477 records from a Json file on server via Fetch API with xaWeb built-in system"
      :cId := "button1"
      :Create()
   END WITH
#else
   WITH OBJECT oBtn1 := WButton():New( Self )
      :cText := "Load 477 records from a Json file on server via Fetch API with xaWeb built-in system"
      :cId := "button1"
      :Create()
   END WITH
#endif

   WITH OBJECT WDiv():New( Self )
      WITH OBJECT oBtn2 := WButton():New( SO )
         :cText := "Change row to lower case via FETCH (Harbour)"
         :cId := "btnEdit"
         :Create()
      END WITH
   END WITH

   WITH OBJECT ::oTable := WTable():New( Self )
      :cId := "table1"
      :oStyle:Width := "100%"
      :LoadData( aHeader, .t. ) // Second parameter .T., indicates that first column field is an ID (Pk)
      :nHeader := 1
      :lResponsive := .T.
      :lCanSort := .t.
      :lShowId := .t.
      :lCanFilter := .t.
      :lShowSelected := .t.
      :OnRowClick := "jsrowClick"
      :oEditControl := oBtn2
      :Create()
   END WITH

   oBtn1:OnClick := ::oTable:LoadFromService( "Srv_TableData", oBtn1 )
   oBtn2:OnClick := ::oTable:EditFromService( "Srv_TableEdit", oBtn2 )

   FILE "DocMain.prg" INTO cCode

   ECHO "<hr>" INTO Self

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
/*
Note: Table data is on CGI PATH + "/data"
*/

METHOD Srv_TableData( hParam ) CLASS WDocMain

   LOCAL cFile, cJson

   cFile := HB_DirBase() + "data/data.json"

   IF File( cFile )
      cJson := HB_MemoRead( cFile )
   ELSE
      cJson := '{ "Error": "File not found: ' + cFile + '"}'
   ENDIF

RETURN cJson

//------------------------------------------------------------------------------

METHOD Srv_TableEdit( hParam ) CLASS WDocMain

   LOCAL hCargo := Engine:hCargo
   LOCAL cKey, cValue

   FOR EACH cKey, cValue IN HB_HKeys( hCargo ), HB_HValues( hCargo )
      HB_HSet( hCargo, cKey, Lower( cValue ) )
   NEXT

RETURN HB_JsonEncode( hCargo )

//------------------------------------------------------------------------------

