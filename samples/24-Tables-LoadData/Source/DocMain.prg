/*
This sample shows how to easily update any Table data.

IMPORTANT NOTE:
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

#include "XailerWeb.ch"

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

   ta = document.getElementById("source");
   if (ta) {
      ta.value = xw_b64toUnicode(ta.value);
   }
   delete ta;
   ENDTEXT

   ::AddScript( cJs )

   aHeader := { {"Id","First","Last","Street","City"} }

   ECHO "<h2>XailerWeb: Table Load data</h2>" INTO Self

   WITH OBJECT WText():New (Self )
      :cId := "rowInfo"
      :cText := "<p>Click on any row</p>"
      :Create()
   END WITH

   WITH OBJECT oBtn1 := WCmpButtonSpinner():New( Self )
      :cText := "Load 477 records from a Json file on server via Fetch API with XailerWeb built-in system"
      :cId := "button1"
   END WITH

   WITH OBJECT WDiv():New( Self )
      WITH OBJECT oBtn2 := WButton():New( SO )
         :cText := "Change row to lower case via FETCH (Harbour)"
         :cId := "btnEdit"
      END WITH
   END WITH

   WITH OBJECT ::oTable := WTable():New( Self )
      :cId := "table1"
      :oStyle:cWidth := "100%"
      :LoadData( aHeader, .t. ) // Second parameter .T., indicates that first column field is an ID (Pk)
      :nHeader := 1
      :lResponsive := .T.
      :lCanSort := .t.
      :lShowId := .t.
      :lCanFilter := .t.
      :lShowSelected := .t.
      :OnRowClick := "jsrowClick"
      :cBtnEditID := oBtn2:cId
      :Create()
   END WITH

   oBtn1:OnClick := ::oTable:LoadFromService( "Srv_TableData", oBtn1:cId )
   oBtn2:OnClick := ::oTable:EditFromService( "Srv_TableEdit", oBtn2:cId )

   FILE "DocMain.prg" INTO cCode

   ECHO "<hr>" INTO Self

   WITH OBJECT WTextArea():New( Self )
      :nCols := 100
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
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

