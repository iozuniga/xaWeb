#include "xa-materialize.ch"

REQUEST DBFCDX

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cCode, cJs, cCss, cPath

   WMaterializeContext():New( Self )

   TEXT INTO cJs
      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.dataset.text);
            ele.hidden = false;
            document.getElementById("btnsource").style.display = "none";
            M.Forms.textareaAutoResize(ele);
         }
      }
   ENDTEXT

#ifdef _LINUX_
   cPath := HB_DirBase() + "data" + hb_OsPathSeparator()
#else
   cPath := HB_DirBase() + "..\..\resource\"
#endif

   WITH OBJECT ::oSession
      IF !:IsFile( "customer.dbf" )
         :SaveFile( cPath + "customer.dbf", "customer.dbf" )
      ENDIF
      IF !:IsFile( "customer.cdx" )
         :SaveFile( cPath + "customer.cdx", "customer.cdx" )
      ENDIF
      USE ( cPath + "customer.dbf" ) ALIAS Customer READONLY VIA "DBFCDX"
      SET ORDER TO TAG "ID"
   END WITH

   ::AddScript( cJs )

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      :Create()
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
   END WITH

   WITH OBJECT WDiv():New( Self )
      :AddClass( "container" )
      ECHO "<h4>xaWeb - Materialize - DBF Table</h4>"

      WITH OBJECT WTable():New( SO )
         :cId := "table1"
         :oStyle:Width := "100%"
         :cHeaderBkColor := Document:oContext:PrimaryColor()
         :cHeaderColor := Document:oContext:PrimaryTextColor()
         :LoadDbf( "customer",  {"Id","First","Last","Street", "City"} )
         :aHeaders[ 1 ]:cHeader := "ID"
         :nHeader := 1
         :cMaxHeight := "600px"
         :lResponsive := .T.
         :lShowID := .t.
         :lShowSelected := .t.
         :lCanSort := .t.
         :lCanFilter := .t.
         :Create()
      END WITH
   END WITH

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
         :AddDataset( "text", HB_Base64Encode( cCode ) )
         :lReadOnly := .t.
         :lVisible := .f.
         :Create()
      END WITH

RETURN nil

//------------------------------------------------------------------------------
