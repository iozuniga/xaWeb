
#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL oDs AS CLASS WXailerWebDataSource
   LOCAL aData, aHeader
   LOCAL cJs, cCode

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

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

   WITH OBJECT oDs := WXailerWebDataSource():New( Self )
#ifndef _LINUX_
      :cPhpRunner := "c:\xampp\php\php.exe"
      IF IsDebug() .AND. IsDebuggerPresent()
         :cPhpPath   := HB_DirBase()
         :cPhpModule := "www\Neptuno.php"
      ELSE
         :cPhpModule := "php/Neptuno.php" // remember to copy this file to c:\xampp\htdocs
      ENDIF
#else
      // This is necessary with multiple php installed. With WSL is not necessary
      //:cPhpRunner := "/opt/plesk/php/7.4/bin/php"
      :cPhpModule := "php/Neptuno.php"
#endif
      :cDatabase := "neptuno"
      aData := :QueryArray( "select * from clientes", @aHeader )
   END WITH

   ECHO "<h1>xaWeb: Xailer WebDatasource MariaDB/MySql sample</h1>" INTO Self

   WITH OBJECT WTable():New( Self )
      :cId := "table1"
      :cHeaderBkColor := "green"
      :cFooterBkColor := "green"
      :cMaxHeight := "600px"
      :LoadData( aHeader )
      :LoadData( aData )
      :nHeader := 1
      :Create()
   END WITH

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
      :nCols := 100
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

