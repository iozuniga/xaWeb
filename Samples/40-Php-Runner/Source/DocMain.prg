
#include "xaWeb.ch"
#include "error.ch"

#define  PHP_EXE_PATH "z:\xampp\php\php.exe"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

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

   ECHO "<h1>xaWeb: PHP Version</h1>" INTO Self

   WITH OBJECT WPhpRunner():New()
#ifndef _LINUX_
      :cPhpRunner := PHP_EXE_PATH
      IF IsDebug() .AND. IsDebuggerPresent()
         :cPhpPath   := HB_DirBase()
         :cPhpModule := "www\phpinfo.php"
      ELSE
         //:cPhpRunner := "/opt/plesk/php/7.4/bin/php"
         :cPhpModule := "php/phpinfo.php" // remember to copy this file to c:\xampp\htdocs\php
      ENDIF
#else
      :cPhpModule := "php/phpinfo.php"
#endif
      IF :Run()
         ECHO "PHP version: " + :cResult INTO Self
      ELSE
         ECHO "ERROR" INTO Self
      ENDIF
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

