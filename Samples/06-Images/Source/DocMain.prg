// https://test.ozs.es/cgi-bin/xaWeb/image.cgi

#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cImgData, cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

//   Engine:lDebug := .t.

   ECHO "<h1>xaWeb - Images</h1>" INTO Self

   WITH OBJECT WImage():New( Self )
      :cSrc := "/assets/banner2.png"
      :Create()
   END WITH

   WITH OBJECT WLink():New( Self )
      :cHRef := "https://www.ozs.es/wp/xaconta-local/"
      :Create()
      WITH OBJECT WImage():New( SO )
         :cSrc := "/assets/xaconta.png"
         :nHeight := 377
         :nWidth := 512
         :Create()
      END WITH
   END WITH

   #IFDEF _LINUX_
      FILE "../www/assets/banner.png" INTO cImgData
   #ELSE
      FILE "..\www\assets\banner.png" INTO cImgData
   #ENDIF

   WITH OBJECT WImage():New( Self )
      :cSrc := "data:image/png;base64, " +  HB_Base64Encode( cImgData )
      :cAlt := "banner loaded from resources"
      :oStyle:Display := "block"
      :Create()
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

