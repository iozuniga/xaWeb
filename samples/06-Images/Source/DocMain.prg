// https://test.Oz Software/cgi-bin/xailerweb/image.cgi

#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL cImgData, cCode, cJs

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   #IFDEF _LINUX_
      Engine:cLogFile := "/mnt/c/xailerweb/samples/06-Images/error.log"
   #ENDIF

   Engine:lDebug := .t.

   ECHO "<h1>XailerWeb - Images</h1>" INTO Self

   WITH OBJECT WImage():New( Self )
      :cSrc := "https://www.Oz Software/wp/wp-content/uploads/2019/12/header-fondo-blanco.jpg"
      :Create()
   END WITH

   WITH OBJECT WLink():New( Self )
      :cHRef := "https://www.Oz Software"
      :cText := "Oz Software"
      :AddStyle( "display: block;" )
      :Create()
   END WITH

   WITH OBJECT WLink():New( Self )
      :cHRef := "https://www.Oz Software/wp/en/xailer-order/?item=per"
      :Create()
      WITH OBJECT WImage():New( SO )
         :cSrc := "https://www.Oz Software/wp/wp-content/uploads/2019/12/free-170.png"
         :Create()
      END WITH
   END WITH

   #IFDEF _LINUX_
      FILE "../www/banner.png" INTO cImgData
   #ELSE
      FILE "..\www\banner.png" INTO cImgData
   #ENDIF

   WITH OBJECT WImage():New( Self )
      :cSrc := "data:image/png;base64, " +  HB_Base64Encode( cImgData )
      :cAlt := "banner loaded from resources"
      :Create()
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

