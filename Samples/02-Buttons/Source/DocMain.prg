#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

   METHOD Act_Click( hParams, hEvent )

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oFetch AS CLASS WFetch
   LOCAL cJs, cUrl, cCode

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "dark"
   END WITH

   TEXT INTO cJs
      function myfunction(e) {
         alert( 'Click triggered from a user function' );
      }

      function citySolver(element, data) {
         if (data.city) {
             element.innerHTML = 'OZ Software server is at ' + data.city;
             xa_setHbData(element, 'cText', element.innerText, true);
         }
         else
            element.innerHTML = 'City could not be found';
      }

      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.value);
            ele.hidden = false;
            document.getElementById("btnsource").disabled = true;
         }
      }
   ENDTEXT

   ::AddScript( cJs )

//   Engine:lDebug := .t.

   WITH OBJECT WParagraph():New( Self )
      :cText := "<h1>xaWeb - Buttons</h1>"
      :Create()
   END WITH

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a inline Javascript code"
      :OnClick := "<script>alert( 'Click!' )</script>"
      :cId := "button1"
      :oStyle:Display := "block"
      :Create()
   END WITH

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a Javascript user function"
      :OnClick := "myfunction"
      :cId := "button2"
      :oStyle:Display := "block"
      :Create()
   END WITH

   IF Engine:IsSecure()
      cUrl := "https://ipinfo.io/38.242.203.180/json"
   ELSE
      cUrl := "http://ip-api.com/json/ozs.es"
   ENDIF

   oFetch := WFetch():New( cUrl )

   WITH OBJECT oFetch
      :cTargetId := "mycity"
      :cSourceId := "button3"
      :cCallBack := "citySolver"
   END WITH

#ifdef __LINUX__
   WITH OBJECT ZcmpButtonSpinner():New( Self )
      :cText := "This button calls an asyncronous API"
      :OnClick := oFetch
      :cId := "button3"
      :Create()
   END WITH
#else
   WITH OBJECT ZButton():New( Self )
      :cText := "This button calls an asyncronous API"
      :OnClick := oFetch
      :cId := "button3"
      :Create()
   END WITH
#endif

   WITH OBJECT WSpan():New( Self )
      :cText := "Where is Oz Software web site server?"
      :cId := "mycity"
      :Create()
   END WITH


   WITH OBJECT WParagraph():New( Self )
      :cText := "Please push this button after pushing the previous one"
   END WITH

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a CGI xaWeb event"
      :OnClick := "Act_Click"
      :cId := "button4"
      :Create()
   END WITH

   WITH OBJECT WParagraph():New( Self )
      :cText := "Insert some text on this field and confirm it does NOT disappear when pushing the previous button."
   END WITH

   WITH OBJECT WEdit():New( Self )
      :cLabel := "Label:"
      :cId := "myEdit"
      :nSize := 50
      :Create()
   END WITH

   WITH OBJECT WButton():New( Self )
      :cText := "This button shows xaWeb source code"
      :Onclick := "showCode"
      :cId := "btnsource"
      :Create()
   END WITH

   WITH OBJECT WParagraph():New( Self )
      :cText := "<hr>"
   END WITH

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WTextArea():New( Self )
      :nCols := 80
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lVisible := .f.
      :lReadOnly := .t.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Click( hParams, hEvent ) CLASS WDocMain

   LOCAL cHtml

   TEXT INTO cHtml
      This new paragraph has been created through a CGI fired event.</p>
      <p>If you had pushed the third button before calling this fourth button,
      take a look at text of the city where OZ Software server is. As you can
      see, the city name continues there. xaWeb magic!"
   ENDTEXT

   WITH OBJECT WParagraph():New( Self )
      :cText := cHtml
   END WITH

RETURN NIL