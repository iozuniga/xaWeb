#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

   METHOD Act_Click( hEvent )

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
             element.innerHTML = 'Xailer server is at ' + data.city;
             xw_setHbData(element, 'cText', element.innerText, true);
         }
         else
            element.innerHTML = 'City could not be found';
      }

      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xw_b64toUnicode(ele.value);
            ele.hidden = false;
            document.getElementById("btnsource").disabled = true;
         }
      }
   ENDTEXT

   #ifdef _LINUX_
      Engine:cLogFile := "/mnt/c/xailerweb/samples/02-buttons/error.log"
   #endif

   Engine:lDebug := .t.

   WITH OBJECT WText():New( Self )
      :cText := "<h1>XailerWeb - Buttons</h1>"
      :Create()
   END WITH

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a inline Javascript code"
      :OnClick := "<script>alert( 'Click!' )</script>"
      :cId := "button1"
      :Create()
   END WITH

   WText():New( Self ):cText := "<p></p>"

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a Javascript user function"
      :OnClick := "myfunction"
      :cId := "button2"
      :Create()
   END WITH

   WText():New( Self ):cText := "<p></p>"

   IF Engine:IsSecure()
      cUrl := "https://ipinfo.io/38.242.203.180/json"
   ELSE
      cUrl := "http://ip-api.com/json/Oz Software"
   ENDIF

   oFetch := WFetch():New( cUrl )

   WITH OBJECT oFetch
      :cTargetId := "mycity"
      :cSourceId := "button3"
      :cCallBack := "citySolver"
   END WITH

   WITH OBJECT ZcmpButtonSpinner():New( Self )
      :cText := "This button calls an asyncronous API"
      :OnClick := oFetch
      :cId := "button3"
      :Create()
   END WITH

   WITH OBJECT WText():New( Self )
      :cText := "Where is Xailer web site server?"
      :cId := "mycity"
      :Create()
   END WITH

   WText():New( Self ):cText := "<p>Please push this button after pushing the previous one</p>"

   WITH OBJECT WButton():New( Self )
      :cText := "This button fires a CGI XailerWeb event"
      :OnClick := "Act_Click"
      :cId := "button4"
      :Create()
   END WITH

   WText():New( Self ):cText := "<p>Insert some text on this field and confirm it does NOT disappear when pushing the previous button.</p>"

   WITH OBJECT WEdit():New( Self )
      :cLabel := "Label:"
      :cId := "myEdit"
      :nSize := 50
      :Create()
   END WITH

   WText():New( Self ):cText := "<p></p>"

   WITH OBJECT WButton():New( Self )
      :cText := "This button shows XailerWeb source code"
      :Onclick := "showCode"
      :cId := "btnsource"
      :Create()
   END WITH

   WText():New( Self ):cText := "<p><hr></p>"

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

   ::AddScript( cJs ):lBottom := .t.

   //Engine:LogEnv()

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Click( hEvent ) CLASS WDocMain

   LOCAL cHtml

   TEXT INTO cHtml
      <p>This new paragraph has been created through a CGI fired event.</p>
      <p>If you had pushed the third button before calling this fourth button,
      take a look at text of the city where Oz Software server is. As you can
      see, the city name continues there. XailerWeb magic!"</p>
   ENDTEXT

   WText():New( Self ):cText := cHtml

RETURN NIL