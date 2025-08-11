
#define __MATERIALIZE__

#ifdef __MATERIALIZE__
   #include "xa-materialize.ch"
   REQUEST WSwitch,WDatePicker,WTimePicker
#else
   #include "xaWeb.ch"
#endif

#define SOCKET_PORT     50260
#define SOCKET_TIMEOUT  1200 // 20 Minutes
#define SOCKET_DEBUG    .f.

/* IMPORTANT:
 Set the value SOCKET_FORCED to true only once and run the app, to have Windows
 ask you if you authorize the port to be opened.
*/

#define SOCKET_FORCED   .f.

REQUEST WForm, WButton,WCheckbox,WCmpButtonIcon,WCmpButtonSpinner,;
        WCmpNumericKeypad,WColor,WDateTime,WDiv,WEdit,WEmail,WFieldset,WFile,;
        WFlexRow,WIconGoogle,WImage,WLabel,WLink,WList,WNumber,WParagraph,;
        WRadio,WRadioMenu,WRange,WSelect,WSpan,WSyntaxHilite,WText,;
        WTextArea

//------------------------------------------------------------------------------

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL oForm, oDiv
   LOCAL cResFile,cCss
   LOCAL nAt, nSocket, nTimeOut, nPort

   // Put any CSS code here ...

  TEXT INTO cCss
      Input {
         margin-top: 0.5em;
         margin-bottom: 0.5em;
      }
      fieldset {
         border-radius: 4px;
         padding: 1em 1em;
     }
     button {
        margin-top: 0.5em;
     }
   ENDTEXT

   ::AddCSS( cCss )

#ifdef __MATERIALIZE__
   WITH OBJECT WMaterializeContext():New( Self )
      :cLanguage := "es"
   END WITH

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      :Create()
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "light_mode"
      END WITH
   END WITH

   WITH OBJECT oDiv := WDiv():New( Self )
      :AddClass( "container" )
   END WITH

#else
   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   WITH OBJECT oDiv := WDiv():New( Self )
   END WITH
#endif

   //HB_SetEnv( "RESOURCE_FILE", "ONLY_SOCKET:" + ToString( 50260 ) )

   cResFile := GetEnv( "RESOURCE_FILE" )

   IF SOCKET_FORCED
      cResFile := "ONLY_SOCKET"
      nTimeOut := 1
   ELSE
      nTimeOut := SOCKET_TIMEOUT
   ENDIF

   IF Empty( cResFile )
      ECHO "No resource file. Imposible to continue." INTO oDiv
   ELSEIF Hb_LeftEq( cResFile, "ONLY_SOCKET" )
      nAt := At( ":", cResFile )
      IF nAt > 0
         nSocket := Val( SubStr( cResFile, nAt + 1 ) )
      ELSE
         nSocket := SOCKET_PORT
      ENDIF
      nPort := Engine:Listen( nTimeOut, nSocket, SOCKET_DEBUG )
      ECHO "Socket tested on port: " + ToString( nPort ) + " ..." INTO oDiv
   ELSEIF !HB_FileExists( cResFile )
      ECHO "Resource file not found: " + cResFile + ". Imposible to continue." INTO oDiv
   ELSE
      WITH OBJECT WFormManager():New( cResFile )
         IF Empty( :cError )
            oForm := :DeployForm( "", oDiv ) // Loads first form in resources
            IF !HB_IsObject( oForm )
               ECHO "Form Manager Error: (" + :cError + ")" INTO oDiv
            ENDIF
         ELSE
            ECHO "Error on WFormManager constructor: (" + :cError + ")" INTO oDiv
         ENDIF
         :End() // Important for garbage collector
      END WITH
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

