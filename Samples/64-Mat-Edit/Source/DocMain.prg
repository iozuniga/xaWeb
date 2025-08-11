#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oDropdown1
   LOCAL cCode, cJs

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

   ::AddScript( cJs )

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      :Create()
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
   END WITH

   WITH OBJECT oContainer := WMain():New( Self )
      :AddClass( "container" )
      WITH OBJECT WText():New( SO )
         :cText := "<h3>xaWeb - Materialize Edit & AutoComplete</h3>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/text-inputs.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
      END WITH

      ECHO "<h3>Input fields</h3>"

      WITH OBJECT WForm():New( SO )
         :oStyle:gap := "1em"
         :cName := "myform"
         :AddClass( "row" )
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:col( 12, 6 )
            :cId := "first_name"
            :nMaxLength := 20
            :cLabel := "First Name"
            :cSupport := "Supporting text"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:col( 12, 6 )
            :cId := "last_name"
            :nMaxLength := 20
            :cLabel := "Last Name"
            :lOutlined := .T.
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:col( 12, 6 )
            :cId := "disabled"
            :cValue := "I am not editable"
            :cLabel := "Disabled"
            :lDisabled := .T.
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:col( 12, 6 )
            :cId := "disabledToo"
            :cValue := "Not editable too"
            :cLabel := "Disabled"
            :lOutlined := .T.
            :lDisabled := .T.
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cPrefix := "place"
            :cSuffix := "gps_fixed"
            :cId := "inp-location"
            :cValue := "Planet Earth"
            :cLabel := "Location"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :oContainer:AddClass( "error" )
            :oSupport:oContext:TextColor( "white" )
            :lOutlined := .T.
            :cPrefix := "bubble_chart"
            :cSuffix := "error"
            :cId := "inp-error"
            :cValue := "$%/'#sdf"
            :cLabel := "Failing input"
            :cSupport := "Invalid characters! Please use 0-9 only."
            :Create()
         END WITH
      END WITH

      ECHO "<h3>Input types</h3>"

      WITH OBJECT WForm():New( SO )
         :oStyle:gap := "1em"
         :cName := "myform1"
         :AddClass( "row" )
         WITH OBJECT WEmail():New( SO )
            :oContainer:oContext:col( 12, 6 )
            :cId := "email"
            :cLabel := "Email"
            :cSupport := "Helper text"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cType := "password"
            :nMaxLength := 20
            :cId := "password"
            :cLabel := "Password"
            :Create()
         END WITH
      END WITH

      ECHO "<h3>Inline Inputs</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform2"
         :AddClass( "row" )
         WITH OBJECT WDiv():New( SO )
            :AddClass( "s12" )
            :cText := "This is an input field:"
            WITH OBJECT WEmail():New( SO )
               :oContainer:oContext:col( 12, 6 )
               :cId := "email2"
               :cLabel := "Email"
               :Create()
            END WITH
         END WITH
      END WITH

      ECHO "<h3>Icon prefixes</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform3"
         :AddClass( "row" )
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cPrefix := "account_circle"
            :cId := "firs_name_2"
            :cLabel := "First Name"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cPrefix := "phone"
            :cId := "phone"
            :cLabel := "Phone"
            :Create()
         END WITH
      END WITH

      ECHO "<h3>Icon suffixes</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform4"
         :AddClass( "row" )
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cSuffix := "account_circle"
            :cId := "firs_name_3"
            :cLabel := "First Name"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cSuffix := "phone"
            :cId := "phone1"
            :cLabel := "Phone"
            :Create()
         END WITH
      END WITH

      ECHO "<h3>Custom Error or Success Messages</h3>"

      ECHO "This functionality is broken in current version of Materialize. Use lError property."

      ECHO "<h3>Autocomplete</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform6"
         :AddClass( "row" )
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cSuffix := "account_circle"
            :cId := "firs_name_4"
            :cLabel := "Auto complete"
            :Create()
            WITH OBJECT :oAutoComplete
               :AddItem( "1", "One" )
               :AddItem( "2", "Two" )
               :AddItem( "3", "Three" )
               :AddItem( "4", "Four" )
               :AddItem( "5", "Five" )
               :AddItem( "6", "Six" )
               :AddItem( "7", "Seven" )
               :AddItem( "8", "Eight" )
               :AddItem( "9", "Nine" )
               :AddItem( "10", "Ten" )
               :AddItem( "11", "Eleven" )
               :AddItem( "12", "Twelve" )
            END WITH
         END WITH
         WITH OBJECT WEdit():New( SO )
            :oContainer:oContext:Col( 12, 6 )
            :cSuffix := "phone"
            :cId := "pdsafasd2"
            :cLabel := "Auto complete with images"
            :Create()
            WITH OBJECT :oAutoComplete
               :AddItem( "1", "Sample", "https://materializeweb.com/images/sample-1.jpg" )
               :AddItem( "2", "Office", "https://materializeweb.com/images/office.jpg" )
               :AddItem( "3", "Yuna", "https://materializeweb.com/images/yuna.jpg" )
               :nMinLength := 0
            END WITH
         END WITH
      END WITH

      ECHO "<h3>Text area</h3>"

      WITH OBJECT WForm():New( SO )
         :cName := "myform5"
         :AddClass( "row" )
         WITH OBJECT WTextArea():New( SO )
            :oContainer:oContext:Col( 12 )
            :cId := "text_area_1"
            :cLabel := "Textarea"
            //:cText := "hola"
            :Create()
         END WITH
         WITH OBJECT WTextArea():New( SO )
            :oContainer:oContext:Col( 12 )
            :cId := "text_area_2"
            :cLabel := "Textarea with placeholder"
            :cPlaceHolder := "A custom place holder :-)"
            :Create()
         END WITH
      END WITH

      FILE "DocMain.prg" INTO cCode

      ECHO "<hr>"

      WITH OBJECT WButton():New( SO )
         :cText := "This button shows xaWeb source code"
         :Onclick := "showCode"
         :cId := "btnsource"
         :cDisplayType := "filled"
         :Create()
      END WITH
      WITH OBJECT WTextArea():New( SO )
         :oStyle:Margin_top := "20px"
         :oContext:WavesEffect( .T., .T. )
         :AddClass( "materialize-textarea" )
         :nCols := 80
         :cId   := "source"
         :nRows := 40
         :AddDataset( "text", HB_Base64Encode( cCode ) )
         :lReadOnly := .t.
         :lVisible := .f.
         :oStyle:Font_family := "monospace"
         :Create()
      END WITH
   END WITH

RETURN nil

