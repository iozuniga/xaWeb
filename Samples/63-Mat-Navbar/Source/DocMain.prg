#include "xa-materialize.ch"

CLASS WDocMain FROM WDoc

   DATA oOption AS CLASS WSpan

   METHOD CreateDoc()
   METHOD Test()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL oContainer, oDropdown1
   LOCAL oTask AS CLASS WTask
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
         :cText := "<h3>xaWeb - Materialize Navbar</h3>"
         :Create()
      END WITH

      WITH OBJECT WLink():New( SO )
         :cText := "Follow this link to see the original Materialize page"
         :cHref := "https://materializeweb.com/navbar.html"
         :oStyle:Display := "block"
         :oStyle:Margin_bottom := "20px"
         :Create()
      END WITH

      WITH OBJECT ::oOption :=  WSpan():New( SO )
         :cText := "Option selected: "
         :oStyle:Display := "block"
         :oStyle:Font_size := "20px"
         :Create()
      END WITH
      ECHO "<h3>Standard</h3>"

      oTask := ::Action( "test" )
      oTask:AddParam( "value", "" )

      WITH OBJECT oDropdown1 := WDropdown():New( SO )
         :AddItem( "one", "search", oTask:Html( "Dropdonw One" ) )
         :AddItem( "two", "view_module", oTask:Html( "Dropdonw Two" )    )
         :AddDivider()
         :AddItem( "three", "refresh", oTask:Html( "Dropdonw Three" )    )
      END WITH

      WITH OBJECT WNavbar():New( SO )
         :oLogo:cText := "Logo"
         :oList:oContext:HideOnMedAndDown()
         :AddItem( "Test1", oTask:Html( "Bar 1, Item 1" ) ):lActive := .T.
         :AddItem( "Test2", oTask:Html( "Bar 1, Item 2" ) )
         :AddItem( "Test3", oTask:Html( "Bar 1, Item 3" ) )
         :AddItem( "Test4", oTask:Html( "Bar 1, Item 4" ) )
      END WITH

      ECHO "<h3>With tabs</h3>"

      WITH OBJECT WNavbar():New( SO )
         :oLogo:cText := "Logo"
         :lExtended := .T.
         :oList:oContext:HideOnMedAndDown()
         :AddItem( "Test1", oTask:Html( "Bar 2, Item 1" ) ):lActive := .T.
         :AddItem( "Test2", oTask:Html( "Bar 2, Item 2" ) )
         :AddItem( "Test3", oTask:Html( "Bar 2, Item 3" ) )
         :AddItem( "Test4", oTask:Html( "Bar 2, Item 4" ) )

         WITH OBJECT :oContent
            WITH OBJECT WTabs():New( SO )
               :AddItem( "Test1", "#test1" )
               :AddItem( "Test2", "#test2" )
               :AddItem( "Test3", "#test3" )
               :AddItem( "Test4", "#test4" )
            END WITH
         END WITH
      END WITH

      ECHO "<br>"

      WITH OBJECT WDiv():New( SO )
         :cId := "test1"
         ECHO "test1"
      END WITH

      WITH OBJECT WDiv():New( SO )
         :cId := "test2"
         ECHO "test2"
      END WITH
      WITH OBJECT WDiv():New( SO )
         :cId := "test3"
         ECHO "test3"
      END WITH
      WITH OBJECT WDiv():New( SO )
         :cId := "test4"
         ECHO "test4"
      END WITH

      ECHO "<h3>Dropdown menu</h3>"

      WITH OBJECT WNavbar():New( SO )
         :AddClass( "red" )
         :oLogo:cText := "Logo"
         :oLogo:cIcon := "cloud"
         :oList:oContext:HideOnMedAndDown()
         :AddItem( "One", oTask:Html( "Bar 4, Item 1" )  )
         :AddItem( "Two", oTask:Html( "Bar 4, Item 2" )  )
         :AddItem( "Three", oDropdown1, "arrow_drop_down" )
      END WITH

      ECHO "<h3>Icon links</h3>"

      WITH OBJECT WNavbar():New( SO )
         :oLogo:cText := "Logo"
         :oLogo:cIcon := "cloud"
         :oList:oContext:HideOnMedAndDown()
         :AddItem( "", oTask:Html( "Bar 5, Item search" ) , "search" )
         :AddItem( "", oTask:Html( "Bar 5, Item view_module" ), "view_module" )
         :AddItem( "", oTask:Html( "Bar 5, Item refresh" ), "refresh" )
         :AddItem( "", oTask:Html( "Bar 5, Item more_vert" ), "more_vert" )
         :AddItem( "with text", oTask:Html( "Bar 5, Item with_text" ), "arrow_drop_down" )
      END WITH

      ECHO "<h3>Halfway FAB in Extended Navbar</h3>"

      WITH OBJECT WNavbar():New( SO )
         :oLogo:cText := "Logo"
         :lExtended := .T.
         :oList:oContext:HideOnMedAndDown()
         :AddItem( "Test1", oTask:Html( "Bar 6, Item 1" ) )
         :AddItem( "Test2", oTask:Html( "Bar 6, Item 2" ) )
         :AddItem( "Test3", oTask:Html( "Bar 6, Item 3" ) )
         :AddItem( "Test4", oTask:Html( "Bar 6, Item 4" ) )

         WITH OBJECT :oContent
            WITH OBJECT WSpan():New( SO )
               :cText := "Title"
               :AddClass( "nav-title" )
            END WITH
            WITH OBJECT WButton():New( SO )
               :cDisplayType := "floating"
               :cIcon := "add"
               :cWaveEffect := "light"
               :cIconAlign := ""

               :AddClass( "btn-large halfway-fab" )
            END WITH
         END WITH
      END WITH

      ECHO "<h3>Collpase button</h3>"

      WITH OBJECT WNavbar():New( SO )
         :oLogo:cText := "Logo"
         :lCollapseBtn := .T.
         :oList:oContext:HideOnMedAndDown()
         :AddItem( "Test1", oTask:Html( "Bar 7, Item 1" ) )
         :AddItem( "Test2", oTask:Html( "Bar 7, Item 2" ) )
         :AddItem( "Test3", oTask:Html( "Bar 7, Item 3" ) )
         :AddItem( "Test4", oTask:Html( "Bar 7, Item 4" ) )
         :AddItem( "Test5", oDropdown1, "arrow_drop_down" )
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

//------------------------------------------------------------------------------

METHOD Test( hParams ) CLASS WDocMain

   //LogDebug( hParams )
   ::oOption:cText += hb_hget( hParams, "value" )

RETURN nil