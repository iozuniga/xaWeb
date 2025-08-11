
#include "xa-materialize.ch"
#include "error.ch"

CLASS WDocMain FROM WDoc

   DATA oMainSection    AS CLASS WDocSection
   DATA oContext        AS CLASS WMaterializeContext
   DATA oSidenav        AS CLASS WSidenav
   DATA oSideList       AS CLASS WList
   DATA oMainDiv        AS CLASS WDiv
   DATA oBtnList        AS CLASS WLink

   DATA oHelpGen        AS CLASS TGen

   METHOD CreateDoc()
   METHOD SideList( oParent )
   METHOD Header()
   METHOD Footer()
   METHOD GetHelpInfo()

   METHOD DefaultSection()
   METHOD Act_Start( hParams, hEvent )
   METHOD Act_Help( hParams )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   WITH OBJECT ::oContext := WMaterializeContext():New( Self )
      :cLanguage := "es"
   END WITH

   //Engine:lDebug := .T.

   ::GetHelpInfo()

   ::DefaultSection()

   IF ::IsDefault()
      ::Act_Start()
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD GetHelpInfo() CLASS WDocMain

   LOCAL cFile
   LOCAL lOk

   cFile := HB_DirBase() + "data/xaWebHelpInfo.txt"

   WITH OBJECT ::oHelpGen := TBasicGen():New( Self )
      IF !:LoadFile( cFile )
         WITH OBJECT ErrorNew()
            :Subsystem   := ERROR_SUBSYSTEM
            :Severity    := ES_ERROR
            :Description := "Could not find xaWeb help file info"
            :Operation   := "WDocMain:GetHelpInfo()"
            Eval( ErrorBlock(), :__WithObject(), 3 )
         END WITH
         RETURN NIL
      ENDIF
   END WITH

RETURN NIL

//------------------------------------------------------------------------------

METHOD DefaultSection() CLASS WDocMain


   ::oMainSection := ::AddSection( "Default" )

   WITH OBJECT WFlexRow():New( ::oMainSection )
      :oStyle:Justify_content := "space-between"
      :oStyle:Align_items := "center"
      WITH OBJECT WLink():New( SO )
         :AddClass( "col" )
         :cId := "theme-switch"
         :Create()
         WITH OBJECT WIconGoogle():New( SO )
            :cText := "dark_mode"
         END WITH
      END WITH
      WITH OBJECT WLink():New( SO )
         :oStyle:Padding := "0px 4px"
         :cText := "EN"
         :cId := "lang-switch"
         :Create()
      END WITH
   END WITH

   ::Header()

   WITH OBJECT WMain():New( ::oMainSection )
      :oStyle:Display := "flex"
      ::SideList( SO )

      WITH OBJECT ::oMainDiv := WDiv():New( SO )
         :AddClass( "col" )
      END WITH
   END WITH

   ::Footer()

RETURN nil

//------------------------------------------------------------------------------

METHOD SideList( oParent ) CLASS WDocMain

   LOCAL oTask
   LOCAL aItems := { "Start" },;
         aLinks := { ::Action( "Act_Start" ) },;
         aClasses
   LOCAL cText, cLink, cAction, cSymbol, cJs, cCss

   TEXT INTO cJs
      const activeSideListItem = document.getElementById("sideList").querySelector(".active");
      if (activeSideListItem ) {
        activeSideListItem.scrollIntoView({
           behavior: 'instant',
           block: 'center',
           inline: 'nearest'
        });
      }
   ENDTEXT

   ::AddScript( cJs )

   TEXT INTO cCss
   #sideList {
      height: 100svh;
      overflow: scroll;
      overflow-x: hidden;
      margin: 0px;
      padding: 6px 4px;
      }

   #sideList li>a {
      width: 100%;
      height: 1.8em;
      margin: 2px 0px;
      align-items: center;
      overflow: hidden;
      border-radius: 6px;
      padding: 0px 12px;
      }

   @media only screen and (min-width: 600px) {
      #sideList {
      height: 85vh;
      display: block;
      }

   @media only screen and (min-width: 768px) {
      #sideList {
      height: 75vh;
      display: block;
      }
   }
   ENDTEXT

   ::AddCSS( cCss )

   //Translator():lReindex := .T.

   IF HB_HHasKey( Engine:hParams, "symbol" )
      cSymbol := hb_hget( Engine:hParams, "symbol" )
      IF Left( cSymbol, 1 ) == "Z"
         SwapChr( @cSymbol, 1, "W" )
      ENDIF
   ENDIF

   IF ::cLang != "EN"
      aItems := Translator():TranslateArray( aItems )
   ENDIF

   aClasses := ::oHelpGen:GetClasses()
   oTask    := ::Action( "Act_Help" )
   oTask:AddParam( "symbol", "" )

   FOR EACH cText IN aClasses
      oTask:SetParam( 1, cText )
      AAdd( aItems, cText )
      AAdd( aLinks,  oTask:Html() )
   NEXT

   WITH OBJECT WDiv():New( oParent )
      :AddClass( "col" )
      WITH OBJECT ::oSideList := WList():New( SO )
         :cId := "sideList"
         :oContext:HideOnSmallOnly()
         :AddClass( "col" )
         :oStyle:Background := ::oContext:OpaqueColor( 0.05 )
         FOR EACH cText, cLink IN aItems, aLinks
            WITH OBJECT :Additem()
               WITH OBJECT WButton():New( SO )
                  :cText := cText
                  :cHRef := cLink
                  :cWaveEffect := "normal"
                  IF !Empty( cSymbol ) .AND. cSymbol == cText
                     :AddClass( "active" )
                     :oStyle:Color := ::oContext:AccentColor()
                  ENDIF
               END WITH
            END WITH
         NEXT
      END WITH
   END WITH

RETURN ::oSideList

//------------------------------------------------------------------------------

METHOD Header() CLASS WDocMain

   LOCAL aSymbols, aSymbol
   LOCAL cJs
   LOCAL n := 0

   TEXT INTO cJs
      function sideListView(e) {
         const ele = document.getElementById( "sideList" )
         if (ele) {
            ele.classList.toggle( "hide-on-small-only" );
         }
      }
      function topicSearch(e) {
         const ele = document.getElementById( "search" );
         e.preventDefault();
         if (ele && ele.value) {
            xa_runAction(ele, `Act_Help&symbol=${ele.value}` , e);
         }
      }
      function searchKey(e) {
         if (e.key == "Enter")
            topicSearch(e);
      }
   ENDTEXT

   ::AddScript( cJs )

   WITH OBJECT WHeader():New( ::oMainSection )
      WITH OBJECT WFlexRow():New( SO )
         :oStyle:Justify_content := "space-between"
         :oStyle:Align_items := "center"
         :oStyle:Height := "50px"
         :oStyle:Gap := "2px"
         :oStyle:Padding := "0px 10px"
         :oContext:cBkPalette := "blue"
         :oContext:TextColor( "white" )

         WITH OBJECT ::oBtnList := WLink():New( SO )
            :cId := "btnlist"
            :AddClass( "col" )
            :oStyle:Margin := "16px 0px"
            :OnClick := "sideListView"
            :oContext:HideOnMedAndUp()
            :Create()
            WITH OBJECT WIconGoogle():New( SO, Self )
               :cText := "menu"
               :cTextColor := "white"
            END WITH
         END WITH

         WITH OBJECT WImage():New( SO )
            :AddClass( "col" )
            :cSrc := "/assets/logo_square.png"
            :oStyle:Height := "40px"
         END WITH

         WITH OBJECT WSpan():New( SO )
            :AddClass( "col" )
            :oStyle:Margin_left := "10px"
            :oStyle:Font_size := "2rem"
            :cText := "xaWeb"
            :oContext:HideOnSmallOnly()
         END WITH

         WITH OBJECT WFlexRow():New( SO )
            :oStyle:Align_items := "center"
            :oStyle:Gap := "4px"
            :AddClass( "col" )
            WITH OBJECT WEdit():New( SO )
               :cId := "search"
               :AddClass( "col" )
               :lCharCounter := .F.
               :lOutlined := .t.
               :lValidate := .f.
               :oStyle:Height := "40px"
               :cType := "search"
               :cPlaceHolder := "Enter topic"
               :oPrefix:cTextColor := "blue"
               :lTranslate := .t.
               :cAutoComplete := "off"
               :OnKeyUp := "searchKey"
               :oPrefix:oStyle:Align_content := "center"
               :Create()
               ::oHelpGen:GetSymbols( @aSymbols )
               ASort( aSymbols,,, {|x,y| x[1] < y[1] } )
               WITH OBJECT :oAutoComplete
                  FOR EACH aSymbol IN aSymbols
                     :AddItem( NIL, aSymbol[ 1 ] )
                  NEXT
               END WITH
            END WITH
            WITH OBJECT WLink():New( SO )
               :cId := "searchLink"
               :AddClass( "col" )
               :oContext:WavesEffect()
               :OnClick := "topicSearch"
               :Create()
               WITH OBJECT WIconGoogle():New( SO )
                  :oStyle:Margin_top := "2px"
                  :cText := "search"
                  :cTextColor := "white"
               END WITH
            END WITH
         END WITH

      END WITH
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Footer() CLASS WDocMain

   WITH OBJECT WFooter():New( Self )
      :cId := "footer"
      :oStyle:Margin_top := "0px"
      :oStyle:Padding_top := "10px"
      WITH OBJECT WDiv():New( SO )
         :AddClass( "container" )
         WITH OBJECT WFlexRow():New( SO )
            WITH OBJECT WDiv():New( SO )
               WITH OBJECT WSpan():New( SO )
                  :cText := "<h5>xaWeb Reference guide</h5>"
                  :lTranslate := .T.
               END WITH
               WITH OBJECT WParagraph():New( SO )
                  :oContext:TextColor( "yellow" )
                  :cText := "<b>Web development fast & easy!</b>"
                  :lTranslate := .T.
               END WITH
               WITH OBJECT WLink():New( SO )
                  :cText := "© 2024 Copyright OZ Software"
                  :cHRef := "https://www.ozs.es"
               END WITH
            END WITH
            WITH OBJECT WDiv():New( SO )
               WITH OBJECT WSpan():New( SO )
                  :cText := "<h5>Links</h5>"
                  :lTranslate := .T.
               END WITH
               WITH OBJECT WDiv():New( SO )
                  :cTag := "ul"
                  :oStyle:Line_height := "2em"
                  WITH OBJECT WDiv():New( SO )
                     :cTag := "li"
                     WITH OBJECT WLink():New( SO )
                        :cText := "Materialize"
                        :cHRef := "https://materializeweb.com/"
                     END WITH
                  END WITH
                  WITH OBJECT WDiv():New( SO )
                     :cTag := "li"
                     WITH OBJECT WLink():New( SO )
                        :cText := "CSS Tutorial"
                        :cHRef := "https://www.w3schools.com/css/"
                     END WITH
                  END WITH
                  WITH OBJECT WDiv():New( SO )
                     :cTag := "li"
                     WITH OBJECT WLink():New( SO )
                        :cText := "HTML Tutorial"
                        :cHRef := "https://www.w3schools.com/html/"
                     END WITH
                  END WITH
                  WITH OBJECT WDiv():New( SO )
                     :cTag := "li"
                     WITH OBJECT WLink():New( SO )
                        :cText := "xaWeb programming guide"
                        :lTranslate := .T.
                        :cHRef := "https://www.ozs.es/files/xaWeb/xaWeb.pdf"
                     END WITH
                  END WITH
               END WITH
            END WITH
         END WITH
      END WITH
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Start( hParams, hEvent ) CLASS WDocMain

   WITH OBJECT ::oMainDiv
      :oStyle:Margin := "auto"
      WITH OBJECT WImage():New( SO )
         :oStyle:Max_width := "100%"
         :oStyle:Padding := "20px"
         :cSrc := "/assets/logo.png"
      END WITH
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Help( hParams, hEvent ) CLASS WDocMain

   LOCAL oSymbol
   LOCAL cParam, cSymbol, cClass
   LOCAL nAt

   cParam := HB_HGetDef( Engine:hParams, "symbol", "" )

   IF ( nAt := At( "[", cParam ) ) > 0
      cClass  := SubStr( cParam, nAt +1, Len( cParam ) - nAt - 1 )
      cSymbol := Trim( Left( cParam, nAt - 1 ) )
   ELSE
      cClass  := cParam
      cSymbol := ""
   ENDIF

   ::oMainDiv:AddStyle( "max-height: 80vh; overflow: scroll; padding: 1rem; overflow-x: hidden; width: 100%;" )

   oSymbol := ::oHelpGen:FindObject( cClass, cSymbol )

   IF HB_IsObject( oSymbol )
      oSymbol:HtmlInfo( ::oMainDiv )
   ELSE
      IF ::NeedTranslate()
         WText():New( ::oMainDiv ):cText := "<h4>No se encontró información de: <b>" + cParam + "</b></h4>"
      ELSE
         WText():New( ::oMainDiv ):cText := "<h4>Information not found for: <b>" + cParam + "</b></h4>"
      ENDIF
   ENDIF

RETURN nil

//------------------------------------------------------------------------------
