
#include "xa-materialize.ch"
#include "error.ch"

CLASS WDocMain FROM WDoc

   DATA oMainSection    AS CLASS WDocSection
   DATA oMainDiv        AS CLASS WMain
   DATA oContext        AS CLASS WMaterializeContext
   DATA oProducts       AS CLASS WNavbarItem
   DATA oAccount        AS CLASS WNavbarItem
   DATA oBasket         AS CLASS WNavbarItem
   DATA oLogout         AS CLASS WNavbarItem

   METHOD CreateDoc()
   METHOD Footer()

   METHOD DefaultSection()
   METHOD Act_Start( hParams, hEvent )
   METHOD Act_Products( hParams, hEvent )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   WITH OBJECT ::oContext := WMaterializeContext():New( Self )
      :cLanguage := "es"
   END WITH

   //Engine:lDebug := .T.

   Translator():cEngine := "text"

   ::DefaultSection()

   IF ::IsDefault()
      ::Act_Start()
   ENDIF

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultSection() CLASS WDocMain

   LOCAL oDropDown
   LOCAL lTranslate := ::NeedTranslate()

   ::oMainSection := ::AddSection( "Default" )

   WITH OBJECT oDropDown := WDropdown():New( Self )
      :cAlignment := "left"
      :AddItem( "xaWeb",, ::Action( "Act_Products" ):AddParam("type", "1" ) )
      :AddItem( "Xailer",,::Action( "Act_Products" ):AddParam("type", "2" ) )
      :AddItem( "Gestión",,::Action( "Act_Products" ):AddParam("type", "3" ) )
      :lCoverTrigger := .F.
   END WITH

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

   WITH OBJECT  WNavbar():New( ::oMainSection )
      :cId := "navbar"
      //:cCache := "global"
      :oLogo:cText := "OZ Software Ecommerce site"
      :oLogo:oSpan:lTranslate := .T.
      :oStyle:Background := ::oContext:PrimaryColor()
      :oLogo:cSrc := "/assets/OzLogo120.png"
      :oLogo:oImage:nWidth := 64
      :oLogo:oImage:nHeight := 64
      :lCollapseBtn := .T.
      ::oProducts := :AddItem( "Products ", oDropDown, "category" )
      ::oAccount  := :AddItem( "Account", "#", "account_circle" )
      ::oBasket   := :AddItem( "Basket", "#", "shopping_cart" )
      ::oLogout   := :AddItem( "Logout", "#", "logout" )

      ::oProducts:oLink:lTranslate := .T.
      ::oAccount:oLink:lTranslate  := .T.
      ::oBasket:oLink:lTranslate   := .T.
      ::oLogout:oLink:lTranslate   := .T.

      WITH OBJECT ::oBasket:oLink
         WITH OBJECT :AddBadge()
            :oContext:cBkPalette := "red"
            :oStyle:Color := "white"
            :oStyle:Border_radius := "5px;"
            :oStyle:Min_Width := "inherit;"
         END WITH

         :cBadge := "1"
      END WITH

   END WITH

   ::oMainDiv := WMain():New( ::oMainSection )

   ::Footer()

RETURN nil

//------------------------------------------------------------------------------

METHOD Footer() CLASS WDocMain

   WITH OBJECT WFooter():New( Self )
      :cId := "footer"
      :oStyle:Margin_top := "0px"
      :oStyle:Padding_top := "10px"
      WITH OBJECT WDiv():New( SO )
         :AddClass( "container" )
         :oStyle:Display := "flex"
         :oStyle:Justify_content := "center"
         WITH OBJECT WLink():New( SO )
            :cText := "© 2025 Copyright OZ Software"
            :cHRef := "https://www.ozs.es"
            :oStyle:Font_size := "2rem;"
         END WITH
      END WITH
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Start( hParams, hEvent ) CLASS WDocMain

   WITH OBJECT WDiv():New( ::oMainDiv )
      :oStyle:Display := "flex"
      :oStyle:Justify_content := "center"
      WITH OBJECT WImage():New( SO )
         :oStyle:Max_width := "100%"
         :oStyle:Padding := "20px"
         :cSrc := "/assets/OzLogo640.png"
      END WITH
      ::oLogout:lDisabled := .T.
   END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Act_Products( hParams, hEvent ) CLASS WDocMain

   LOCAL oProducts AS CLASS WProducts
   LOCAL cKey, cValue

   oProducts := WProducts():New( Self )

//   oProducts:Html( ::oMainDiv )

   WITH OBJECT WDiv():New( ::oMainDiv )
      :oStyle:Display := "flex"
      :oStyle:Justify_content := "center"
//      WITH OBJECT WDiv():New( SO )
         //:oContext:Row()
         :AddClass( "primary-container" )
         :oStyle:Border_radius := "14px;"
         :oStyle:Margin := "10px 20px;"
         :oStyle:Padding := "1rem;"
         :cText := "hola"
         :oStyle:Font_weight := "600;"
         //ECHO "<h4>Test</h4>"
//         WITH OBJECT WDiv():New( SO )
//            :AddClass( "container row" )
//
//            ECHO "<h4>Test</h4>"
//         END WITH
//      END WITH
   END WITH

   oProducts:Html( ::oMainDiv )

RETURN nil

//------------------------------------------------------------------------------

