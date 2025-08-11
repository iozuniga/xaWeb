/*
This example performs a complete CRUD maintenance of a DBF table with a single
URL load. All CRUD processes are completed using FETCH services and embedded
JavaScript code. Printing to PDF does not require any additional utilities on
the web server. The only files used in the example are: a DBF table and the
CGI file itself.
*/

#include "xa-materialize.ch"

REQUEST DBFCDX

CLASS WDocMain FROM WDoc

   DATA oMainSection    AS CLASS WDocSection
   DATA oFormSection    AS CLASS WDocSection
   DATA oForm           AS CLASS WForm
   DATA oContext        AS CLASS WMaterializeContext
   DATA oTable          AS CLASS WTable
   DATA oNavbar         AS CLASS WNavbar
   DATA oSidenav        AS CLASS WSidenav

   METHOD CreateDoc()
   METHOD Sidenav( oParent )
   METHOD Navbar( oParent )

   METHOD DefaultSection()
   METHOD FormSection()

   METHOD Srv_CheckUserCode( hParam )
   METHOD Srv_TableEdit( hParam )
   METHOD Srv_TableDelete( hParam )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL cJs, cPath

   WITH OBJECT ::oContext := WMaterializeContext():New( Self )
      :cLanguage := "es"
   END WITH

//   Engine:lDebug := .T.

   // This simple Javascript function just shows the source code on a hidden TextArea

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

#ifdef _LINUX_
   cPath := HB_DirBase() + "data/"
#else
   cPath := HB_DirBase() + "..\..\resource\"
   cPath := "z:\xaWeb\resource\"
#endif

   // Open and copy (if necessary) the tables on the session folder

   WITH OBJECT ::oSession
      IF !:IsFile( "customer.dbf" )
         :SaveFile( cPath + "customer.dbf", "customer.dbf" )
      ENDIF
      IF !:IsFile( "customer.cdx" )
         :SaveFile( cPath + "customer.cdx", "customer.cdx" )
      ENDIF
      USE (:SessionPath() + "customer.dbf" ) ALIAS Customer SHARED VIA "DBFCDX"
      SET ORDER TO TAG "ID"
      SET DELETED ON
   END WITH

   ::AddScript( cJs )

   ::DefaultSection()
   ::FormSection()

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultSection() CLASS WDocMain

   LOCAL oModalPack, oModal, oPdfPack, oMain
   LOCAL cCode

   ::oMainSection := ::AddSection( "Default" )

   WITH OBJECT WLink():New( ::oMainSection )
      :cId := "theme-switch"
      :Create()
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
   END WITH

   WITH OBJECT WHeader():New( ::oMainSection )
      ::Navbar( SO )
   END WITH

   WITH OBJECT oMain := WMain():New( ::oMainSection )
      :cId := "main"
      :AddClass( "container" )

      WParagraph():New( SO )

      WITH OBJECT ::oTable := WTable():New( SO )
         :cId := "table1"
         :LoadDbf( "customer",  {"Id","First","Last","Street", "City"}, .T. )
         :aHeaders[ 1 ]:cHeader := "ID"
         :oStyle:Width := "100%"
         :cHeaderBkColor := ::oContext:PrimaryColor()
         :cHeaderColor := ::oContext:PrimaryTextColor()
         :cMaxHeight := "600px"
         :lShowSelected := .t.
         :lResponsive := .T.
         :lShowID := .t.
         :lCanSort := .t.
         :lCanFilter := .t.
         :Create()
      END WITH

   END WITH

   oModalPack := WModalMsgBtn():New( Self )

   WITH OBJECT oPdfPack := WPdf():New( Self )
      WITH OBJECT :oAutoTable
         :oTable := ::oTable
         :oStyle:font = "times"
         :oStyle:CellWidth = "auto"
         :cTitle := "xaWeb DBF Crud demo"
         :cSubTitle := "Web development fast & easy! "
         :cLogo := "/assets/logo_square.png"
      END WITH
   END WITH

   WITH OBJECT oModal := oModalPack:ShowModal("xaWeb: DBF Table CRUD", "Confirm row deletion?", {"OK", "Cancel"} )
      :OnClick( 1, ::oTable:DeleteFromService( "Srv_TableDelete", ::oNavbar:aItems[ 3 ] ) )
   END WITH

   WITH OBJECT ::oNavbar
      :aItems[ 1 ]:chRef := ::oTable:Append( "FormSection" )
      :aItems[ 2 ]:chRef := ::oTable:Edit( "FormSection" )
      ::oTable:oEditControl := :aItems[ 2 ]
      ::oTable:oDelControl  := :aItems[ 3 ]
      :aItems[ 3 ]:OnClick := oModal
      :aItems[ 4 ]:OnClick := oPdfPack:oAutoTable
   END WITH

   ::Sidenav( ::oMainSection )

   FILE "DocMain.prg" INTO cCode

   ECHO "<hr>" INTO ::oMainSection

      WITH OBJECT WButton():New( ::oMainSection )
         :cText := "This button shows xaWeb source code"
         :Onclick := "showCode"
         :cId := "btnsource"
         :Create()
      END WITH

      WITH OBJECT WTextArea():New( ::oMainSection )
         :oStyle:Margin_top := "20px"
         :oStyle:Font_Family := "monospace"
         :nCols := 80
         :cId   := "source"
         :nRows := 40
         :AddDataset( "text", HB_Base64Encode( cCode ) )
         :lReadOnly := .t.
         :lVisible := .f.
         :Create()
      END WITH

RETURN nil

//------------------------------------------------------------------------------

METHOD Sidenav( oParent ) CLASS WDocMain

   WITH OBJECT ::oSidenav := WSidenav():New( oParent )
      :cId := "sidenav"
      :cCache := "global"
      WITH OBJECT :AddHeader( "/assets/fondo.jpg" )
         :AddImage( "/assets/logo_square.png" )
         :AddItem( "xaWeb" ):AddClass( "white-text name" )
         :AddItem( "Materialize - DBF CRUD demo" ):AddClass( "white-text email" )
      END WITH
   END WITH

RETURN NIL

//------------------------------------------------------------------------------

METHOD Navbar( oParent ) CLASS WDocMain

   WITH OBJECT ::oNavbar := WNavbar():New( oParent )
      :cId := "navbar"
      :cCache := "global"
      :oLogo:cText := "DBF CRUD demo"
      :oSidenav := ::oSidenav
      :oStyle:Background := ::oContext:PrimaryColor()
      :oLogo:cSrc := "/assets/logo_square.png"
      :oLogo:oImage:nWidth := 64
      :oLogo:oImage:nHeight := 64
      :lCollapseBtn := .T.
      :AddItem( "New ", "#", "person_add" )
      :AddItem( "Edit", "#", "manage_accounts" )
      :AddItem( "Delete", "#", "person_remove" )
      :AddItem( "Print", "#", "print" )
   END WITH

RETURN NIL

//------------------------------------------------------------------------------

METHOD FormSection() CLASS WDocMain

   LOCAL oGroup

   WITH OBJECT ::oFormSection := ::AddSection( "FormSection" )
      :lHide   := .T.
      :lDeploy := .T.
      :lFooter := .T.
   END WITH

   ::AddCSS( ".input-field { margin:1em 0em;}" )

   WITH OBJECT ::oForm := WForm():New( ::oFormSection )
      :AddClass( "Container" )
      :cId := "myform"
      :cCache := "global"
      :cMethod := "post"
      // :cName := "myform" not necesssary since it is a fetech operation
      :cTableId := "table1"
      :lAutoComplete := .T.
      :lModal := .t.
      :AddStyle( "width: 80ch;" )
      :OnSubmit := :SubmitToService( "Srv_TableEdit", .T. )
      :Create()
      WITH OBJECT oGroup := WFieldset():New( ::oForm )
         :cLegend := "Personal data"
         :cName := "User data"
         :Create()
         WITH OBJECT WNumber():New( oGroup )
            :lOutlined := .T.
            :cId := "Id"
            :cLabel := "Id:"
            :cDataField := "Id"
            :lDisabledOnEdit := .T.
            :nMin := 1
            :nMax := 99999
            :nSize := 5
            :OnValidate := "Srv_CheckUserCode"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :lOutlined := .T.
            :cId := "first"
            :cLabel := "First:"
            :cDataField := "First"
            :nMaxLength := 50
            :nMinLength := 4
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :lOutlined := .T.
            :cId := "last"
            :cLabel := "Last:"
            :cDataField := "Last"
            :nMaxLength := 50
            :nMinLength := 4
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :lOutlined := .T.
            :cId := "street"
            :cLabel := "Street:"
            :cDataField := "Street"
            :nMaxLength := 50
            :nMinLength := 10
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :lOutlined := .T.
            :cId := "city"
            :cLabel := "City:"
            :cDataField := "City"
            :nMaxLength := 50
            :nMinLength := 4
            :Create()
         END WITH
      END WITH
      WITH OBJECT WDiv():New( ::oForm )
         :oStyle:Padding := "1em 0em"
         WITH OBJECT WButton():New( SO )
            :cDisplayType := "filled"
            :cType := "submit"
            :cText := "Ok"
            :cId := "btnOk"
            :cForm := "myform"
            :Create()
         END WITH
         WITH OBJECT WButton():New( SO )
            :cDisplayType := "filled"
            :cText := "Cancel"
            :cId := "btnCancel"
            :cType := "cancel"
            :Create()
         END WITH
         WITH OBJECT WButton():New( SO )
            :cDisplayType := "filled"
            :cText := "Reset"
            :cId := "btnReset"
            :cType := "reset"
            :Create()
         END WITH
      END WITH
   END WITH

RETURN ::oFormSection

//------------------------------------------------------------------------------

METHOD Srv_TableEdit( hParam ) CLASS WDocMain

   LOCAL hData := Engine:hPost
   LOCAL nTimeOut := 5, nId
   LOCAL lAppend, lOk

   lAppend := ( HB_HGetDef( hData, "append_mode", "" ) == "true" )
   lOk     := .F.
   nId     := Val( hData[ "id" ] )

   IF lAppend
      DO WHILE nTimeOut > 0
         Customer->( DBAppend() )
         IF !NetErr()
            lOk := .t.
            EXIT
         ENDIF
         hb_idleSleep( 0.5 )
         nTimeOut -= 0.5
      ENDDO

      IF lOk
         Customer->Id := nId
      ELSE
      ENDIF
   ELSE
      IF Customer->( DBSeek( nId ) )
         DO WHILE nTimeOut > 0
            IF Customer->( DBRLock() )
               lOk := .t.
               EXIT
            ENDIF
            hb_idleSleep( 0.5 )
            nTimeOut -= 0.5
         ENDDO
      ENDIF
   ENDIF

   IF lOk
      Customer->First  := hData[ "first" ]
      Customer->Last   := hData[ "last" ]
      Customer->Street := hData[ "street" ]
      Customer->City   := hData[ "city" ]
      Customer->( DBGoTo( RecNo() ) )
   ELSE
      hData := HB_Hash()
   ENDIF

RETURN HB_JsonEncode( hData )

//------------------------------------------------------------------------------

METHOD Srv_TableDelete( hParam ) CLASS WDocMain

   LOCAL hData := Engine:hCargo, hResult := { => }
   LOCAL nId, nTimeOut := 5
   LOCAL lOk := .f.

   nId := Val( hData[ "id" ] )

   IF Customer->( DBSeek( nId ) )
      DO WHILE nTimeOut > 0
         IF Customer->( DBRLock() )
            Customer->( DBDelete() )
            lOk := .t.
            EXIT
         ENDIF
         hb_idleSleep( 0.5 )
         nTimeOut -= 0.5
      ENDDO
   ENDIF

   HB_HSet( hResult, "pass", lOk )

RETURN HB_JsonEncode( hResult )

//------------------------------------------------------------------------------

METHOD Srv_CheckUserCode( hParam ) CLASS WDocMain

   LOCAL hRet
   LOCAL cId
   LOCAL nId
   LOCAL lAppend, lFound

   hRet    := { => }
   lAppend := ( HB_HGetDef( hParam, "append_mode", "" ) == "true" )

   IF lAppend
      cId := HB_HGetDef( hParam, "value", "" )
      nId := Val( cId )
      IF Empty( nId )
         HB_HSet( hRet, "pass", .F. )
         HB_HSet( hRet, "error", "ID field must have a value" )
      ELSE
         lFound := Customer->( DBSeek( nId ) )
         IF lFound
            HB_HSet( hRet, "pass", .F. )
            HB_HSet( hRet, "error", "The code " + cId + " already exists in the DBF file (resolved in Harbour)")
         ELSE
            HB_HSet( hRet, "pass", .T. )
            HB_HSet( hRet, "error", "")
         ENDIF
      ENDIF
   ELSE
      HB_HSet( hRet, "pass", .T. )
      HB_HSet( hRet, "error", "" )
   ENDIF

RETURN HB_JsonEncode( hRet )

//------------------------------------------------------------------------------

