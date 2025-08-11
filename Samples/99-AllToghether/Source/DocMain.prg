// https://test2.ozs.es/cgi-bin/xaweb/alltoghether.cgi

#include "xa-materialize.ch"
#include "fileio.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

METHOD CreateDoc() CLASS WDocMain

   LOCAL aModule
   LOCAL cFile, cBuffer
   LOCAL hLog

   wMaterializeContext():New( Self )

   cFile := HB_DirBase() + "logs" + HB_OsPathSeparator() + "alltoghether.log"

   IF HB_FileExists( cFile )
      hLog := FOpen( cFile, FO_READWRITE )
      FSeek( hLog, 0, FS_END )
   ELSE
      hLog := FCreate( cFile, FC_NORMAL )
   ENDIF

   cBuffer := "> " + HB_ValToStr( HB_DateTime() ) + " - " + Engine:RemoteAddr() + hb_eol()

   FWrite( hLog, cBuffer )

   FClose( hLog )

   WITH OBJECT WLink():New( Self )
      :cId := "theme-switch"
      WITH OBJECT WIconGoogle():New( SO )
         :cText := "dark_mode"
      END WITH
      :Create()
   END WITH

   WITH OBJECT WDiv():New( Self )
      :AddClass( "container" )
      WITH OBJECT WCollection():New( SO )
         :AddHeader( "xaWeb: All toghether!!", 4 )
         FOR EACH aModule IN Modulos()
            :AddItem( aModule[ 2 ], "https://xaweb.ozs.es/cgi-bin/" + aModule[ 1 ] )
         NEXT
      END WITH
   END WITH

RETURN nil

//------------------------------------------------------------------------------

STATIC FUNCTION Modulos()

   LOCAL aModulos := { { "hello.xaweb", "01-Hello world!"},;
                       { "buttons.xaweb", "02-Buttons"},;
                       { "containers.xaweb", "03-Containers"},;
                       { "simpleform.xaweb", "04-Simple form"},;
                       { "select.xaweb", "05-Select"},;
                       { "image.xaweb", "06-Images"},;
                       { "tables.xaweb", "07-Tables"},;
                       { "nav.xaweb", "08-Nav"},;
                       { "login.xaweb", "09-Login"},;
                       { "modal.xaweb", "10-Package Modal"},;
                       { "watercss.xaweb", "11-Context Water.css"},;
                       { "simplecss.xaweb", "12-Context Simple.css"},;
                       { "modalform.xaweb", "13-Modal form" },;
                       { "modalformfetch.xaweb", "14-Modal form via Fetch" },;
                       { "googleicons.xaweb", "15-Google Icons" },;
                       { "forminit.xaweb", "16-Form Init" },;
                       { "inputmask.xaweb", "17-Input Mask" },;
                       { "tablesresponsive.xaweb","20-Tables Responsive"},;
                       { "tablesautosort.xaweb", "21-Tables Autosort"},;
                       { "tablesrowclick.xaweb", "22-Tables Row click"},;
                       { "tablesrowfilter.xaweb", "23-Tables Auto filter"},;
                       { "tablesloaddata.xaweb", "24-Tables Load data"},;
                       { "tablescrud.xaweb", "25-Tables CRUD"},;
                       { "custombutton.xaweb", "30-Icon-button / Spinner-button"},;
                       { "xailerwebdatasource.xaweb", "41-XailerWebDatasource (MySQL)"},;
                       { "mathello.xaweb", "50-Materialize - Hello world!"},;
                       { "matbuttons.xaweb", "51-Materialize - Buttons"},;
                       { "matcards.xaweb", "52-Materialize - Cards"},;
                       { "matcollections.xaweb", "53-Materialize - Collections"},;
                       { "matsidenav.xaweb", "54-Materialize - Sidenav"},;
                       { "matfloatactbtn.xaweb", "55-Materialize - Float action button"},;
                       { "matpagination.xaweb", "56-Materialize - Pagination & Preloader"},;
                       { "matcarousel.xaweb", "57-Materialize - Carousel"},;
                       { "matcollapsible.xaweb", "58-Materialize - Collapsible"},;
                       { "matdropdown.xaweb", "59-Materialize - Dropdown"},;
                       { "matboxslider.xaweb", "60-Materialize - Material Box & Slider"},;
                       { "matmodal.xaweb", "61-Materialize - Modal"},;
                       { "mattabs.xaweb", "62-Materialize - Tabs"},;
                       { "matnavbar.xaweb", "63-Materialize - Navbar"},;
                       { "matedit.xaweb", "64-Materialize - Edit"},;
                       { "matcheckbox.xaweb", "65-Materialize - Checkbox & Radio-button"},;
                       { "matdatetime.xaweb", "66-Materialize - Date & Time Pickers"},;
                       { "matrange.xaweb", "67-Materialize - Range & Chips & Switches"},;
                       { "matselect.xaweb", "68-Materialize - Select"},;
                       { "matform.xaweb", "69-Materialize - Forms using xaWeb validation"},;
                       { "mattable.xaweb", "70-Materialize - XW Tables supported"},;
                       { "matcontacts.xaweb", "71-Materialize - DBF tables"},;
                       { "matcontactspagination.xaweb", "72-Materialize - DBF tables with pagination"},;
                       { "matcontactscrud.xaweb", "73-Materialize - DBF CRUD (include printing)"},;
                       { "matxailersqlite.xaweb", "74-Mat & Xailer - SQLite sample"},;
                       { "matxailermysql.xaweb", "75-Mat & Xailer - MySQL sample"},;
                       { "matmysql.xaweb", "76-Mat & (xaWeb DS) - MySQL sample"},;
                       { "matsqlite.xaweb", "76-Mat & (xaWeb DS) - SQLite sample"};
                       }


RETURN aModulos

//------------------------------------------------------------------------------



