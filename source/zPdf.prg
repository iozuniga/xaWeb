/*
 * Proyecto: xaWeb framework
 * Fichero: zPdf.prg
 * Descripción: PDF package
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Credits: https://www.npmjs.com/package/jspdf
 */

#include "xaWeb.ch"
#include "error.ch"

CLASS ZPdf FROM WPackage
PUBLISHED:
   METHOD oAutoTable( oValue ) SETGET AS CLASS WPdfAutoTable

RESERVED:
   DATA cName        INIT "xa_Pdf" READONLY

   METHOD New( oDoc AS CLASS WDoc, nPos )   CONSTRUCTOR
   METHOD End()
   METHOD PreProcess()

PROTECTED:
   DATA foAutoTable

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oDoc, nPos ) CLASS ZPdf

   ::Super:New( oDoc, nPos )

   IF ::nIndex > 0
      ::AddScript( "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js",  ::cName, .T. )
   ENDIF

RETURN Self

//------------------------------------------------------------------------------

METHOD End()

   IF HB_IsObject( ::foAutoTable )
      ::foAutoTable:End()
      ::foAutoTable := NIL
   ENDIF

RETURN ::Super:End()

//------------------------------------------------------------------------------

METHOD oAutoTable( oValue ) CLASS ZPdf

   IF PCount() > 0 .and. HB_IsObject( oValue )
      ::foAutoTable := oValue
   ELSEIF HB_IsNIL( ::foAutoTable )
      ::foAutoTable := WPdfAutoTable():New( Self )
   ENDIF

RETURN ::foAutoTable

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZPdf

   IF HB_IsObject( ::foAutoTable )
      ::AddScript( "https://unpkg.com/jspdf-autotable",  ::cName, .T. )
   ENDIF

   ::AddScript( Script(), ::cName, .F. )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

STATIC FUNCTION Script()

   LOCAL cScript

   TEXT INTO cScript
      const { jsPDF } = window.jspdf;

      async function xa_imgToBase64(src, info) {
         return new Promise((resolve, reject) => {
            const outputFormat = src.substr(-3) === 'png' ? 'image/png' : 'image/jpeg';
            const img = new Image();
            img.crossOrigin = 'Anonymous';
            img.onload = function() {
               const canvas = document.createElement('CANVAS');
               const ctx = canvas.getContext('2d');
               let dataURL;
               canvas.height = this.naturalHeight;
               canvas.width = this.naturalWidth;
               if (info) {
                  info['width'] = canvas.width;
                  info['height'] = canvas.height;
                  }
               ctx.drawImage(this, 0, 0);
               dataURL = canvas.toDataURL(outputFormat);
               resolve( dataURL );
            };
            img.onerror = reject;
            img.src = src;
         });
      }

      async function xa_pdfAutoTable(options) {
         const doc = new jsPDF();
         const table = document.getElementById(options.html.slice(1));
         const searchRow = table.querySelector(".table-search-row");
         let oldDisplay;
         let finalX = 14;
         let finalY = 15;

         if (options.logo) {
            const info = {};
            await xa_imgToBase64(options.logo, info).then( urlData => {
               const outputFormat = options.logo.substr(-3) === 'png' ? 'PNG' : 'JPEG';
               doc.addImage(urlData, outputFormat, finalX, finalY-10, 20, 20);
               finalX += 20;
            });
         }
         if (options.title) {
            doc.setFontSize(20);
            doc.text(options.title, finalX, finalY);
            }
         if (options.subTitle) {
            doc.setFontSize(12);
            finalY += 8
            doc.text(options.subTitle, finalX, finalY);
            finalY += 10
            }
         options[ 'startY' ] = finalY;
         if (searchRow)
            oldDisplay = searchRow.style.display;
            searchRow.style.display="none";

         doc.autoTable(options);
         doc.save(options.filename);
         if (searchRow)
            searchRow.style.display = oldDisplay;
      }
   ENDTEXT

RETURN cScript

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZPdfAutoTable
PUBLISHED:
   DATA oParent
   DATA oTable                AS CLASS WTable
   DATA cFilename             INIT "autotable.pdf"
   DATA cTitle                INIT ""
   DATA cSubTitle             INIT ""
   DATA cLogo                 INIT ""
   DATA cTheme                INIT "" VALUES "stripped", "grid", "plain"
   DATA lUseCss               INIT .F.
   DATA nStartY               INIT 0
   DATA nMargin               INIT 40
   DATA cPageBreak            INIT "" VALUES "auto", "avoid", "always"
   DATA cRowPageBreak         INIT "" VALUES "auto", "avoid"
   DATA cTableWidth           INIT "" VALUES "auto", "wrap", "number"
   DATA cShowHead             INIT "" VALUES "everyPage", "firstPage", "never"
   DATA cShowFoot             INIT "" VALUES "everyPage", "firstPage", "never"
   DATA nTableLineWidth       INIT 0
   DATA nTableLineColor       INIT 200
   DATA lHorizontalPageBreak  INIT .F.
   DATA cHorizontalPageBreakRepeat  INIT "" VALUES "*"
   DATA cHorizontalPageBreakBehaviour  INIT "" VALUES "immediately", "afterAllRows"

   METHOD oStyle()            SETGET    AS CLASS WPdfAutoTableStyle
   METHOD oHeadStyle()        SETGET    AS CLASS WPdfAutoTableStyle
   METHOD oBodyStyle()        SETGET    AS CLASS WPdfAutoTableStyle
   METHOD oFootStyle()        SETGET    AS CLASS WPdfAutoTableStyle
   METHOD oAlternateRowStyle() SETGET   AS CLASS WPdfAutoTableStyle
   METHOD oColumnStyle()      SETGET    AS CLASS WPdfAutoTableStyle

RESERVED:
   DATA foStyle
   DATA foHeadStyle
   DATA foBodyStyle
   DATA foFootStyle
   DATA foAlternateRowStyle
   DATA foColumnStyle

   METHOD New( oParent )      CONSTRUCTOR
   METHOD End()
   METHOD Html()

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent ) CLASS ZPdfAutoTable

   IF hb_isobject( ::foStyle )
      ::foStyle:End()
       ::foStyle := NIL
   ENDIF
   IF hb_isobject( ::foHeadStyle )
      ::foHeadStyle:End()
       ::foHeadStyle := NIL
   ENDIF
   IF hb_isobject( ::foBodyStyle )
      ::foBodyStyle:End()
      ::foBodyStyle := NIL
   ENDIF
   IF hb_isobject( ::foFootStyle )
      ::foFootStyle:End()
      ::foFootStyle := NIL
   ENDIF
   IF hb_isobject( ::foAlternateRowStyle )
      ::foAlternateRowStyle:End()
      ::foAlternateRowStyle := NIL
   ENDIF
   IF hb_isobject( ::foColumnStyle )
      ::foColumnStyle:End()
      ::foColumnStyle := NIL
   ENDIF

   ::oParent := oParent

RETURN Self

//------------------------------------------------------------------------------

METHOD End() CLASS ZPdfAutoTable

   ::oParent := NIL
   ::oTable  := NIL

RETURN nil

//------------------------------------------------------------------------------

METHOD oStyle() CLASS ZPdfAutoTable

   IF ::foStyle == NIL
      ::foStyle := WPdfAutoTableStyle():New( "styles" )
   ENDIF

RETURN ::foStyle

//------------------------------------------------------------------------------

METHOD oHeadStyle() CLASS ZPdfAutoTable

   IF ::foHeadStyle == NIL
      ::foHeadStyle := WPdfAutoTableStyle():New( "headStyles" )
   ENDIF

RETURN ::foHeadStyle

//------------------------------------------------------------------------------

METHOD oBodyStyle() CLASS ZPdfAutoTable

   IF ::foBodyStyle == NIL
      ::foBodyStyle := WPdfAutoTableStyle():New( "bodyStyles" )
   ENDIF

RETURN ::foBodyStyle

//------------------------------------------------------------------------------

METHOD oFootStyle() CLASS ZPdfAutoTable

   IF ::foFootStyle == NIL
      ::foFootStyle := WPdfAutoTableStyle():New( "footStyles" )
   ENDIF

RETURN ::foFootStyle

//------------------------------------------------------------------------------

METHOD oAlternateRowStyle() CLASS ZPdfAutoTable

   IF ::foAlternateRowStyle == NIL
      ::foAlternateRowStyle := WPdfAutoTableStyle():New( "alternateRowStyles" )
   ENDIF

RETURN ::foAlternateRowStyle

//------------------------------------------------------------------------------

METHOD oColumnStyle() CLASS ZPdfAutoTable

   IF ::foColumnStyle == NIL
      ::foColumnStyle := WPdfAutoTableStyle():New( "columnStyles" )
   ENDIF

RETURN ::foColumnStyle

//------------------------------------------------------------------------------

METHOD Html() CLASS ZPdfAutoTable

   LOCAL hHash   := {=>}
   LOCAL oStyle
   LOCAL aStyles := { ::foStyle, ::foHeadStyle, ::foBodyStyle, ::foFootStyle, ;
                      ::foAlternateRowStyle, ::foColumnStyle }

   IF !HB_IsObject( ::oTable )
      RETURN 'alert("oTable not assigned");'
   ENDIF

   IF !Empty( ::cTitle )
      HB_HSet( hHash, "title", ::cTitle )
   ENDIF

   IF !Empty( ::cSubTitle )
      HB_HSet( hHash, "subTitle", ::cSubTitle )
   ENDIF

   IF !Empty( ::cLogo )
      HB_HSet( hHash, "logo", ::cLogo )
   ENDIF

   IF !Empty( ::cFilename )
      HB_HSet( hHash, "filename", ::cFilename )
   ENDIF

   HB_HSet( hHash, "html", "#" + ::oTable:ValidId() )

   IF ::lUseCss
      HB_HSet( hHash, "useCss", .T. )
   ENDIF

   IF ::nStartY != 0
      HB_HSet( hHash, "startY", ::nStart )
   ENDIF

   IF ::nMargin != 40
      HB_HSet( hHash, "margin", ::nStart )
   ENDIF

   IF !Empty( ::cPageBreak )
      HB_HSet( hHash, "pageBreak", ::cPageBreak )
   ENDIF

   IF !Empty( ::cRowPageBreak )
      HB_HSet( hHash, "rowPageBreak", ::cRowPageBreak )
   ENDIF

   IF !Empty( ::cTableWidth )
      HB_HSet( hHash, "tableWidth", ::cTableWidth )
   ENDIF

   IF !Empty( ::cShowHead )
      HB_HSet( hHash, "showHead", ::cShowHead )
   ENDIF

   IF !Empty( ::cShowFoot )
      HB_HSet( hHash, "showFoot", ::cShowFoot )
   ENDIF

   IF ::nTableLineWidth != 0
      HB_HSet( hHash, "tableLineWidth", ::nTableLineWidth )
   ENDIF

   IF ::nTableLineColor != 200
      HB_HSet( hHash, "tableLineColor", ::nTableLineColor )
   ENDIF

   IF ::lHorizontalPageBreak
      HB_HSet( hHash, "horizontalPageBreak", .T. )
   ENDIF

   IF !Empty( ::cHorizontalPageBreakBehaviour )
      HB_HSet( hHash, "horizontalPageBreakBehaviour", ::cHorizontalPageBreakBehaviour )
   ENDIF

   IF !Empty( ::cHorizontalPageBreakRepeat )
      HB_HSet( hHash, "horizontalPageBreakRepeat", ::cHorizontalPageBreakRepeat )
   ENDIF

   FOR EACH oStyle IN aStyles
      IF oStyle != NIL .AND. oStyle:IsStyle()
         HB_HSet( hHash, oStyle:cName, oStyle:hStyle )
      ENDIF
   NEXT

RETURN 'xa_pdfAutoTable(' + HB_JsonEncode( hHash ) + ');'

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZPdfAutoTableStyle FROM WPdfStyle
PUBLISHED:
   DATA Font            VALUES "helvetica", "times", "courier"
   DATA FontStyle       VALUES "normal","bold","italic","bolditalic"
   DATA Overflow        VALUES "linebreak", "ellipsize", "visible", "hidden"
   DATA Fillcolor       VALUES "*"
   DATA TextColor       VALUES "*"
   DATA CellWidth       VALUES "auto", "wrap", "*"
   DATA MinCellWidth    VALUES "*"
   DATA MinCellHeight   VALUES "*"
   DATA Halign          VALUES "left", "center", "right"
   DATA Valign          VALUES "top", "middle", "bottom"
   DATA FontSize        VALUES "*"
   DATA CellPadding     VALUES "*"
   DATA LineColor       VALUES "*"
   DATA LineWidth       VALUES "*"

ENDCLASS

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

CLASS ZPdfStyle FROM WBasicStyle
RESERVED:
   DATA hStyle    INIT {=>}
   DATA cName     INIT ""

   METHOD New( cName )              CONSTRUCTOR
   METHOD SetControl( oControl )    INLINE ::oActiveControl := Self
   METHOD SetStyle( cKey, cValue )
   METHOD IsStyle()                 INLINE !Empty( ::hStyle )

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( cName ) CLASS ZPdfStyle

   ::cName := cName
   ::oActiveControl := Self // always, do not call ::super()

   HB_HAutoAdd( ::hStyle, .T. )
   HB_HCaseMatch( ::hStyle, .F. )

RETURN Self

//------------------------------------------------------------------------------

METHOD SetStyle( cKey, cValue ) CLASS ZPdfStyle

   LowerFirst( @cKey )

   IF !Empty( cValue )
      HB_HSet( ::hStyle, cKey , cValue)
   ELSE
      HB_HDel( ::hStyle, cKey )
   ENDIF

RETURN NIL

//------------------------------------------------------------------------------
