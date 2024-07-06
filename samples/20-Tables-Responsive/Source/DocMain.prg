#include "XailerWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL aData, aHeader
   LOCAL cCode, cJs, cCss

   WSimpleContext():New( Self )

   TEXT INTO cJs
      ta = document.getElementById("source");
      if (ta) {
         ta.value = xw_b64toUnicode(ta.value);
      }
      delete ta;
   ENDTEXT

   ::oBody:AddStyle( "grid-template-columns: 1fr min(145rem, 90%) 1fr;" )

   ::AddScript( cJs )

   Customers( @aData, @aHeader, 4  )

   ECHO "<h2>XailerWeb - Responsive table</h2>" INTO Self

   WITH OBJECT WTable():New( Self )
      :cId := "table1"
      :oStyle:cWidth := "100%"
      :cMaxHeight := "600px"
      :cHeaderBkColor := ::oContext:BorderColor()
      :LoadData( aHeader )
      :LoadData( aData )
      :nHeader := 1
      :lResponsive := .T.
   END WITH

   FILE "DocMain.prg" INTO cCode

   ECHO "<hr>" INTO Self

   WITH OBJECT WTextArea():New( Self )
      :nCols := 100
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------


STATIC FUNCTION Customers( aData, aHeader, nCols )

   LOCAL cData, hData, hRec, aRec, xValue

TEXT INTO cData
{
  "CUSTOMER":
  [
    {
      "First": "ROBIN",
      "Last": "LAFORTUNE",
      "Street": "23974 NORTH REDGUM",
      "City": "CHOCTAW",
      "Npago": 1,
      "Cpago": "1",
      "State": "ME",
      "Zip": "54532-2170",
      "Hiredate": "19070417",
      "Married": true,
      "Age": 98,
      "Salary": 25500,
      "Notes": "THIS IS A TEST FOR RECORD 5"
    },
    {
      "First": "TARTAGLIA",
      "Last": "DODSON",
      "Street": "18883 SOUTHWEST 12TH AVENUE",
      "City": "DELTA",
      "Npago": 1,
      "Cpago": "1",
      "State": "CT",
      "Zip": "37018-5716",
      "Hiredate": "19910515",
      "Married": true,
      "Age": 67,
      "Salary": 66800,
      "Notes": "THIS IS A TEST FOR RECORD 459"
    },
    {
      "First": "PETER",
      "Last": "FRITZ",
      "Street": "13645 GROSVENOR HALL",
      "City": "MESA",
      "Npago": 1,
      "Cpago": "1",
      "State": "OH",
      "Zip": "35110-0233",
      "Hiredate": "19870722",
      "Married": false,
      "Age": 25,
      "Salary": 95000,
      "Notes": "THIS IS A TEST FOR RECORD 460"
    },
    {
      "First": "FRED",
      "Last": "HILTON",
      "Street": "3196 SOUTH BLACKHAWK AVE",
      "City": "CHATTANOOGA",
      "Npago": 1,
      "Cpago": "1",
      "State": "MD",
      "Zip": "56692-7523",
      "Hiredate": "19910904",
      "Married": true,
      "Age": 73,
      "Salary": 20000,
      "Notes": "THIS IS A TEST FOR RECORD 461"
    },
    {
      "First": "MAJOLA",
      "Last": "MONTANARI",
      "Street": "20481 GRAND AVENUE",
      "City": "FOUNTAIN VALLEY",
      "Npago": 1,
      "Cpago": "1",
      "State": "AK",
      "Zip": "33499-8757",
      "Hiredate": "19920830",
      "Married": false,
      "Age": 34,
      "Salary": 144300,
      "Notes": "THIS IS A TEST FOR RECORD 462"
    },
    {
      "First": "ARNALDO",
      "Last": "CARNEY",
      "Street": "1692 BUFFALO AVE",
      "City": "COLUMBUS",
      "Npago": 1,
      "Cpago": "1",
      "State": "IN",
      "Zip": "71128-3146",
      "Hiredate": "19870514",
      "Married": true,
      "Age": 61,
      "Salary": 83900,
      "Notes": "THIS IS A TEST FOR RECORD 463"
    },
    {
      "First": "MARGURETHA",
      "Last": "WOODWARD",
      "Street": "29099 BOX 303",
      "City": "NORTH ANDOVER",
      "Npago": 1,
      "Cpago": "1",
      "State": "SC",
      "Zip": "40888-3619",
      "Hiredate": "19840530",
      "Married": false,
      "Age": 87,
      "Salary": 139600,
      "Notes": "THIS IS A TEST FOR RECORD 464"
    },
    {
      "First": "LOU",
      "Last": "STUBBS",
      "Street": "23232 BAMBRIDGE STREET",
      "City": "OTTSVILLE",
      "Npago": 1,
      "Cpago": "1",
      "State": "CO",
      "Zip": "88399-5974",
      "Hiredate": "19830130",
      "Married": true,
      "Age": 57,
      "Salary": 40800,
      "Notes": "THIS IS A TEST FOR RECORD 465"
    },
    {
      "First": "DAVE",
      "Last": "GOTSMAN",
      "Street": "17960 VANDEVEN COURT",
      "City": "LAVERNE",
      "Npago": 1,
      "Cpago": "1",
      "State": "KS",
      "Zip": "68932-3990",
      "Hiredate": "19890409",
      "Married": false,
      "Age": 92,
      "Salary": 107500,
      "Notes": "THIS IS A TEST FOR RECORD 466"
    },
    {
      "First": "OVED",
      "Last": "IRWIN",
      "Street": "22152 AVON PLACE",
      "City": "SUNNYVALE",
      "Npago": 1,
      "Cpago": "1",
      "State": "UT",
      "Zip": "11062-9596",
      "Hiredate": "19901216",
      "Married": true,
      "Age": 47,
      "Salary": 37200,
      "Notes": "THIS IS A TEST FOR RECORD 467"
    },
    {
      "First": "BOHDAN",
      "Last": "SAAD",
      "Street": "22528 BEAUREGARD STREET",
      "City": "SPRINGFIELD",
      "Npago": 1,
      "Cpago": "1",
      "State": "NV",
      "Zip": "53767-6842",
      "Hiredate": "19900618",
      "Married": false,
      "Age": 84,
      "Salary": 106700,
      "Notes": "THIS IS A TEST FOR RECORD 468"
    },
    {
      "First": "ALI",
      "Last": "HAAN",
      "Street": "19830 INDUSTRIAL DRIVE",
      "City": "SAN FRANCISCO",
      "Npago": 1,
      "Cpago": "1",
      "State": "AZ",
      "Zip": "24436-7713",
      "Hiredate": "19880217",
      "Married": true,
      "Age": 44,
      "Salary": 55800,
      "Notes": "THIS IS A TEST FOR RECORD 469"
    },
    {
      "First": "LAM",
      "Last": "BICKLEY",
      "Street": "2283 PIONCIANA",
      "City": "MONTVILLE",
      "Npago": 1,
      "Cpago": "1",
      "State": "WY",
      "Zip": "34372-6809",
      "Hiredate": "19850310",
      "Married": false,
      "Age": 38,
      "Salary": 29200,
      "Notes": "THIS IS A TEST FOR RECORD 470"
    },
    {
      "First": "VINCENT",
      "Last": "BECKERMAN",
      "Street": "24596 EXCHANGE PLACE",
      "City": "TORONTO",
      "Npago": 1,
      "Cpago": "1",
      "State": "KY",
      "Zip": "41752-0710",
      "Hiredate": "19921109",
      "Married": true,
      "Age": 67,
      "Salary": 2500,
      "Notes": "THIS IS A TEST FOR RECORD 471"
    },
    {
      "First": "LENORE",
      "Last": "JOCHUM",
      "Street": "143 CANANDAIGUA ROAD",
      "City": "MONTGOMERY",
      "Npago": 1,
      "Cpago": "1",
      "State": "LA",
      "Zip": "13717-0205",
      "Hiredate": "19920828",
      "Married": false,
      "Age": 46,
      "Salary": 2400,
      "Notes": "THIS IS A TEST FOR RECORD 472"
    },
    {
      "First": "GUNTHER",
      "Last": "BLAKE",
      "Street": "21339 CLARKS HILL",
      "City": "LAKELAND",
      "Npago": 1,
      "Cpago": "1",
      "State": "SD",
      "Zip": "21682-8593",
      "Hiredate": "19870421",
      "Married": true,
      "Age": 86,
      "Salary": 26500,
      "Notes": "THIS IS A TEST FOR RECORD 473"
    },
    {
      "First": "ARTHUR",
      "Last": "QUINN",
      "Street": "21900 BOXWOOD LANE",
      "City": "LAFAYETTE",
      "Npago": 1,
      "Cpago": "1",
      "State": "CT",
      "Zip": "12070-9416",
      "Hiredate": "19860329",
      "Married": false,
      "Age": 75,
      "Salary": 55600,
      "Notes": "THIS IS A TEST FOR RECORD 474"
    },
    {
      "First": "LUCIAN",
      "Last": "KELLEY",
      "Street": "23633 SW MARTIN DOWNS BLVD",
      "City": "FORT MILL",
      "Npago": 1,
      "Cpago": "1",
      "State": "WV",
      "Zip": "62074-2989",
      "Hiredate": "19830531",
      "Married": true,
      "Age": 92,
      "Salary": 15400,
      "Notes": "THIS IS A TEST FOR RECORD 475"
    },
    {
      "First": "BECKY",
      "Last": "COLEMAN",
      "Street": "29884 NORTH WEST 66TH ST",
      "City": "ANN ARBOR",
      "Npago": 1,
      "Cpago": "1",
      "State": "ME",
      "Zip": "85024-5187",
      "Hiredate": "19901218",
      "Married": false,
      "Age": 71,
      "Salary": 46100,
      "Notes": "THIS IS A TEST FOR RECORD 476"
    },
    {
      "First": "REG",
      "Last": "HONG",
      "Street": "1010 MELROSE AVENUE",
      "City": "LISLE",
      "Npago": 1,
      "Cpago": "1",
      "State": "OH",
      "Zip": "66673-1051",
      "Hiredate": "19880327",
      "Married": true,
      "Age": 44,
      "Salary": 101100,
      "Notes": "THIS IS A TEST FOR RECORD 477"
    },
    {
      "First": "LANCE",
      "Last": "THOMSON",
      "Street": "2740 LAWRENCE AVE WEST",
      "City": "VACAVILLE",
      "Npago": 1,
      "Cpago": "1",
      "State": "WI",
      "Zip": "05599-9144",
      "Hiredate": "19891207",
      "Married": false,
      "Age": 26,
      "Salary": 108900,
      "Notes": "THIS IS A TEST FOR RECORD 478"
    },
    {
      "First": "DARRELL",
      "Last": "HUSSAIN",
      "Street": "1875 WHIPPLE AVE NW",
      "City": "BELTSVILLE",
      "Npago": 1,
      "Cpago": "1",
      "State": "MI",
      "Zip": "76780-5302",
      "Hiredate": "19831215",
      "Married": true,
      "Age": 66,
      "Salary": 94100,
      "Notes": "THIS IS A TEST FOR RECORD 479"
    },
    {
      "First": "WESLEY",
      "Last": "GOTSMAL",
      "Street": "29583 E LINCOLN",
      "City": "JERSEY CITY",
      "Npago": 1,
      "Cpago": "1",
      "State": "AL",
      "Zip": "18415-9523",
      "Hiredate": "19860716",
      "Married": false,
      "Age": 67,
      "Salary": 116800,
      "Notes": "THIS IS A TEST FOR RECORD 480"
    },
    {
      "First": "FRANK",
      "Last": "DEARRY",
      "Street": "22530 FOREST DRIVE",
      "City": "WOODBRIDGE",
      "Npago": 1,
      "Cpago": "1",
      "State": "MO",
      "Zip": "20683-7667",
      "Hiredate": "19841231",
      "Married": true,
      "Age": 91,
      "Salary": 59900,
      "Notes": "THIS IS A TEST FOR RECORD 481"
    },
    {
      "First": "PHILIPPE",
      "Last": "PEZZANI",
      "Street": "29933 LAURIE LANE",
      "City": "BALTIMORE",
      "Npago": 1,
      "Cpago": "1",
      "State": "AR",
      "Zip": "70417-7019",
      "Hiredate": "19840913",
      "Married": false,
      "Age": 69,
      "Salary": 106300,
      "Notes": "THIS IS A TEST FOR RECORD 482"
    },
    {
      "First": "RANDALL",
      "Last": "BREMAN",
      "Street": "8369 SOKOLOV STREET",
      "City": "SOUTH ELGIN",
      "Npago": 1,
      "Cpago": "1",
      "State": "PA",
      "Zip": "13339-4503",
      "Hiredate": "19860619",
      "Married": true,
      "Age": 80,
      "Salary": 44100,
      "Notes": "THIS IS A TEST FOR RECORD 483"
    },
    {
      "First": "TROY",
      "Last": "CAIN",
      "Street": "17976 UNIVERSITY AVENUE",
      "City": "SPRINGFIELD",
      "Npago": 1,
      "Cpago": "1",
      "State": "NV",
      "Zip": "05950-4191",
      "Hiredate": "19840623",
      "Married": false,
      "Age": 99,
      "Salary": 120300,
      "Notes": "THIS IS A TEST FOR RECORD 484"
    },
    {
      "First": "BERNHARD",
      "Last": "HALEY",
      "Street": "29203 CARMENITA ROAD",
      "City": "TALLAHASSE",
      "Npago": 1,
      "Cpago": "1",
      "State": "TX",
      "Zip": "72811-9912",
      "Hiredate": "19920208",
      "Married": true,
      "Age": 42,
      "Salary": 38200,
      "Notes": "THIS IS A TEST FOR RECORD 485"
    },
    {
      "First": "BRUNO",
      "Last": "WILSON",
      "Street": "11047 KNOLLSIDE LANE",
      "City": "VICTORIA",
      "Npago": 1,
      "Cpago": "1",
      "State": "AZ",
      "Zip": "84637-2588",
      "Hiredate": "19830625",
      "Married": false,
      "Age": 67,
      "Salary": 102900,
      "Notes": "THIS IS A TEST FOR RECORD 486"
    },
    {
      "First": "SUZANNE",
      "Last": "GALVEZ",
      "Street": "31751 FOCHT AVENUE",
      "City": "LAKELAND",
      "Npago": 1,
      "Cpago": "1",
      "State": "KY",
      "Zip": "82486-7296",
      "Hiredate": "19910504",
      "Married": true,
      "Age": 78,
      "Salary": 36700,
      "Notes": "THIS IS A TEST FOR RECORD 487"
    },
    {
      "First": "NICK",
      "Last": "JONES",
      "Street": "9349 BELLMORE",
      "City": "ALLENTOWN",
      "Npago": 1,
      "Cpago": "1",
      "State": "LA",
      "Zip": "11800-9446",
      "Hiredate": "19890608",
      "Married": false,
      "Age": 53,
      "Salary": 48200,
      "Notes": "THIS IS A TEST FOR RECORD 488"
    },
    {
      "First": "TREVOR",
      "Last": "RUSSEL",
      "Street": "30453 CAROLINA AVENUE",
      "City": "STEVENS POINT",
      "Npago": 1,
      "Cpago": "1",
      "State": "ID",
      "Zip": "48628-9897",
      "Hiredate": "19890218",
      "Married": true,
      "Age": 81,
      "Salary": 64600,
      "Notes": "THIS IS A TEST FOR RECORD 489"
    },
    {
      "First": "BEHZAD",
      "Last": "STOOPS",
      "Street": "31540 HUNNEWELL STREET",
      "City": "GLENDALE",
      "Npago": 1,
      "Cpago": "1",
      "State": "CT",
      "Zip": "68581-0672",
      "Hiredate": "19911216",
      "Married": false,
      "Age": 72,
      "Salary": 106500,
      "Notes": "THIS IS A TEST FOR RECORD 490"
    },
    {
      "First": "CLESSON",
      "Last": "FISHER",
      "Street": "8567 SHERMAN WAY",
      "City": "LAVERNE",
      "Npago": 1,
      "Cpago": "1",
      "State": "ME",
      "Zip": "78994-0192",
      "Hiredate": "19840316",
      "Married": true,
      "Age": 86,
      "Salary": 120800,
      "Notes": "THIS IS A TEST FOR RECORD 491"
    },
    {
      "First": "LENORE",
      "Last": "COTTERILL",
      "Street": "10605 SOUTH UNIVERSITY",
      "City": "ORLAND PARK",
      "Npago": 1,
      "Cpago": "1",
      "State": "MD",
      "Zip": "36496-4469",
      "Hiredate": "19880127",
      "Married": false,
      "Age": 81,
      "Salary": 98800,
      "Notes": "THIS IS A TEST FOR RECORD 492"
    },
    {
      "First": "BYRON",
      "Last": "DOWD",
      "Street": "30266 RENE LEVESQUE WEST",
      "City": "FORT COLLINS",
      "Npago": 1,
      "Cpago": "1",
      "State": "HI",
      "Zip": "47170-1769",
      "Hiredate": "19880405",
      "Married": true,
      "Age": 78,
      "Salary": 32600,
      "Notes": "THIS IS A TEST FOR RECORD 493"
    },
    {
      "First": "DAVID",
      "Last": "ZEAL",
      "Street": "26187 COHEN STREET",
      "City": "CHATTANOOGA",
      "Npago": 1,
      "Cpago": "1",
      "State": "AK",
      "Zip": "29287-2701",
      "Hiredate": "19830503",
      "Married": false,
      "Age": 70,
      "Salary": 3500,
      "Notes": "THIS IS A TEST FOR RECORD 494"
    },
    {
      "First": "JACKIE",
      "Last": "BECKERMAN",
      "Street": "32292 DEVONSHIRA DRIVE",
      "City": "ORANGE",
      "Npago": 1,
      "Cpago": "1",
      "State": "OK",
      "Zip": "21025-6797",
      "Hiredate": "19870209",
      "Married": true,
      "Age": 93,
      "Salary": 133300,
      "Notes": "THIS IS A TEST FOR RECORD 495"
    }
  ]
}
ENDTEXT

   DEFAULT nCols TO 0

   aHeader := {}
   aData   := {}

   HB_JsonDecode( cData, @hData )

   IF Len( hData[ 'CUSTOMER' ] ) > 0
      aRec := {}
      FOR EACH xValue IN HB_HKeys( hData[ 'CUSTOMER' ][ 1 ] )
         IF nCols == 0 .OR. Len( aRec ) < nCols
            AAdd( aRec, xValue )
         ENDIF
      NEXT
      AAdd( aHeader, aRec )
   ENDIF

   FOR EACH hRec IN hData[ 'CUSTOMER' ]
      aRec := {}
      FOR EACH xValue IN hRec
         IF nCols == 0 .OR. Len( aRec ) < nCols
            AAdd( aRec, xValue )
         ENDIF
      NEXT
      AAdd( aData, aRec )
   NEXT

RETURN NIL
