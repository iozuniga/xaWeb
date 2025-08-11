/*
   https://beholdr.github.io/maska/
   https://github.com/beholdr/maska

   Default maska tokens:
    '#': Digits
    '@': Letters (No international chars)
    '*': Letters & digits (No international chars)

   Extra xaWeb tokens CA-Clipper alike (one token == one input char):
    'A': Only letters
    'N': Letters & digits
    'D': Only digits
    'U': Letters & digits & transform to upper case

   Extra xaWeb tokens (not CA-Clipper alike);
    '0': Numbers multiple
    '9': Numbers optional
    'B': Letters & digits multiple
    'V': Letters & digits & transform to upper case & multiple
   Examples:
    - "0.99" Numeric any length, two digits after decimal point
    - "#99.#99.#99.#99" IP address
    - "B B" Two words
    - "V V V" Three words convert to upper case

   Extra xaWeb picture function for <<strictly>> numeric values:
    - '!#ll:dd:p":
       > 'll' is the locale setting for the format. By default InputMask():cLocale,
       > 'dd' is the fraction for decimal portion (number),
       > 'p' (number) if greater than zero, Only admits positive values.

       When this picture funcion is used the rest of the mask is useless
       Note: If you use a locale setting that uses '.' as thousand separator,
             the decimal point must be set with the ',' char.
*/


#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   METHOD CreateDoc()
   METHOD Frm_MyForm( hPost )

END CLASS

//------------------------------------------------------------------------------

METHOD Frm_MyForm( hPost ) CLASS WDocMain

   LOCAL oSection
   LOCAL cKey, cValue

   oSection := Document:AddSection( "results" )

   WITH OBJECT WDiv():New( oSection )
      ECHO "<h1>xaWeb - Forms</h1>"
      ECHO "<p>This are the inputs from your form:</p>"
      FOR EACH cKey, cValue IN HB_HKeys( hPost ), HB_HValues( hPost )
         ECHO cKey + ": " + cValue + "<br>"
      NEXT
   END WITH

RETURN oSection
//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

   LOCAL oForm, oGroup
   LOCAL cCode, cJs, cCss

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   TEXT INTO cCss
      @media screen and (width > 600px) {
         form {
            width: 600px;
         }
      }
   ENDTEXT

   ::AddCSS( cCss )

   InputMask():cLocale := "es"

   WITH OBJECT oForm := WForm():New( Document )
      :cMethod := "post"
      :cName := "Frm_myform"
      :lAutoComplete := .T.
      :Create()
      WITH OBJECT oGroup := WFieldset():New( oForm )
         :cLegend := "Input mask sample"
         :cName := "User data"
         :Create()
         WITH OBJECT WEdit():New( oGroup )
            :cId := "userid"
            :cLabel := "User ID:"
            :cPlaceHolder := "user id (five digits)"
            :nMaxLength := 5
            :nMinLength := 2
            :lLabelNewLine := .T.
            :cPicture := "#####"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :cId := "username"
            :cPlaceHolder := "User name (two words)"
            :cLabel := "User name:"
            :nMaxLength := 50
            :lLabelNewLine := .T.
            :cPicture := "B B"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :cId := "alias"
            :cName := "alias"
            :cPlaceHolder := "Alias (upper case)"
            :cLabel := "Alias:"
            :nMaxLength := 10
            :lLabelNewLine := .T.
            :cPicture := "UUUUUUUUUU"
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :cId := "salary"
            :cName := "salary"
            :cPlaceHolder := "Salary (spanish format)"
            :cLabel := "Salary:"
            :nMaxLength := 20
            :lLabelNewLine := .T.
            :cPicture := "!#es:2:1"
            :Create()
         END WITH
      END WITH
      WITH OBJECT WButton():New( oForm )
         :cType := "submit"
         :cText := "Submit"
         :cId := "button"
         :Create()
      END WITH
   END WITH

   TEXT INTO cJs
      function showCode() {
         const ele = document.getElementById("source");
         if (ele) {
            ele.value = xa_b64toUnicode(ele.value);
            ele.hidden = false;
            document.getElementById("btnsource").style.display = "none";
         }
      }
   ENDTEXT

   ::AddScript( cJs )

   ECHO "<hr>" INTO Self

   FILE "DocMain.prg" INTO cCode

   WITH OBJECT WButton():New( Self )
      :cText := "This button shows xaWeb source code"
      :Onclick := "showCode"
      :cId := "btnsource"
      :Create()
   END WITH

   WITH OBJECT WTextArea():New( Self )
      :oStyle:Margin_top := "20px"
      :oStyle:Font_Family := "monospace"
      :nCols := 90
      :cId   := "source"
      :nRows := 40
      :cText := HB_Base64Encode( cCode )
      :lReadOnly := .t.
      :lVisible := .f.
      :Create()
   END WITH

RETURN nil

//------------------------------------------------------------------------------

