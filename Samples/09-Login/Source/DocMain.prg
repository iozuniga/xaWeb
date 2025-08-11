/*
Nota importante:

Este ejemplo pretende demostrar la <<forma más sencilla>> de tener DATAs a nivel
de documento de forma persistente.

El sistema de persistencia es importante entenderlo: Todas las propiedades
tipo PERSISTENT son guardadas en archivo JSON dentro de cada sesión de usuario,
y el momento exacto en el que se guardan es cuando se hace el despliegue final
del CGI y se recuperan en el mismo momento en el que se instancia el objeto
WDoc. Por dicho motivo hay que tener en cuenta las siguientes limitaciones:

1) Sólo puede establecer el valor inicial de una DATA persistent en la propia
   definición de clase. Cualquier otro sitio, provoca que se pierda el valor
   recibido de persistencia por la sesión.
2) Cuando establece el valor de una DATA tipo persistent SOLO tendrá ese nuevo
   valor, en todo el código que se ejecute a continuación y la próxima vez que
   ejecute el CGI.
3) Cuando se instanacia WDoc, lo primero que se ejecuta es su método DocStart(),
   a continuación cualquier método que haya usted indicado como servicio o
   acción y por último todos las secciones que haya registrado con el método
   WDoc:RegisterSection( <cSection> )

En este ejemplo, hemos utilizado un triquiñuela para que en el mismo método
DocStart() ya tenga la propiedad cUser actualizada y consiste en forzar una
recarga de la página con el método WDocMain:Reload( <cAction>, <cDocument> )
Este proceso es muy rápido si lo único que hace es actualizar cUser y salir.

No obsante, hay una forma más elegante de hacerlo, sin recarga, que es
utilizando el método WDoc():RegisterSection(). Este método permite establecer
secciones que se procesarán después de haberse ejecutado el método 'Action" o
'Post' que usted haya indicado. Por lo tanto, la solución es fácil: minimice al
máximo el código que ubica en el método StartDoc() y colóquelo en secciones que
registrará con el método RegisterSection(). Cuando como ocurre en este ejemplo,
actualice la data PERSISTENT en su codigo en un proceso POST (Método
Frm_SectionCheck); lo único que tendrá hacer es hacer una llamada a un método
que haya sido registrado. Si todo esto le parece muy complicado, utilice el
método WDocMain:Reload( <cAction>, <cDocument> ) que como puede ver y así se
indica en la ayuda, puede hacer que la recarga ejecute cualquier acción, incluso
de un documento distinto.
*/

#include "xaWeb.ch"

CLASS WDocMain FROM WDoc

   DATA cUser INIT "" PERSISTENT

   METHOD CreateDoc()
   METHOD DefaultSection()
   METHOD CodeSection()

   METHOD Act_LoginSection()
   METHOD Act_Section2()
   METHOD Act_Section3()

   METHOD Frm_SectionCheck( hParams )

END CLASS

//------------------------------------------------------------------------------

METHOD CreateDoc() CLASS WDocMain

//   Engine:lDebug := .t.

   WITH OBJECT WBasicContext():New( Self )
      :cTheme := "light"
   END WITH

   ::DefaultSection()
   ::RegisterSection( "CodeSection" )

RETURN nil

//------------------------------------------------------------------------------

METHOD DefaultSection() CLASS WDocMain

   LOCAL oSection
   LOCAL lLogin := .f.

   oSection := ::AddSection( "default" )

   WITH OBJECT WParagraph():New( oSection )
      ECHO "<h1>xaWeb - Simple login demo</h1>"
      ECHO "<p>This sample does not work on Xailer's IDE navigator</p>"
      IF !Empty( ::cUser )
         ECHO "<p>You are logged as user: " + ::cUser + "</p>"
         lLogin := .T.
      ELSE
         ECHO "You must be logged for section two and three.<br>"
         ECHO "Valid credentials: name: 'admin', password: '1234'"
      ENDIF
   END WITH

   WITH OBJECT WNav():New( oSection )
      :AddStyle( "display:flex; gap:0.5em; flex-wrap:wrap;" )
      WITH OBJECT WLink():New( SO )
         :cText := "Log in"
         :cHref := ::Action( "Act_LoginSection" )
      END WITH
      WITH OBJECT WLink():New( SO )
         :cText := "Second Section"
         :cHref := ::Action( "Act_Section2" )
         :lDisabled := !lLogin
      END WITH
      WITH OBJECT WLink():New( SO )
         :cText := "Third Section"
         :cHref := ::Action( "Act_Section3" )
         :lDisabled := !lLogin
      END WITH
   END WITH

   oSection:lDeploy := .T.

RETURN oSection

//------------------------------------------------------------------------------

METHOD CodeSection() CLASS WDocMain

   LOCAL oSection
   LOCAL cCode, cJs

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

   WITH OBJECT oSection := ::AddSection( "code" )
      :lDeploy := .t.
      :lFooter := .t.

      ECHO "<hr>"

      FILE "DocMain.prg" INTO cCode

      WITH OBJECT WButton():New( SO )
         :cText := "This button shows xaWeb source code"
         :Onclick := "showCode"
         :cId := "btnsource"
         :Create()
      END WITH

      WITH OBJECT WTextArea():New( SO )
         :oStyle:Margin_top := "20px"
         :oStyle:Font_Family := "monospace"
         :nCols := 80
         :cId   := "source"
         :nRows := 40
         :cText := HB_Base64Encode( cCode )
         :lReadOnly := .t.
         :lVisible := .f.
         :Create()
      END WITH
   END WITH

RETURN oSection

//------------------------------------------------------------------------------

METHOD Act_LoginSection() CLASS WDocMain

   LOCAL oSection, oForm, oGroup

   oSection := ::AddSection( "SectionLogin" )

   WParagraph():New( oSection )

   WITH OBJECT oForm := WForm():New( oSection )
      :cMethod := "post"
      :cName := "Frm_SectionCheck"
      :lAutoComplete := .T.
      :Create()
      WITH OBJECT oGroup := WFieldset():New( oForm )
         :cLegend := "Login credentials"
         :cName := "User data"
         :AddStyle( "width: 400px" )
         :Create()
         WITH OBJECT WEdit():New( oGroup )
            :cLabel := "User:"
            :lLabelNewLine := .t.
            :cId := "user"
            :cName := "user"
            :cPlaceHolder := "user name"
            :nSize := 50
            :nMaxLength := 50
            :nMinLength := 5
            :Create()
         END WITH
         WITH OBJECT WEdit():New( oGroup )
            :cLabel := "Password:"
            :lLabelNewLine := .t.
            :cType := "password"
            :cId := "password"
            :cName := "password"
            :cPlaceHolder := "password"
            :nSize := 50
            :nMaxLength := 50
            :nMinLength := 4
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

RETURN oSection

//------------------------------------------------------------------------------

METHOD Act_Section2() CLASS WDocMain

   LOCAL oSection

   WITH OBJECT oSection := ::AddSection( "Section2" )
      ECHO "<ul><h1>This is Section number two</h1></ul>"
   END WITH

RETURN oSection

//------------------------------------------------------------------------------

METHOD Act_Section3() CLASS WDocMain

   LOCAL oSection

   WITH OBJECT oSection := ::AddSection( "Section3" )
      ECHO "<ul><h1>This is Section number three</h1></ul>"
   END WITH

RETURN oSection

//------------------------------------------------------------------------------

METHOD Frm_SectionCheck( hParams ) CLASS WDocMain

   LOCAL oSection
   LOCAL cUser, cPass

   IF HB_HHasKey( hParams, "user" )
      cUser := hParams[ "user" ]
      cPass := hParams[ "password" ]
   ELSE
      WITH OBJECT oSection := ::AddSection( "checklogin" )
         ECHO "<ul>"
         ECHO "<h1>Use a true web server</h1>"
         ECHO "<p>Xailer's IDE navigator does not support META refresh operations</p>"
         ECHO "</ul>"
         RETURN nil
      END WITH
   ENDIF

   IF cUser = "admin" .AND. cPass = "1234"
      ::cUser := cUser
      Document:Reload()
   ELSE
      WITH OBJECT oSection := ::AddSection( "checklogin" )
         ECHO "<ul>"
         ECHO "<h1>Invalid credentials</h1>"
         ECHO "<p>Try again</p>"
         ECHO "</ul>"
      END WITH
   ENDIF

RETURN oSection

//------------------------------------------------------------------------------
