/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeButton.prg
 * Descripción: class for Materialize Buttons
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatButton.
 *       This is done automatically when using the xa-materialize.ch file.
 */

#include "xaWeb.ch"

ANNOUNCE ZMatButton

CLASS ZButton FROM WLink
PUBLISHED:
   DATA cType        INIT "button" VALUES "button", "reset", "submit", "cancel"
   DATA cDisplayType INIT "" VALUES "", "filled", "tonal", "outlined", "text", "floating"
   DATA cWaveEffect  INIT "" VALUES "", "normal", "light"
   DATA cForm        INIT ""
   DATA cText        INIT ""
   DATA cIcon        INIT ""
   DATA cColor       INIT ""
   DATA cIconAlign   INIT "" VALUES "", "left", "right"
   DATA lRounded     INIT .F.
   DATA lDisabled    INIT .F.
   DATA llarge       INIT .F.
   DATA lSmall       INIT .F.
   DATA lPulse       INIT .F.
   DATA lElevated    INIT .F.
   DATA oIcon        AS CLASS WIconGoogle

RESERVED:
   METHOD New( oParent, oControl, lAuto ) CONSTRUCTOR
   METHOD End()      INLINE ( ::oIcon := NIL, ::Super:End() )
   METHOD PreProcess()
   METHOD HtmlTagBody()

PROTECTED:

ENDCLASS

//------------------------------------------------------------------------------

METHOD New( oParent, oControl, lAuto ) CLASS ZButton

   ::Super:New( oParent, oControl, lAuto )
   ::oIcon := WIconGoogle():New( Self, Self, .T. )

RETURN Self

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZButton

   LOCAL oForm, oSection
   LOCAL cClass

   cClass := "btn"

   IF !Empty( ::cDisplayType )
      IF ::cDisplayType == "floating"
         cClass += "-floating"
      ELSE
         cClass += " " + ::cDisplayType
      ENDIF
   ENDIF

   IF !Empty( ::cIcon )
      ::oIcon:cText := ::cIcon
      IF ::cIconAlign == "left"
         cClass += " icon-left"
         IF !Empty( ::cText )
            ::oIcon:oStyle:Margin_right := "8px"
         ENDIF
      ELSEIF ::cIconAlign == "right"
         cClass += " icon-right"
         IF !Empty( ::cText )
            ::oIcon:oStyle:Margin_left := "8px"
         ENDIF
      ENDIF
   ELSE
      ::RemoveControl( ::oIcon )
      ::oIcon := NIL
   ENDIF

   IF !Empty( ::cWaveEffect )
      cClass += " waves-effect"
      IF ::cWaveEffect == "light"
         cClass += " waves-light"
      ENDIF
   ENDIF

   IF ::lRounded
      cClass += " rounded"
   ENDIF

   IF ::lDisabled
      cClass += " disabled"
   ENDIF

   IF ::lElevated
      cClass += " elevated"
   ENDIF

   IF ::lLarge
      cClass += " large"
   ENDIF

   IF ::lSmall
      cClass += " small"
   ENDIF

   IF !Empty( ::cColor )
      cClass += " " + ::cColor
   ENDIF

   IF ::lPulse
      cClass += " pulse"
   ENDIF

   ::AddClass( cClass )

   SWITCH ::cType
   CASE "submit"
      ::cTag := "button"
      EXIT
   CASE "cancel"
      //::cTag := "button"
      IF !::IsEvent( "onclick" )
         IF !Empty( ::cForm )
            oForm := Document:SearchControl( ::cForm )
         ELSE
            oForm := ::GetForm()
         ENDIF
         IF HB_IsObject( oForm )
            oSection := oForm:GetSection()
            IF HB_IsObject( oSection ) .AND. oForm:lFetch
               ::OnClick := '<script>xa_showSection("' + oSection:cId + '", false, "' + oForm:cId + '");</script>'
            ELSE
               ::OnClick := '<script>history.back();</script>'
            ENDIF
         ENDIF
      ENDIF
      EXIT
   CASE "reset"
      //::cTag := "button"
      IF !::IsEvent( "onclick" )
         IF !Empty( ::cForm )
            oForm := Document:SearchControl( ::cForm )
         ELSE
            oForm := ::GetForm()
            ::cForm := oForm:cId
         ENDIF
         IF HB_IsObject( oForm )
            ::OnClick := '<script>document.getElementById("' + ::cForm + '").reset();</script>'
         ENDIF
      ENDIF
      EXIT
   END SWITCH

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

METHOD HtmlTagBody() CLASS ZButton

   LOCAL cHtml := ""

   SWITCH::cType
   CASE "submit"
      cHtml += ' type="submit"'
      EXIT
   CASE "reset"
   //   cHtml += ' type="reset"'
      EXIT
   END SWITCH

   cHtml += ::Super:HtmlTagBody()

RETURN cHtml

//------------------------------------------------------------------------------

