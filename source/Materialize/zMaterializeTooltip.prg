/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeTooltip.prg
 * Descripción: class for Materialize Tooltips
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatTooltip

STATIC lDeploy := .F.

CLASS ZTooltip FROM WControl
PUBLISHED:
   DATA cPosition    INIT "bottom" VALUES "bottom", "left", "top", "right"
   DATA cText        INIT ""
   DATA cIcon        INIT ""

   DATA nExitDelay   INIT 200
   DATA nEnterDelay  INIT 0
   DATA nMargin      INIT 5
   DATA nInDuration  INIT 300
   DATA nOutDuration INIT 250
   DATA nOpacity     INIT 1
   DATA nTransitionMovement INIT 10
   DATA oIcon        AS CLASS WIconGoogle

RESERVED:
   METHOD PreProcess()
   METHOD End() INLINE (::oIcon := NIL, ::Super:End() )

PROTECTED:
   DATA cTag      INIT "div"

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZTooltip

   LOCAL hOpt := { => }
   LOCAL cJs

   WITH OBJECT ::oParent
      :AddClass( "tooltipped" )
      :AddDataset( "position", ::cPosition )
      :AddDataset( "tooltip", ::cText )
   END WITH

   IF Empty( ::cIcon )  // No need to create new controls
      ::oParent:RemoveControl( Self )
   ELSE
      ::oParent:AddDataset( "tooltip-id", ::ValidId() )
      ::AddStyle( "display: none;" )
      ::oIcon := WIconGoogle():New( Self )
      ::oIcon:cText := ::cIcon
      ::oIcon:AddStyle( "vertical-align: bottom;" )
   ENDIF

   IF !lDeploy .OR. Document:lRender2
      lDeploy := .T.
      IF ::nExitDelay != 200
         HB_HSet( hOpt, "exitDelay", ::nExitDelay )
      ENDIF
      IF ::nEnterDelay != 0
         HB_HSet( hOpt, "enterDelay", ::nEnterDelay )
      ENDIF
      IF ::nMargin != 5
         HB_HSet( hOpt, "margin", ::nMargin )
      ENDIF
      IF ::nInDuration != 300
         HB_HSet( hOpt, "inDuration", ::ninDuration )
      ENDIF
      IF ::nOutDuration != 250
         HB_HSet( hOpt, "outDuration", ::nOutDuration )
      ENDIF
      IF ::nOpacity != 1
         HB_HSet( hOpt, "opacity", ::nOpacity )
      ENDIF
      IF ::nTransitionMovement != 10
         HB_HSet( hOpt, "transitionMovement", ::nTransitionMovement )
      ENDIF

      cJs := " M.Tooltip.init(document.querySelectorAll('.tooltipped')," + HB_JsonEncode( hOpt ) + ");" + hb_eol()
      Document:oContext:AddCodeOnLoad( cJs )
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

