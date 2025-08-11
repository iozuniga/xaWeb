/*
 * Proyecto: xaWeb framework
 * Fichero: zMaterializePreloader.prg
 * Descripción: class for Materialize preloader
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatPreloader

CLASS ZPreloader FROM WDiv
PUBLISHED:
   DATA cSize        INIT "" VALUES "", "big", "small"
   DATA cColor       INIT ""
   DATA lCircular    INIT .F.
   DATA lDeterminate INIT .F.
   DATA lFlash       INIT .F.
   DATA nValue       INIT 0

RESERVED:
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZPreloader

   LOCAL cClass

   IF !::lCircular
      ::AddClass( "progress" )
      WITH OBJECT WDiv():New( Self )
         IF ::lDeterminate
            :AddClass( "determinate" )
            IF ::nValue > 0
               :AddStyle( "width: " + HB_ValToStr( ::nValue ) + "%" )
            ENDIF
         ELSE
            :AddClass( "indeterminate" )
         ENDIF
         IF !empty( ::cColor )
            :AddClass( ::cColor )
         ENDIF
      END WITH
   ELSE
      ::AddClass( "preloader-wrapper active" )
      IF !Empty( ::cSize )
         ::AddClass( ::cSize )
      ENDIF
      IF !::lFlash
         WITH OBJECT WDiv():New( Self )
            AddCircle( Self, ::cColor, .F. )
         END WITH
      ELSE
         WITH OBJECT WDiv():New( Self )
            AddCircle( Self, "blue", .T. )
         END WITH
         WITH OBJECT WDiv():New( Self )
            AddCircle( Self, "red", .T. )
         END WITH
         WITH OBJECT WDiv():New( Self )
            AddCircle( Self, "yellow", .T. )
         END WITH
         WITH OBJECT WDiv():New( Self )
            AddCircle( Self, "green", .T. )
         END WITH
      ENDIF
   ENDIF

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------

STATIC FUNCTION AddCircle( oParent, cColor, lFlash )

   LOCAL oDiv

   WITH OBJECT WDiv():New( oParent )
      :AddClass( "spinner-layer" )
      IF !Empty( cColor )
         :AddClass( "spinner-" + cColor + IIF( lFlash, "", "-only" ) )
      ENDIF
      WITH OBJECT WDiv():New( SO, oParent )
         :AddClass( "circle-clipper left" )
         WITH OBJECT WDiv():New( SO, oParent )
            :AddClass( "circle" )
         END WITH
      END WITH
      WITH OBJECT WDiv():New( SO, oParent )
         :AddClass( "gap-patch" )
         WITH OBJECT WDiv():New( SO, oParent )
            :AddClass( "circle" )
         END WITH
      END WITH
      WITH OBJECT WDiv():New( SO, oParent )
         :AddClass( "circle-clipper right" )
         WITH OBJECT WDiv():New( SO )
            :AddClass( "circle" )
         END WITH
      END WITH
   END WITH

RETURN oDiv

//------------------------------------------------------------------------------

