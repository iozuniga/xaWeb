/*
 * Proyecto: xaWeb framework
 * Fichero: ZDump.prg
 * Descripción: HTML Dump class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZDump FROM ZBasic
PUBLISHED:
   DATA cInnerHTML   INIT ""

RESERVED:
   METHOD RunHtml()

PROTECTED:
   DATA cText  INIT ""
   DATA cTag   INIT "div"

ENDCLASS

//------------------------------------------------------------------------------

METHOD RunHtml() CLASS ZDump

   LOCAL cHtml := ::cInnerHTML

   ::OnDeploy( @cHtml )

RETURN cHtml

//------------------------------------------------------------------------------
