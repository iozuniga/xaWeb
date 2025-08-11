/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeBreadcrumb.prg
 * Descripción: class for Materialize Breadcrumb
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

ANNOUNCE ZMatBreadcrumb

CLASS ZBreadcrumb FROM WLink
RESERVED:
   METHOD PreProcess()

ENDCLASS

//------------------------------------------------------------------------------

METHOD PreProcess() CLASS ZBreadcrumb

   ::AddClass( "breadcrumb" )

RETURN ::Super:PreProcess()

//------------------------------------------------------------------------------
