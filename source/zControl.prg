/*
 * Proyecto: xaWeb framework
 * Fichero: ZControl.prg
 * Descripción: Base class for HTML controls
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 * Note: This class is internal. Should not be instanciated directly
 */

#include "xaWeb.ch"

CLASS ZControl FROM WContainer

OVERLOADED:
   DATA cText        INIT ""
   DATA cTag         INIT ""

ENDCLASS

//------------------------------------------------------------------------------



