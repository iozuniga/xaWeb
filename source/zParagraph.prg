/*
 * Proyecto: xaWeb framework
 * Fichero: ZParagraph.prg
 * Descripción: HTML paragraph class
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 */

#include "xaWeb.ch"

CLASS ZParagraph FROM WControl
   DATA cText  INIT ""

PROTECTED:
   DATA cTag   INIT "p"
ENDCLASS

