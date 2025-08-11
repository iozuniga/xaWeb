/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeEdit.prg
 * Descripción: class for Materialize input-edit
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
  * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatEdit.
 *       This is done automatically when using the xa-materialize.ch file.
 */

#include "xaWeb.ch"

ANNOUNCE ZMatEdit

CLASS ZEdit FROM WBasicEdit
ENDCLASS
