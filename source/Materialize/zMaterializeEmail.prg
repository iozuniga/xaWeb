/*
 * Proyecto: xaWeb framework
 * Fichero: ZMaterializeEmail.prg
 * Descripción: class for Materialize input-email
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
  * Note: This class overrides a xaWeb class with the same name. Its important
 *       that the Materialize library is linked before de xaWeb library and
 *       at least one module of the user App contains a REQUEST to ZMatEmail.
 *       This is done automatically when using the xa-materialize.ch file.
 */

#include "xaWeb.ch"

ANNOUNCE ZMatEmail

CLASS ZEmail FROM WBasicEmail
ENDCLASS
