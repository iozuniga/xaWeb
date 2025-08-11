/*
 * Proyecto: xaWeb framework
 * Fichero: zXailerMariaDbDatasource.prg
 * Descripción: MariaDb datasource with Xailer
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
 *
 * Important note on Linux installations:
 *  MariaDb-devel package must be installed on your system:
 *  -> sudo apt install libmariadb3 libmariadb-dev
 */

#include "xaWeb.ch"

CLASS ZXailerMariaDbDatasource FROM TMariaDbDatasource

RESERVED:
   DATA lAllowProcessMessages    INIT .F.

END CLASS

//------------------------------------------------------------------------------

