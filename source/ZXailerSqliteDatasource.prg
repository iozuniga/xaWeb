/*
 * Proyecto: xaWeb
 * Fichero: ZXailerSqliteDatasource.prg
 * Descripción: Xailer SqliteDatasource wrapper
 * Autor: Ignacio Ortiz de Zúñiiga
 * Fecha: 19/02/2025
 */

#include "xaWeb.ch"

//------------------------------------------------------------------------------

CLASS ZXailerSqliteDatasource FROM TSqliteDataSource

RESERVED:
   DATA lDateAsString            INIT .T.
   DATA lDoubleQuotes            INIT .F.
   DATA lAllowProcessMessages    INIT .F.
   DATA lReadToCache             INIT .F.
   DATA lUtfToAnsi               INIT .F.

END CLASS

//------------------------------------------------------------------------------
