/*
 * Proyect: XailerWeb framework
 * File: ZControl.prg
 * Description: Base class for HTML controls
 * Author: Ignacio Ortiz de Zuniga
 * Copyright 2024 Ignacio Ortiz de Zuniga
 * Copyright 2024 Oz Software
 * Note: This class is internal. Should not be instanciated directly
 */

#include "xailerweb.ch"

CLASS ZControl FROM WBasic
EXPORTED:
   DATA cText    INIT ""
   DATA cTag            INIT ""

RESERVED:
   METHOD Preprocess() VIRTUAL

ENDCLASS

//------------------------------------------------------------------------------



