/*
 * Proyecto: xaWeb framework
 * Fichero: matarialize.ch
 * Descripción: Cabecera para proyectos xaWeb + materialize
 * Autor: Ignacio Ortiz de Zuniga
 * Copyright 2025 OZ Software (marca comercial de Ortiz de Zúñiga SL)
 * Copyright 2025 Ortiz de Zúñiga SL
*/

#ifndef _MATERIALIZE_CH_
#define _MATERIALIZE_CH_

REQUEST zMatNavbar, zMatInput, zMatEdit, zMatEmail, zMatNumber, zMatTextarea
REQUEST zMatCheckbox, zMatRadio, zMatButton, zMatFooter, zMatRange, zMatSelect
REQUEST zMatAutoComplete, zMatControl

// Necesarios por si se utiliza FormEditor
REQUEST WSwitch,WDatePicker,WTimePicker

// No necesarios por no existir aún en xaWeb básico
//REQUEST zMatBreadcrumb, zMatCard, zMatCarousel, zMatCollapsible
//REQUEST zMatCollection, zMatDropdown, zMatFloatActBtn, zMatModal
//REQUEST zMatPagination, zMatPreloader, zMatSidenav, zMatSlider, zMatTabs

#include "xaWeb.ch"

#endif