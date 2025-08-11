/*
 * Proyecto: PrgParser
 * Fichero: CodeHelper.ch
 */

// Elementos de cada línea importada separados por ";"

#define chFILE         1  // Nombre del fichero
#define chTYPE         2  // Tipo (ver más abajo)
#define chLINE         3  // Número de línea
#define chSYMBOL       4  // Nombre del símmbolo, vacio cuando es tipo CLASSDEC
#define chCLASS        5  // Nombre de clase, para Func/Proc == ID_SYMBOL
#define chVALTYPE      6  // Texto indicativo del tipo de dato para tipo 5 (TYPE_DATA). Parametros para C funcs.
#define chPARAMS       7  // Matriz de elementos {cParName, cParType, nParInfo(BYREF, OPTIONAL)}. En blanco para C funcs.
#define chRETURN       8  // Texto con el valor de retorno, ancestro cuando es tipo CLASSDEC, y persistent en DATA

#define chPRGFUNC    0
#define chCFUNC      1
#define chPURECFUNC  2
#define chCLASSDEC   3
#define chMETHOD     4
#define chEVENT      5
#define chDATA       6
#define chVAR        7
#define chDEF        8
#define chPUBLIC     9
#define chALL        10 // Para WHelpClass:GetSymbols()

#define chBYVALUE       0
#define chBYREF         1
#define chOPTIONAL      2
#define chPERSISTENT    4
#define chREADONLY      8

#ifndef __C__
   #xtranslate  IS_BYREF( <n> )       => HB_BitTest( <n> ,0 )
   #xtranslate  IS_OPTIONAL( <n> )    => HB_BitTest( <n> ,1 )
   #xtranslate  IS_PERSISTENT( <n> )  => HB_BitTest( <n> ,2 )
   #xtranslate  IS_READONLY( <n> )    => HB_BitTest( <n> ,3 )
#endif

#define LOREM_IPSUM  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin varius dui arcu, eu viverra dolor ullamcorper vel. Suspendisse rutrum, odio ac lacinia pretium, nisl nunc luctus arcu, at interdum orci lectus sed neque. Ut scelerisque orci non massa dapibus auctor. Cras dapibus porttitor viverra. Nunc nulla nunc, tempor auctor nisi eget, feugiat pellentesque neque. Pellentesque rhoncus accumsan enim at condimentum. Donec a lorem urna. Integer enim nisi, porttitor ut ipsum eget, eleifend porta nisi. Cras ut lacus urna. Nam id maximus arcu. In vel semper turpis. Ut tortor augue, bibendum nec facilisis nec, dapibus a nibh. Suspendisse potenti."




