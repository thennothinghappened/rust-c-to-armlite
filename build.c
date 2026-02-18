// 
// Armlite-C has no idea what linking is, so we're doing unity builds - source C files are listed in
// the order in which they should be compiled.
// 

#ifdef __armlitec__
#include "stdlib/src/string.c"
#include "stdlib/src/stdlib.c"
#endif

#include "armlite.c"
#include "armlite-io.c"
#include "main.c"
