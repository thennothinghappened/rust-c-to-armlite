
#pragma once

#include <stddef.h>

#ifndef __ARMLITE_HEAP_BYTES
/// The size of the heap, in bytes. Can be overriden by setting it before including <stdlib.h>.
#define __ARMLITE_HEAP_BYTES 4096
#endif

[[noreturn]]
void abort();

[[noreturn]]
void exit(int exit_code);

void *malloc(size_t size);

void free(void *ptr);
