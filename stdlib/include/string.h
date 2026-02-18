
#pragma once
#include <stddef.h>

size_t strlen(const char* buffer);
size_t strnlen_s(const char* buffer, size_t bufferLength);
void memcpy(void *destination, const void *source, size_t count);
