
#pragma once

#include <stddef.h>

void WriteChar(char c);
void WriteString(char* string);
void WriteSignedNum(int num);
void WriteUnsignedNum(unsigned int num);
void ReadString(char* outputString);

[[noreturn]]
void Panic(char* message);

[[noreturn]]
void Exit(int code);

void memcpy(void *destination, void *source, size_t count);
