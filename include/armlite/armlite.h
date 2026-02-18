
#pragma once

void WriteChar(char c);
void WriteString(char* string);
void WriteSignedNum(int num);
void WriteUnsignedNum(unsigned int num);
void ReadString(char* outputString);

[[noreturn]]
void Panic(char* message);
