
#ifndef ARMLITE_H
#define ARMLITE_H

void WriteChar(char c);
void WriteString(char* string);
void WriteSignedNum(int num);
void WriteUnsignedNum(unsigned int num);
void ReadString(char* outputString);
void Panic(char* message);
void Exit(int code);

#endif
