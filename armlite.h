
#ifndef ARMLITE_H
#define ARMLITE_H

void WriteString(char* string);
void WriteSignedNum(int num);
void WriteUnsignedNum(unsigned int num);
void ReadString(char* outputString);

int add(int a, int b);

void if_(int condition, void* if_true, void* if_false);

#endif
