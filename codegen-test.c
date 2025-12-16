
#include "stdint.h"
#include "armlite.h"
#include "StringLength.c"

int main()
{
	WriteString("Please enter your name:\n");

	char buffer[128];
	ReadString(buffer);

	size_t nameLength = StringLength(buffer, sizeof(buffer));

	WriteString("Your name is");
	WriteUnsignedNum(nameLength);
	WriteString("letters long.\n");

	return nameLength;
}
