
#include "armlite.h"

typedef unsigned int size_t;

size_t StringLength(char* buffer, size_t bufferLength);

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

size_t StringLength(char* buffer, size_t bufferLength)
{
	size_t index = 0;

	while (!(index == bufferLength))
	{
		if (buffer[index] == 0)
		{
			break;
		}

		index = index + 1;
	}

	return index;
}
