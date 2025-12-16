
#ifndef STRINGLENGTH_C
#define STRINGLENGTH_C

#include "stdint.h"

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

#endif
