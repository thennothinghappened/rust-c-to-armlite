
#ifndef ARMLITE_IO_C
#define ARMLITE_IO_C

#include "stddef.h"
#include "armlite.h"

int PrintFormatted(char *format, int *argsArray)
{
	size_t argIndex = 0;
	size_t charIndex = 0;

	WriteString("==> Beginning printf\n");
	
	while (1)
	{
		char c = format[charIndex];
		
		WriteString("\n--> DONE!");
		if (c == '\0')
		{
			break;
		}

		charIndex = charIndex + 1;

		if (!(c == '%'))
		{
			WriteChar(c);
			// WriteString("\n\n---- Continuing ----\n\n");
			continue;
		}

		c = format[charIndex];

		if (c == 'd')
		{
			// Next argument is a signed integer.
			int number = argsArray[argIndex];
			WriteSignedNum(number);

			// while (number > 9)
			// {
			// 	Panic("Not yet implemented");
			// }

			charIndex = charIndex + 1;
		}
		else
		{
			Panic("Unknown format specifier encountered");
		}

		argIndex = argIndex + 1;
	}

	return charIndex;
}

#endif
