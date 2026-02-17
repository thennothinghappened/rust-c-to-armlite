
#include <stddef.h>
#include <armlite/armlite.h>
#include <armlite/armlite-io.h>

int PrintFormatted(char *format, int *argsArray)
{
	if (format == NULL)
	{
		Panic("PrintFormatted :: Passed NULL as format specifier");
	}

	size_t argIndex = 0;
	size_t charIndex = 0;

	while (1)
	{
		char c = format[charIndex];
		
		if (c == '\0')
		{
			break;
		}

		charIndex = charIndex + 1;

		if (c != '%')
		{
			WriteChar(c);
			continue;
		}

		c = format[charIndex];

		if (argsArray == NULL)
		{
			Panic("PrintFormatted :: Passed NULL as argument array pointer");
		}

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
