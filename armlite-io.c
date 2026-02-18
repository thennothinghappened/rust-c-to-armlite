
#include <stddef.h>
#include <armlite/armlite.h>
#include <armlite/armlite-io.h>

size_t PrintFormatted(char *format, int *argsArray) {
	if (format == NULL) {
		Panic("PrintFormatted :: Passed NULL as format specifier");
	}

	size_t argIndex = 0;
	size_t charIndex = 0;

	for (;;) {
		char c = format[charIndex];
		
		if (c == '\0') {
			break;
		}

		charIndex += 1;

		if (c != '%') {
			WriteChar(c);
			continue;
		}

		c = format[charIndex];

		if (argsArray == NULL) {
			Panic("PrintFormatted :: Passed NULL as argument array pointer");
		}

		if (c == 'd') {
			// Next argument is a signed integer.
			int number = argsArray[argIndex];
			WriteSignedNum(number);

			// while (number > 9)
			// {
			// 	Panic("Not yet implemented");
			// }

			charIndex += 1;
		} else {
			Panic("Unknown format specifier encountered");
		}

		argIndex += 1;
	}

	return charIndex;
}
