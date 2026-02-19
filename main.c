
#include <stdlib.h>
#include <armlite/armlite.h>
#include <armlite/armlite-io.h>
#include <armlite/display.h>
#include <string.h>

#ifdef __armlite__
#define GRAPHICS_SUPPORTED true
#endif

#ifdef GRAPHICS_SUPPORTED

bool g_graphicsReady;

void GraphicsTest() {
	if (!g_graphicsReady) {
		DisplayInit();
		g_graphicsReady = true;
	}
	
	for (size_t y = 0; y < DISPLAY_GRID_ROWS; y ++) {
		for (size_t x = 0; x < DISPLAY_GRID_COLUMNS; x ++) {
			size_t index = x + (y << 5);
			g_displayGrid[index] = (x << 12) + (index);
		}
	}
}

#endif

int MultiplyBy10(int value) {
	// yes this is cursed but there's no multiplying yet lol.
	return (value << 3) + (value << 1);
}

/// Convert an ASCII letter to a number, or return a negative value.
int ToNumber(char asciiChar) {
	int number = (asciiChar - '0');

	if (number > 9) {
		return 0-1;
	}

	return number;
}

void SkipSpaces(char **next) {
	while (**next == ' ') {
		*next += 1;
	}
}

void Calculator() {
	char userInput[128];

	WriteString("Enter a basic equation:\n");
	ReadString(userInput);

	char *next = userInput;

	int leftNumber = 0;
	int rightNumber = 0;
	char operator = '\0';

	// Get the first operand.
	while (*next != '\0') {
		SkipSpaces(&next);

		int number = ToNumber(*next);

		if (number < 0) {
			operator = *next;
			next ++;
			break;
		}

		leftNumber = MultiplyBy10(leftNumber) + number;
		next ++;
	}

	// Get the second operand.
	while (*next != '\0') {
		SkipSpaces(&next);

		int number = ToNumber(*next);

		if (number < 0) {
			int args[1];
			args[0] = (int) *next;

			PrintFormatted("`%c` isn't a number!\n", args);
			abort();
		}

		rightNumber = MultiplyBy10(rightNumber) + number;
		next ++;
	}

	int result;

	// Do the operation!
	if (operator == '+') {
		result = leftNumber + rightNumber;
	} else if (operator == '-') {
		result = leftNumber - rightNumber;
	} else {
		int args[1];
		args[0] = (int) operator;
		
		PrintFormatted("Operator `%c` is not supported.\n", args);
		abort();
	}

	int args[4];
	args[0] = leftNumber;
	args[1] = (int) operator;
	args[2] = rightNumber;
	args[3] = result;
	PrintFormatted("Result: %d %c %d = %d\n", args);
}

int main() {
	char userInput[128];

#ifdef GRAPHICS_SUPPORTED
	g_graphicsReady = false;
#endif

	while (true) {
		WriteString("Pick an option!\n"
#ifdef GRAPHICS_SUPPORTED
			"- GraphicsTest\n"
#endif
			"- Calculator\n"
			"- Exit\n\n"
		);

		ReadString(userInput);

#ifdef GRAPHICS_SUPPORTED
		if (strcmp(userInput, "GraphicsTest") == 0) {
			GraphicsTest();
			continue;
		}
#endif

		if (strcmp(userInput, "Calculator") == 0) {
			Calculator();
			continue;
		}

		if (strcmp(userInput, "Exit") == 0) {
			break;
		}
	}

	return 0;
}
