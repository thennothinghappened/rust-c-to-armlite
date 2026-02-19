
#include <armlite/armlite.h>
#include <armlite/armlite-io.h>
#include <armlite/display.h>
#include <string.h>

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

void Calculator() {
	// char userInput[128];
	Panic("Not yet implemented!");
}

int main() {
	char userInput[128];
	g_graphicsReady = false;

	while (true) {
		WriteString("Pick an option!\n"
			"- GraphicsTest\n"
			"- Calculator\n"
			"- Exit\n\n"
		);

		ReadString(userInput);

		if (strcmp(userInput, "GraphicsTest") == 0) {
			GraphicsTest();
			continue;
		}

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
