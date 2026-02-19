
#include <armlite/display.h>

void DisplayInit() {
	// this is here purely because clangd breaks syntax highlighting after a `#` in __asm__ lol.
	Colour *displayGrid;

	__asm__("MOV R0, #.Pixel0");
	__asm__("STR R0, [R11-#4]");

	g_displayGrid = displayGrid;
}
