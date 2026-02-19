
#pragma once

#include <stdint.h>

#define DISPLAY_GRID_ROWS 24
#define DISPLAY_GRID_COLUMNS 32

typedef uint32_t Colour;

Colour *g_displayGrid;

/// Initialise the ARMLite display. Must be called before writing to `g_displayGrid`.
void DisplayInit();
