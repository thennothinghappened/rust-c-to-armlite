
#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __armlite__
/// Whether the ARMLite display is supported on the current target.
#define ARMLITE_GRAPHICS true
#endif

typedef uint32_t Colour;

#ifdef ARMLITE_GRAPHICS

#define DISPLAY_GRID_ROWS 24
#define DISPLAY_GRID_COLUMNS 32

Colour *g_displayGrid = NULL;

/// Initialise the ARMLite display. Must be called before writing to `g_displayGrid`.
void DisplayInit();

#endif
