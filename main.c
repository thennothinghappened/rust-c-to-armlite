
#include <armlite/armlite.h>
#include <armlite/armlite-io.h>
#include <stdint.h>
#include <stdlib.h>

int g_stuff;
bool g_whatever;

typedef struct {
	int32_t x;
	int32_t y;
} Point2D;

int main()
{
	g_stuff += 5;
	g_whatever = true;

	Point2D pos;
	pos.x = 2;
	pos.y = 3;

	PrintFormatted("Position = %d, %d\n", (int*) &pos);

	if (g_whatever) {
		PrintFormatted("uwu %d\n", &g_stuff);
	}

	Point2D *a = malloc(sizeof(*a));
	a->x = 3;

	PrintFormatted("Example position a = (%d, %d)", (int*)a);

	free(a);

	// String str;
	// str.buffer = ;

	return 0;
}
