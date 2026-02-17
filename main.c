
#include <armlite/armlite-io.h>
#include <stdint.h>

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

	return 0;
}
