
#include <armlite/armlite-io.h>

int g_stuff;
bool g_whatever;

int main()
{
	g_stuff += 5;
	g_whatever = true;

	if (g_whatever) {
		PrintFormatted("uwu %d\n", &g_stuff);
	}

	return 0;
}
