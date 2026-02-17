
#include <armlite/armlite-io.h>

int g_stuff;

int main()
{
	g_stuff += 5;
	PrintFormatted("uwu %d\n", &g_stuff);

	return 0;
}
