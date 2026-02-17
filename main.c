
#include <armlite/armlite-io.h>

int g_stuff;

int main()
{
	g_stuff = 5;

	int args[1];
	args[0] = g_stuff;
	PrintFormatted("uwu %d\n", args);

	return 0;
}
