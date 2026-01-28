
#include "armlite.h"
#include "armlite-io.c"
#include "string.c"
#include "armlite.c"

int main() {
	int randomArray[5];
	randomArray[2] = 2;

	size_t index = 0;

	while (index < 5)
	{
		randomArray[index] = 2 + index;

		WriteString("randomArray[");
		WriteUnsignedNum(index);
		WriteString("] = ");
		WriteSignedNum(randomArray[index]);
		WriteChar('\n');

		index = index + 1;
	}

	// char name[128];
	// char *string = "stuff";

	// WriteString(string);

	// WriteString("Enter your name:");
	// ReadString(name);

	// WriteString("\nHello, ");
	// WriteString(name);
	// WriteString("!\n");

	// WriteString("The first letter of your name is ");
	// WriteChar(name[0]);
	// WriteString("\n");

	// // int args[1] = {
	// // 	name[0]
	// // };

	// int firstArg = name[0];
	// PrintFormatted("The first letter of your name is %d.\n", &firstArg);

	// size_t nameLength = strnlen_s(name, sizeof(name));
	
	// if (nameLength < 3)
	// {
	// 	WriteString("Your name is less than 3 letters long.");
	// }
	// else
	// {
	// 	WriteString("Your name is at least 3 letters long.");
	// }

	// return 1;
}
