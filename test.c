
#include "armlite.h"

char name[128];

int main() {
	char *string = "stuff";

	WriteString(string);

	WriteString("Enter your name:");
	ReadString(name);

	WriteString("\nHello, ");
	WriteString(name);
	WriteString("!\n");

	WriteString("The first letter of your name is ");
	WriteChar(name[0]);
	WriteString("\n");

	if (!(name[3] == 0)) {
		WriteString("Your name is at least 3 letters long.");
	} else {
		WriteString("Your name is less than 3 letters long.");
	}

	return 1;
}
