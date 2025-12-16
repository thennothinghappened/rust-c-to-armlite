
#include "armlite.h"


int main() {
	char name[128];
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

	if (!(name[2] == 0)) {
		WriteString("Your name is at least 3 letters long.");
	} else {
		WriteString("Your name is less than 3 letters long.");
	}

	return 1;
}
