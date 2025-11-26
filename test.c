
#include "armlite.h"

int main() {
	char name[128];

	WriteString("Enter your name:");
	ReadString(name);

	WriteString("\nHello, ");
	WriteString(name);
	WriteString("!\n");

	WriteString("Interpreting name as number:");
	WriteSignedNum(name[0]);

	return 1;
}
