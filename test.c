
#include "armlite.h"

int main() {
	char name[128];

	WriteString("Enter your name:");
	ReadString(name);

	WriteString("\nHello, ");
	WriteString(name);
	WriteString("!\n");
}
