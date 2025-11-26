
#include "armlite.h"

int main() {
	// int c = 1 + 2;

	WriteString("The number is:");
	WriteSignedNum(add(3 + add(1, 2), 5));

	return 5;
}
