
#include "armlite.h"

int main() {
	int a = 2;
	int b = 3;
	int c = 0;

	if (a < b) {
		c = a;
	} else {
		c = b;
	}

	WriteSignedNum(c);
	return 0;
}
