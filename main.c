
#include <armlite/armlite.h>
#include <armlite/armlite-io.h>
#include <stdint.h>
#include <stdlib.h>

// struct Blah {
// 	int a;
// 	int b;
// };

// void ArrayReceiver(int *array) {
// 	PrintFormatted("ArrayReceiver :: Array = {%d, %d, %d, %d}", array);
// }

// void BlahReceiver(struct Blah blah) {
// 	PrintFormatted("BlahReceiver :: reinterpreting `blah` argument as an integer array: a=%d, b=%d.\n", (int*) &blah);

// 	struct Blah blah2 = blah;
// 	PrintFormatted("BlahReceiver :: reinterpreting `blah2` variable copy as an integer array: a=%d, b=%d.\n", (int*) &blah2);

// 	int args[2];
// 	args[0] = blah.a;
// 	args[1] = blah.b;
// 	PrintFormatted("BlahReceiver :: `blah` values into integer array: a=%d, b=%d.\n", args);
// }

// int main() {
// 	struct Blah blah;
// 	blah.a = 1;
// 	blah.b = 2;

// 	PrintFormatted("main :: reinterpreting `blah` variable as an integer array: a=%d, b=%d.\n", (int*) &blah);

// 	struct Blah blah2 = blah;
// 	PrintFormatted("main :: reinterpreting `blah2` variable copy as an integer array: a=%d, b=%d.\n", (int*) &blah2);

// 	int args[2];
// 	args[0] = blah.a;
// 	args[1] = blah.b;
// 	PrintFormatted("main :: `blah` values into integer array: a=%d, b=%d.\n", args);

// 	BlahReceiver(blah);

// 	int array[4];
// 	array[0] = 1;
// 	array[1] = 2;
// 	array[2] = 3;
// 	array[3] = 4;

// 	PrintFormatted("ArrayReceiver :: Array = {%d, %d, %d, %d}", array);
// 	ArrayReceiver(array);

// 	return 0;
// }

int g_stuff;
bool g_whatever;

typedef struct {
	int32_t x;
	int32_t y;
} Point2D;

#define ValueKind_Boolean 0
#define ValueKind_Integer 1

typedef struct {
	int kind;
	union { bool boolean; int integer; } data;
} Value;

void PrintValue(Value *value) {
	if (value->kind == ValueKind_Boolean) {
		if (value->data.boolean) {
			WriteString("true");
		} else {
			WriteString("false");
		}
	} else if (value->kind == ValueKind_Integer) {
		WriteSignedNum(value->data.integer);
	} else {
		int args[1];
		args[0] = value->kind;

		PrintFormatted("Invalid value kind %d.\n", args);
		abort();
	}
}

int main()
{
	g_stuff += 5;
	g_whatever = true;

	Point2D pos;
	pos.x = 2;
	pos.y = 3;

	PrintFormatted("Position = %d, %d\n", (int*) &pos);

	if (g_whatever) {
		PrintFormatted("uwu %d\n", &g_stuff);
	}

	Value valueA;
	valueA.kind = ValueKind_Integer;
	valueA.data.integer = 12345;

	PrintValue(&valueA);

	valueA.data.boolean = false;
	PrintValue(&valueA);

	Value valueB;
	valueB.kind = ValueKind_Boolean;
	valueB.data.boolean = true;

	PrintValue(&valueB);

	// String str;
	// str.buffer = ;

	return 0;
}
