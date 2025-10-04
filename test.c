
struct A {
	int a;
};

union Blah {
	struct A a, *b;
};

int main() {
	typedef struct A Blah;
	return 0;
}
