
#include <stddef.h>
#include <stdlib.h>

[[armlite_c::raw_assembly, noreturn]]
void exit(int exit_code) {
	__asm__("POP {R0}");
	__asm__("B c_entry_post_run");
}

[[armlite_c::raw_assembly, noreturn]]
void abort() {
	exit(1);
}

/// ARMLite doesn't have virtual memory, it is purely ours to manage. The heap is thus just a large
/// array of memory we're setting aside at compile-time.
/// 
/// FIXME: This is not a good heap implementation!!
struct {
	size_t nextOffset;
	char heap[__ARMLITE_HEAP_BYTES];
} __g_heap;

bool __g_heapInitialised = false;

void *malloc(size_t size) {
	if (!__g_heapInitialised) {
		__g_heap.nextOffset = 0;
		__g_heapInitialised = true;
	}

	size_t remainingSpace = (__ARMLITE_HEAP_BYTES - __g_heap.nextOffset);

	if (remainingSpace < size) {
		return NULL;
	}

	void *ptr = &__g_heap.heap[__g_heap.nextOffset];
	__g_heap.nextOffset += size;

	return ptr;
}

void free(void *ptr) {
	// TODO: Implement free(), currently it just piles up!
	// if (ptr == NULL) {
	// 	return;
	// }

	// ptrdiff_t offset = (__g_heap.nextOffset)
}
