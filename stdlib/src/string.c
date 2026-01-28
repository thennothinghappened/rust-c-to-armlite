
#include <stddef.h>
#include <string.h>

size_t strlen(char *buffer)
{
	size_t index = 0;

	while (buffer[index] != '\0')
	{
		index = index + 1;
	}

	return index;
}

size_t strnlen_s(char* buffer, size_t bufferLength)
{
	size_t index = 0;

	while (index != bufferLength)
	{
		if (buffer[index] == 0)
		{
			break;
		}

		index = index + 1;
	}

	return index;
}

#ifdef __armlitec__

[[armlite_c::raw_assembly]]
void memcpy(void *destination, void *source, size_t count)
{
	// R0: Destination address
	// R1: Source address
	// R2: Byte count
	__asm__("POP {R0, R1, R2}");

	// Check if caller asked us to copy 0 bytes for some reason.
	__asm__("CMP R2, #0");
	__asm__("BEQ Ldone__memcpy");

	// Cheat a bit so that indexing by the count works :P
	__asm__("SUB R0, R0, #1");
	__asm__("SUB R1, R1, #1");

__asm__("Lloop__memcpy:");
	__asm__("LDRB R3, [R1, R2]");
	__asm__("STRB R3, [R0, R2]");
	__asm__("SUBS R2, R2, #1");
	__asm__("BNE Lloop__memcpy");

__asm__("Ldone__memcpy:");
	__asm__("MOV PC, LR");
}

#endif
