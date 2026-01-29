
#include <armlite/armlite.h>
#include <string.h>

#ifndef __armlitec__
// We're compiling in a hosted environment with libc available.

extern int printf(const char * format, ...);
extern signed char getchar();

[[noreturn]]
extern void exit(int code);

#endif

#ifndef __armlite__

void __WriteSizedString(char *string, size_t length)
{
#ifdef __armlitec__
	// Struct described [here](https://cpulator.01xz.net/doc/#syscall_semihosting)
	// [0] = file handler
	// [4] = data pointer
	// [8] = data length
	// 
	// Stored in reverse order because the stack grows downwards.
	size_t dataLength = length;
	char* dataPointer = string;
	size_t fileHandle = 1;

	__asm__("MOV R0, #0x5"); // SYS_WRITE
	__asm__("MOV R1, SP"); // R1 holds pointer to struct
	__asm__("SVC #0x123456");
#else
	printf("%.*s", (int) length, string);
#endif
}

#endif

void WriteChar(char c)
{
#ifdef __armlite__
	char local = c;

	__asm__("LDRB R0, [R11-#4]");
	__asm__("STRB R0, .WriteChar");
#else
	__WriteSizedString(&c, 1);
#endif
}

void WriteString(char *string)
{
#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .WriteString");
#else
	__WriteSizedString(string, strlen(string));
#endif
}

void WriteSignedNum(int num)
{
#ifdef __armlitec__

#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .WriteSignedNum");
#else
	WriteString("FIXME: WriteSignedNum is not supported");
#endif

#else
	printf("%d", num);
#endif
}

void WriteUnsignedNum(unsigned int num)
{
#ifdef __armlitec__

#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .WriteUnsignedNum");
#else
	WriteString("FIXME: WriteUnsignedNum is not supported");
#endif

#else
	printf("%u", num);
#endif
}

void ReadString(char* outputString)
{
#ifdef __armlitec__

#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .ReadString");
#else
	char* message = "ReadString is not supported.";
	memcpy(outputString, message, strlen(message) + 1);
#endif

#else
	size_t i;

	for (i = 0; i < 127; i ++)
	{
		signed char c = getchar();

		if (c == '\n' || c == -1)
		{
			break;
		}

		outputString[i] = c;
	}

	outputString[i] = '\0';
#endif
}

[[noreturn]]
void Panic(char* message)
{
	WriteString("\n\nProgram panicked with error message: ");
	WriteString(message);
	
#ifdef __armlitec__
	__asm__("B c_halt");
#else
	for (;;) {}
#endif
}

#ifdef __armlitec__

[[armlite_c::raw_assembly, noreturn]]
void Exit(int code)
{
	__asm__("POP {R0}");
	__asm__("B c_entry_post_run");
}

#else

[[noreturn]]
void Exit(int code)
{
	exit(code);
}

#endif
