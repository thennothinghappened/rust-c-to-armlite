
#include "armlite.h"
#include "string.h"

#ifdef __arm__

void __WriteSizedString(char *string, size_t length)
{
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
}

#endif

void WriteChar(char c)
{
#ifdef __armlite__
	char local = c;

	__asm__("LDRB R0, [R11-#4]");
	__asm__("STRB R0, .WriteChar");
#endif

#ifdef __arm__
	__WriteSizedString(&c, 1);
#endif
}

void WriteString(char *string)
{
#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .WriteString");
#endif

#ifdef __arm__
	__WriteSizedString(string, strlen(string));
#endif
}

void WriteSignedNum(int num)
{
#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .WriteSignedNum");
#endif

#ifdef __arm__
	WriteString("FIXME: WriteSignedNum is not supported");
#endif
}

void WriteUnsignedNum(unsigned int num)
{
#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .WriteUnsignedNum");
#endif

#ifdef __arm__
	WriteString("FIXME: WriteUnsignedNum is not supported");
#endif
}

void ReadString(char* outputString)
{
#ifdef __armlite__
	__asm__("LDR R0, [R11+#8]");
	__asm__("STR R0, .ReadString");
#endif

#ifdef __arm__
	char* message = "ReadString is not supported.";
	memcpy(outputString, message, strlen(message) + 1);
#endif
}

[[noreturn]]
void Panic(char* message)
{
	WriteString("\n\nProgram panicked with error message: ");
	WriteString(message);
	
	__asm__("B c_halt");
}

[[armlite_c::raw_assembly, noreturn]]
void Exit(int code)
{
	__asm__("POP {R0}");
	__asm__("B c_entry_post_run");
}
