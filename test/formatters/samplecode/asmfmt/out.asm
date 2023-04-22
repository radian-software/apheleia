# Taken from https: // files.klauspost.com/diff.html

#include "zasm_GOOS_GOARCH.h"
#include "funcdata.h"
#include "textflag.h"

TEXT runtime·rt0_go(SB), NOSPLIT, $0
	// copy arguments forward on an even stack
	MOVQ DI, AX       // argc
	MOVQ SI, BX       // argv
	SUBQ $(4*8+7), SP // 2args 2auto
	ANDQ $~15, SP
	MOVQ AX, 16(SP)
	MOVQ BX, 24(SP)

	// create istack out of the given (operating system) stack.
	// _cgo_init may update stackguard.
	MOVQ $runtime·g0(SB), DI
	LEAQ (-64*1024+104)(SP), BX
	MOVQ BX, g_stackguard0(DI)
	MOVQ BX, g_stackguard1(DI)
	MOVQ BX, (g_stack+stack_lo)(DI)
	MOVQ SP, (g_stack+stack_hi)(DI)

	// find out information about the processor we're on
	MOVQ $0, AX
	CPUID
	CMPQ AX, $0
	JE   nocpuinfo
	MOVQ $1, AX
	CPUID
	MOVL CX, runtime·cpuid_ecx(SB)
	MOVL DX, runtime·cpuid_edx(SB)

nocpuinfo:

	// if there is an _cgo_init, call it.
	MOVQ  _cgo_init(SB), AX
	TESTQ AX, AX
	JZ    needtls

	// g0 already in DI
	MOVQ DI, CX              // Win64 uses CX for first parameter
	MOVQ $setg_gcc<>(SB), SI
	CALL AX

