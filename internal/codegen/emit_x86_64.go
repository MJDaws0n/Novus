package codegen

import (
	"fmt"
	"strings"
)

// ---------------------------------------------------------------------------
// x86-64 Assembly Emitter
//
// Produces GAS (AT&T syntax) assembly for Linux and macOS, or NASM (Intel
// syntax) assembly for Windows.
//
// Virtual registers are spilled to the stack frame.  Since x86 does not
// allow memory-to-memory operations, every VReg access is mediated through
// scratch registers %r10 and %r11 (GAS) or r10/r11 (NASM).
//
// Frame layout (from rbp downward):
//   [rbp - 8]         … local slot 0
//   [rbp - locals*8]  … last local slot
//   [rbp - (locals+1)*8] … vreg 0
//   [rbp - (locals+N+1)*8] … vreg N
// ---------------------------------------------------------------------------

func EmitX86_64(mod *IRModule, target *Target) string {
	e := &x86_64Emitter{
		mod:    mod,
		target: target,
		b:      &strings.Builder{},
	}
	e.emit()
	return e.b.String()
}

type x86_64Emitter struct {
	mod    *IRModule
	target *Target
	b      *strings.Builder
	uid    int
}

func (e *x86_64Emitter) uniqueID() int {
	e.uid++
	return e.uid
}

func (e *x86_64Emitter) emit() {
	if e.target.Flavor == NASM {
		e.emitNASM()
	} else {
		e.emitGAS()
	}
}

// ---------------------------------------------------------------------------
// Frame / vreg helpers
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) computeFrameSize(fn *IRFunc) int {
	maxVReg := 0
	for _, instr := range fn.Instrs {
		for _, op := range []Operand{instr.Dst, instr.Src1, instr.Src2} {
			if op.Kind == OpVirtReg && op.Reg+1 > maxVReg {
				maxVReg = op.Reg + 1
			}
		}
		for _, op := range instr.Args {
			if op.Kind == OpVirtReg && op.Reg+1 > maxVReg {
				maxVReg = op.Reg + 1
			}
		}
	}
	totalSlots := fn.Locals + maxVReg + 4 // +4 for safety
	size := totalSlots * 8
	if size%16 != 0 {
		size += 16 - (size % 16)
	}
	if size < 16 {
		size = 16
	}
	return size
}

// vregOffset returns the byte offset from %rbp for virtual register N.
func (e *x86_64Emitter) vregOffset(fn *IRFunc, reg int) int {
	return -((fn.Locals + reg + 1) * 8)
}

// ---------------------------------------------------------------------------
// GAS (AT&T syntax) — for Linux and macOS
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) emitGAS() {
	w := e.b

	// --- Data section ---
	if len(e.mod.Strings) > 0 {
		if e.target.OS == OS_Linux {
			w.WriteString(".data\n")
		} else {
			w.WriteString(".section __DATA,__data\n")
		}
		for _, s := range e.mod.Strings {
			label := e.target.Sym(s.Label)
			w.WriteString(fmt.Sprintf("%s:\n", label))
			w.WriteString(fmt.Sprintf("    .asciz %s\n", gasQuoteString(s.Value)))
			w.WriteString(fmt.Sprintf("%s_len:\n", label))
			w.WriteString(fmt.Sprintf("    .quad %d\n", len(s.Value)))
		}
		w.WriteString("\n")
	}

	// --- BSS: string concatenation double-buffer ---
	if e.usesStrConcat() {
		bufA := e.target.Sym("_novus_strcat_buf_a")
		bufB := e.target.Sym("_novus_strcat_buf_b")
		sel := e.target.Sym("_novus_strcat_sel")
		w.WriteString(fmt.Sprintf(".lcomm %s, 4096\n", bufA))
		w.WriteString(fmt.Sprintf(".lcomm %s, 4096\n", bufB))
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n\n", sel))
	}

	// --- Text section ---
	if e.target.OS == OS_Linux {
		w.WriteString(".text\n")
	} else {
		w.WriteString(".section __TEXT,__text\n")
	}

	entryFuncName := e.target.Sym("main")
	w.WriteString(fmt.Sprintf(".globl %s\n\n", entryFuncName))

	// Linux needs a _start entry.
	if e.target.OS == OS_Linux {
		w.WriteString(".globl _start\n")
		w.WriteString("_start:\n")
		w.WriteString(fmt.Sprintf("    call %s\n", entryFuncName))
		w.WriteString("    movq %rax, %rdi\n")
		w.WriteString("    movq $60, %rax\n")
		w.WriteString("    syscall\n\n")
	}

	for _, fn := range e.mod.Functions {
		e.emitGASFunction(fn)
	}
}

func (e *x86_64Emitter) emitGASFunction(fn *IRFunc) {
	w := e.b
	sym := e.target.Sym(fn.Name)
	frameSize := e.computeFrameSize(fn)

	w.WriteString(fmt.Sprintf("%s:\n", sym))
	w.WriteString("    pushq %rbp\n")
	w.WriteString("    movq %rsp, %rbp\n")
	if frameSize > 0 {
		w.WriteString(fmt.Sprintf("    subq $%d, %%rsp\n", frameSize))
	}

	for _, instr := range fn.Instrs {
		e.emitGASInstr(fn, instr)
	}
	w.WriteString("\n")
}

// ---------------------------------------------------------------------------
// GAS operand loading helpers
//
// loadToGAS loads an operand into a scratch register and returns the register
// name (e.g. "%r10").  For PhysRegs and Immediates, it returns the operand
// directly if safe for the context.
// ---------------------------------------------------------------------------

// promoteReg upgrades 32-bit x86 register names to 64-bit equivalents
// since we emit movq (64-bit) instructions.
func promoteReg(name string) string {
	m := map[string]string{
		"eax": "rax", "ebx": "rbx", "ecx": "rcx", "edx": "rdx",
		"esi": "rsi", "edi": "rdi", "ebp": "rbp", "esp": "rsp",
	}
	if r, ok := m[name]; ok {
		return r
	}
	return name
}

// loadToReg loads an operand value into the given scratch register.
// Returns the AT&T register string to use.
func (e *x86_64Emitter) gasLoadToReg(fn *IRFunc, op Operand, scratch string) string {
	w := e.b
	switch op.Kind {
	case OpPhysReg:
		return fmt.Sprintf("%%%s", promoteReg(op.PhysReg))
	case OpImmediate:
		w.WriteString(fmt.Sprintf("    movq $%d, %s\n", op.Imm, scratch))
		return scratch
	case OpVirtReg:
		off := e.vregOffset(fn, op.Reg)
		w.WriteString(fmt.Sprintf("    movq %d(%%rbp), %s\n", off, scratch))
		return scratch
	case OpStringRef:
		label := e.mod.Strings[op.Imm].Label
		sym := e.target.Sym(label)
		if e.target.OS == OS_Darwin {
			w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %s\n", sym, scratch))
		} else {
			w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %s\n", sym, scratch))
		}
		return scratch
	case OpMemory:
		w.WriteString(fmt.Sprintf("    movq %d(%%%s), %s\n", op.MemOffset, op.MemBase, scratch))
		return scratch
	case OpLabel:
		return op.Label
	}
	return scratch
}

// gasSpillIfNeeded stores a scratch register back to a vreg's stack slot.
func (e *x86_64Emitter) gasSpillIfNeeded(fn *IRFunc, op Operand, reg string) {
	if op.Kind == OpVirtReg {
		off := e.vregOffset(fn, op.Reg)
		e.b.WriteString(fmt.Sprintf("    movq %s, %d(%%rbp)\n", reg, off))
	}
}

// gasEnsureReg loads dst operand into scratch, returns the register or phys reg string.
func (e *x86_64Emitter) gasEnsureReg(fn *IRFunc, op Operand, scratch string) string {
	return e.gasLoadToReg(fn, op, scratch)
}

// gasStoreToOperand stores a register value into a destination operand.
func (e *x86_64Emitter) gasStoreToOperand(fn *IRFunc, dst Operand, reg string) {
	w := e.b
	switch dst.Kind {
	case OpPhysReg:
		promoted := fmt.Sprintf("%%%s", promoteReg(dst.PhysReg))
		if reg != promoted {
			w.WriteString(fmt.Sprintf("    movq %s, %s\n", reg, promoted))
		}
	case OpVirtReg:
		off := e.vregOffset(fn, dst.Reg)
		w.WriteString(fmt.Sprintf("    movq %s, %d(%%rbp)\n", reg, off))
	case OpMemory:
		w.WriteString(fmt.Sprintf("    movq %s, %d(%%%s)\n", reg, dst.MemOffset, dst.MemBase))
	}
}

// ---------------------------------------------------------------------------
// GAS instruction emission
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) emitGASInstr(fn *IRFunc, instr IRInstr) {
	w := e.b

	switch instr.Op {
	case IRLabel:
		if instr.Dst.Kind == OpLabel && instr.Dst.Label != fn.Name {
			w.WriteString(fmt.Sprintf("%s:\n", instr.Dst.Label))
		}

	case IRComment:
		if instr.Src1.Kind == OpLabel {
			w.WriteString(fmt.Sprintf("    ## %s\n", instr.Src1.Label))
		}

	case IRMov:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		if instr.Dst.Kind == OpPhysReg {
			promoted := fmt.Sprintf("%%%s", promoteReg(instr.Dst.PhysReg))
			if src != promoted {
				w.WriteString(fmt.Sprintf("    movq %s, %s\n", src, promoted))
			}
		} else {
			e.gasStoreToOperand(fn, instr.Dst, src)
		}

	case IRLea:
		// lea loads an address into a register.
		scratch := "%r10"
		if instr.Src1.Kind == OpStringRef {
			label := e.mod.Strings[instr.Src1.Imm].Label
			sym := e.target.Sym(label)
			w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %s\n", sym, scratch))
		} else if instr.Src1.Kind == OpVirtReg {
			off := e.vregOffset(fn, instr.Src1.Reg)
			w.WriteString(fmt.Sprintf("    leaq %d(%%rbp), %s\n", off, scratch))
		} else if instr.Src1.Kind == OpMemory {
			w.WriteString(fmt.Sprintf("    leaq %d(%%%s), %s\n", instr.Src1.MemOffset, instr.Src1.MemBase, scratch))
		} else {
			e.gasLoadToReg(fn, instr.Src1, scratch)
		}
		e.gasStoreToOperand(fn, instr.Dst, scratch)

	case IRLoad:
		scratch := "%r10"
		if instr.Src1.Kind == OpMemory {
			w.WriteString(fmt.Sprintf("    movq %d(%%%s), %s\n", instr.Src1.MemOffset, instr.Src1.MemBase, scratch))
		} else if instr.Src1.Kind == OpStringRef {
			label := e.mod.Strings[instr.Src1.Imm].Label
			sym := e.target.Sym(label)
			w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %s\n", sym, scratch))
		} else {
			e.gasLoadToReg(fn, instr.Src1, scratch)
		}
		e.gasStoreToOperand(fn, instr.Dst, scratch)

	case IRStore:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		if instr.Dst.Kind == OpMemory {
			w.WriteString(fmt.Sprintf("    movq %s, %d(%%%s)\n", src, instr.Dst.MemOffset, instr.Dst.MemBase))
		} else if instr.Src2.Kind != OpNone {
			addr := e.gasLoadToReg(fn, instr.Src2, "%r11")
			w.WriteString(fmt.Sprintf("    movq %s, (%s)\n", src, addr))
		}

	case IRPush:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    pushq %s\n", src))

	case IRPop:
		if instr.Dst.Kind == OpPhysReg {
			w.WriteString(fmt.Sprintf("    popq %%%s\n", instr.Dst.PhysReg))
		} else {
			w.WriteString("    popq %r10\n")
			e.gasStoreToOperand(fn, instr.Dst, "%r10")
		}

	case IRAdd:
		e.emitGASBinOp(fn, instr, "addq")
	case IRSub:
		e.emitGASBinOp(fn, instr, "subq")
	case IRMul:
		e.emitGASMul(fn, instr)
	case IRDiv:
		e.emitGASDiv(fn, instr)
	case IRMod:
		e.emitGASMod(fn, instr)

	case IRNeg:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    negq %s\n", src))
		e.gasStoreToOperand(fn, instr.Dst, src)

	case IRNot:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    xorq $1, %s\n", src))
		e.gasStoreToOperand(fn, instr.Dst, src)

	case IRAnd:
		e.emitGASBinOp(fn, instr, "andq")
	case IROr:
		e.emitGASBinOp(fn, instr, "orq")
	case IRXor:
		e.emitGASBinOp(fn, instr, "xorq")

	case IRCmpEq:
		e.emitGASCmp(fn, instr, "sete")
	case IRCmpNe:
		e.emitGASCmp(fn, instr, "setne")
	case IRCmpLt:
		e.emitGASCmp(fn, instr, "setl")
	case IRCmpLe:
		e.emitGASCmp(fn, instr, "setle")
	case IRCmpGt:
		e.emitGASCmp(fn, instr, "setg")
	case IRCmpGe:
		e.emitGASCmp(fn, instr, "setge")

	case IRJmp:
		w.WriteString(fmt.Sprintf("    jmp %s\n", instr.Dst.Label))

	case IRJmpIf:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    testq %s, %s\n", src, src))
		w.WriteString(fmt.Sprintf("    jnz %s\n", instr.Dst.Label))

	case IRJmpNot:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    testq %s, %s\n", src, src))
		w.WriteString(fmt.Sprintf("    jz %s\n", instr.Dst.Label))

	case IRCall:
		e.emitGASCall(fn, instr)

	case IRRet:
		if instr.Src1.Kind != OpNone {
			src := e.gasLoadToReg(fn, instr.Src1, "%r10")
			if src != "%rax" {
				w.WriteString(fmt.Sprintf("    movq %s, %%rax\n", src))
			}
		}
		frameSize := e.computeFrameSize(fn)
		if frameSize > 0 {
			w.WriteString(fmt.Sprintf("    addq $%d, %%rsp\n", frameSize))
		}
		w.WriteString("    popq %rbp\n")
		w.WriteString("    ret\n")

	case IRSyscall:
		w.WriteString(fmt.Sprintf("    %s\n", e.target.SyscallInstr))

	case IRInt:
		if instr.Src1.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    int $0x%x\n", instr.Src1.Imm))
		} else {
			src := e.gasLoadToReg(fn, instr.Src1, "%r10")
			w.WriteString(fmt.Sprintf("    ## dynamic int via %s (using int $0x80)\n", src))
			w.WriteString("    int $0x80\n")
		}

	case IRNop:
		w.WriteString("    nop\n")

	case IRSetReg:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		e.gasStoreToOperand(fn, instr.Dst, src)

	case IRGetReg:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		e.gasStoreToOperand(fn, instr.Dst, src)

	case IRSetFlag, IRGetFlag:
		w.WriteString("    ## flag manipulation (not yet implemented)\n")

	case IRStrConcat:
		e.emitGASStrConcat(fn, instr)
	case IRStrLen:
		e.emitGASStrLen(fn, instr)
	case IRStrIndex:
		e.emitGASStrIndex(fn, instr)
	case IRStoreByte:
		addr := e.gasLoadToReg(fn, instr.Dst, "%r11")
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    movb %sb, (%s)\n", src, addr))
	case IRGetTimeNs:
		e.emitGASGetTimeNs(fn, instr)

	case IRData:
		// handled in data section
	}
}

// ---------------------------------------------------------------------------
// GAS binary / comparison / call helpers
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) emitGASBinOp(fn *IRFunc, instr IRInstr, mnemonic string) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")
	// dst = src1 OP src2  →  r10 = src1; r10 OP= src2
	if src1 != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src1))
	}
	w.WriteString(fmt.Sprintf("    %s %s, %%r10\n", mnemonic, src2))
	e.gasStoreToOperand(fn, instr.Dst, "%r10")
}

func (e *x86_64Emitter) emitGASMul(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")
	w.WriteString(fmt.Sprintf("    movq %s, %%rax\n", src1))
	w.WriteString(fmt.Sprintf("    imulq %s, %%rax\n", src2))
	e.gasStoreToOperand(fn, instr.Dst, "%rax")
}

func (e *x86_64Emitter) emitGASDiv(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")
	w.WriteString(fmt.Sprintf("    movq %s, %%rax\n", src1))
	w.WriteString("    cqo\n")
	if src2 == "%r11" || src2 == "%rcx" {
		w.WriteString(fmt.Sprintf("    idivq %s\n", src2))
	} else {
		w.WriteString(fmt.Sprintf("    movq %s, %%rcx\n", src2))
		w.WriteString("    idivq %rcx\n")
	}
	e.gasStoreToOperand(fn, instr.Dst, "%rax")
}

func (e *x86_64Emitter) emitGASMod(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")
	w.WriteString(fmt.Sprintf("    movq %s, %%rax\n", src1))
	w.WriteString("    cqo\n")
	if src2 == "%r11" || src2 == "%rcx" {
		w.WriteString(fmt.Sprintf("    idivq %s\n", src2))
	} else {
		w.WriteString(fmt.Sprintf("    movq %s, %%rcx\n", src2))
		w.WriteString("    idivq %rcx\n")
	}
	e.gasStoreToOperand(fn, instr.Dst, "%rdx")
}

func (e *x86_64Emitter) emitGASCmp(fn *IRFunc, instr IRInstr, setcc string) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")
	if src1 != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src1))
	}
	w.WriteString(fmt.Sprintf("    cmpq %s, %%r10\n", src2))
	w.WriteString(fmt.Sprintf("    %s %%r10b\n", setcc))
	w.WriteString("    movzbq %r10b, %r10\n")
	e.gasStoreToOperand(fn, instr.Dst, "%r10")
}

func (e *x86_64Emitter) emitGASCall(fn *IRFunc, instr IRInstr) {
	w := e.b
	argRegs := e.target.ArgRegs
	stackArgs := 0

	// Push excess args in reverse order.
	if len(instr.Args) > len(argRegs) {
		for i := len(instr.Args) - 1; i >= len(argRegs); i-- {
			src := e.gasLoadToReg(fn, instr.Args[i], "%r10")
			w.WriteString(fmt.Sprintf("    pushq %s\n", src))
			stackArgs++
		}
	}

	// Move register args.
	for i := 0; i < len(instr.Args) && i < len(argRegs); i++ {
		src := e.gasLoadToReg(fn, instr.Args[i], "%r10")
		reg := fmt.Sprintf("%%%s", argRegs[i])
		if src != reg {
			w.WriteString(fmt.Sprintf("    movq %s, %s\n", src, reg))
		}
	}

	// Stack alignment for SysV ABI (16-byte aligned before call).
	needAlign := stackArgs%2 != 0
	if needAlign {
		w.WriteString("    subq $8, %rsp\n")
	}

	label := instr.Src1.Label
	callTarget := e.target.Sym(label)
	w.WriteString(fmt.Sprintf("    call %s\n", callTarget))

	if needAlign {
		stackArgs++
	}
	if stackArgs > 0 {
		w.WriteString(fmt.Sprintf("    addq $%d, %%rsp\n", stackArgs*8))
	}

	if instr.Dst.Kind != OpNone {
		e.gasStoreToOperand(fn, instr.Dst, "%rax")
	}
}

// ---------------------------------------------------------------------------
// GAS string operations (inline)
// ---------------------------------------------------------------------------

// emitGASGetTimeNs emits inline GAS assembly to get time in nanoseconds.
// macOS x86_64: gettimeofday syscall 0x2000074 (rdi=ptr, rsi=NULL).
// Linux x86_64: clock_gettime syscall 228 (rdi=CLOCK_MONOTONIC, rsi=ptr).
func (e *x86_64Emitter) emitGASGetTimeNs(fn *IRFunc, instr IRInstr) {
	w := e.b
	w.WriteString("    // __time_ns: get current time in nanoseconds\n")
	w.WriteString("    subq $16, %rsp\n") // allocate 16 bytes for struct

	if e.target.OS == OS_Darwin {
		// gettimeofday(rdi=ptr, rsi=NULL), syscall 0x2000074
		w.WriteString("    movq %rsp, %rdi\n")
		w.WriteString("    xorq %rsi, %rsi\n")
		w.WriteString("    movq $0x2000074, %rax\n")
		w.WriteString("    syscall\n")
		// rsp+0 = tv_sec, rsp+8 = tv_usec
		w.WriteString("    movq 0(%rsp), %r10\n") // tv_sec
		w.WriteString("    movq 8(%rsp), %r11\n") // tv_usec
		// result = tv_sec * 1_000_000_000 + tv_usec * 1000
		w.WriteString("    imulq $1000000000, %r10, %r10\n")
		w.WriteString("    imulq $1000, %r11, %r11\n")
		w.WriteString("    addq %r11, %r10\n")
	} else {
		// clock_gettime(rdi=CLOCK_MONOTONIC=1, rsi=ptr), syscall 228
		w.WriteString("    movq $1, %rdi\n")
		w.WriteString("    movq %rsp, %rsi\n")
		w.WriteString("    movq $228, %rax\n")
		w.WriteString("    syscall\n")
		// rsp+0 = tv_sec, rsp+8 = tv_nsec
		w.WriteString("    movq 0(%rsp), %r10\n") // tv_sec
		w.WriteString("    movq 8(%rsp), %r11\n") // tv_nsec
		// result = tv_sec * 1_000_000_000 + tv_nsec
		w.WriteString("    imulq $1000000000, %r10, %r10\n")
		w.WriteString("    addq %r11, %r10\n")
	}

	w.WriteString("    addq $16, %rsp\n") // deallocate
	e.gasStoreToOperand(fn, instr.Dst, "%r10")
}

func (e *x86_64Emitter) emitGASStrConcat(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")

	id := e.uniqueID()
	copy1Label := fmt.Sprintf(".Lsc1_%d", id)
	copy2Label := fmt.Sprintf(".Lsc2_%d", id)
	doneLabel := fmt.Sprintf(".Lscd_%d", id)

	bufSym := e.target.Sym("_novus_strcat_buf")

	// Ensure source operands are in r10 / r11.
	if src1 != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src1))
	}
	if src2 != "%r11" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r11\n", src2))
	}

	// Save working registers and load buffer address.
	w.WriteString("    pushq %rdi\n")
	w.WriteString("    pushq %rcx\n")
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rdi\n", bufSym))
	w.WriteString("    pushq %rdi\n") // save buffer start

	// Copy left string (skip null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy1Label))
	w.WriteString("    movb (%r10), %cl\n")
	w.WriteString("    testb %cl, %cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", copy2Label))
	w.WriteString("    movb %cl, (%rdi)\n")
	w.WriteString("    incq %r10\n")
	w.WriteString("    incq %rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy1Label))

	// Copy right string (including null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy2Label))
	w.WriteString("    movb (%r11), %cl\n")
	w.WriteString("    movb %cl, (%rdi)\n")
	w.WriteString("    testb %cl, %cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", doneLabel))
	w.WriteString("    incq %r11\n")
	w.WriteString("    incq %rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy2Label))

	// Done — recover buffer start as result.
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	w.WriteString("    popq %r10\n") // buffer start
	w.WriteString("    popq %rcx\n") // restore
	w.WriteString("    popq %rdi\n") // restore
	e.gasStoreToOperand(fn, instr.Dst, "%r10")
}

func (e *x86_64Emitter) emitGASStrLen(fn *IRFunc, instr IRInstr) {
	w := e.b
	src := e.gasLoadToReg(fn, instr.Src1, "%r10")

	id := e.uniqueID()
	startLabel := fmt.Sprintf(".Lstrlen_s_%d", id)
	doneLabel := fmt.Sprintf(".Lstrlen_d_%d", id)

	// Use %r10 for pointer and %r11 for counter to avoid clobbering user registers.
	if src != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src))
	}
	w.WriteString("    xorq %r11, %r11\n")
	w.WriteString(fmt.Sprintf("%s:\n", startLabel))
	w.WriteString("    cmpb $0, (%r10,%r11)\n")
	w.WriteString(fmt.Sprintf("    je %s\n", doneLabel))
	w.WriteString("    incq %r11\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", startLabel))
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	e.gasStoreToOperand(fn, instr.Dst, "%r11")
}

func (e *x86_64Emitter) emitGASStrIndex(fn *IRFunc, instr IRInstr) {
	w := e.b
	src := e.gasLoadToReg(fn, instr.Src1, "%r10")
	idx := e.gasLoadToReg(fn, instr.Src2, "%r11")

	// Use scratch registers to avoid clobbering user state.
	if src != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src))
	}
	if idx != "%r11" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r11\n", idx))
	}
	w.WriteString("    xorq %rax, %rax\n")
	w.WriteString("    movb (%r10,%r11), %al\n")
	e.gasStoreToOperand(fn, instr.Dst, "%rax")
}

// ---------------------------------------------------------------------------
// NASM (Intel syntax) — for Windows
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) emitNASM() {
	w := e.b

	w.WriteString("bits 64\n")
	w.WriteString("default rel\n\n")

	if len(e.mod.Strings) > 0 {
		w.WriteString("section .data\n")
		for _, s := range e.mod.Strings {
			w.WriteString(fmt.Sprintf("    %s: db %s, 0\n", s.Label, nasmQuoteString(s.Value)))
			w.WriteString(fmt.Sprintf("    %s_len: equ $ - %s - 1\n", s.Label, s.Label))
		}
		w.WriteString("\n")
	}

	// BSS: string concatenation double-buffer.
	if e.usesStrConcat() {
		w.WriteString("section .bss\n")
		w.WriteString("    _novus_strcat_buf_a: resb 4096\n")
		w.WriteString("    _novus_strcat_buf_b: resb 4096\n")
		w.WriteString("    _novus_strcat_sel: resb 8\n\n")
	}

	w.WriteString("section .text\n")
	for _, fn := range e.mod.Functions {
		w.WriteString(fmt.Sprintf("    global %s\n", fn.Name))
	}
	w.WriteString("\n")

	for _, fn := range e.mod.Functions {
		e.emitNASMFunction(fn)
	}
}

func (e *x86_64Emitter) emitNASMFunction(fn *IRFunc) {
	w := e.b
	frameSize := e.computeFrameSize(fn)

	w.WriteString(fmt.Sprintf("%s:\n", fn.Name))
	w.WriteString("    push rbp\n")
	w.WriteString("    mov rbp, rsp\n")
	if frameSize > 0 {
		w.WriteString(fmt.Sprintf("    sub rsp, %d\n", frameSize))
	}

	for _, instr := range fn.Instrs {
		e.emitNASMInstr(fn, instr)
	}
	w.WriteString("\n")
}

// ---------------------------------------------------------------------------
// NASM operand helpers
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) nasmLoadToReg(fn *IRFunc, op Operand, scratch string) string {
	w := e.b
	switch op.Kind {
	case OpPhysReg:
		return promoteReg(op.PhysReg)
	case OpImmediate:
		w.WriteString(fmt.Sprintf("    mov %s, %d\n", scratch, op.Imm))
		return scratch
	case OpVirtReg:
		off := e.vregOffset(fn, op.Reg)
		w.WriteString(fmt.Sprintf("    mov %s, qword [rbp%+d]\n", scratch, off))
		return scratch
	case OpStringRef:
		label := e.mod.Strings[op.Imm].Label
		w.WriteString(fmt.Sprintf("    lea %s, [rel %s]\n", scratch, label))
		return scratch
	case OpMemory:
		if op.MemBase != "" {
			w.WriteString(fmt.Sprintf("    mov %s, qword [%s%+d]\n", scratch, op.MemBase, op.MemOffset))
		} else {
			w.WriteString(fmt.Sprintf("    mov %s, %d\n", scratch, op.MemOffset))
		}
		return scratch
	case OpLabel:
		return op.Label
	}
	return scratch
}

func (e *x86_64Emitter) nasmStoreToOperand(fn *IRFunc, dst Operand, reg string) {
	w := e.b
	switch dst.Kind {
	case OpPhysReg:
		promoted := promoteReg(dst.PhysReg)
		if reg != promoted {
			w.WriteString(fmt.Sprintf("    mov %s, %s\n", promoted, reg))
		}
	case OpVirtReg:
		off := e.vregOffset(fn, dst.Reg)
		w.WriteString(fmt.Sprintf("    mov qword [rbp%+d], %s\n", off, reg))
	case OpMemory:
		w.WriteString(fmt.Sprintf("    mov qword [%s%+d], %s\n", dst.MemBase, dst.MemOffset, reg))
	}
}

func (e *x86_64Emitter) nasmSpillIfNeeded(fn *IRFunc, op Operand, reg string) {
	if op.Kind == OpVirtReg {
		off := e.vregOffset(fn, op.Reg)
		e.b.WriteString(fmt.Sprintf("    mov qword [rbp%+d], %s\n", off, reg))
	}
}

// ---------------------------------------------------------------------------
// NASM instruction emission
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) emitNASMInstr(fn *IRFunc, instr IRInstr) {
	w := e.b
	switch instr.Op {
	case IRLabel:
		if instr.Dst.Kind == OpLabel && instr.Dst.Label != fn.Name {
			w.WriteString(fmt.Sprintf("%s:\n", instr.Dst.Label))
		}

	case IRComment:
		if instr.Src1.Kind == OpLabel {
			w.WriteString(fmt.Sprintf("    ; %s\n", instr.Src1.Label))
		}

	case IRMov:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		e.nasmStoreToOperand(fn, instr.Dst, src)

	case IRLea:
		scratch := "r10"
		if instr.Src1.Kind == OpStringRef {
			label := e.mod.Strings[instr.Src1.Imm].Label
			w.WriteString(fmt.Sprintf("    lea %s, [rel %s]\n", scratch, label))
		} else if instr.Src1.Kind == OpVirtReg {
			off := e.vregOffset(fn, instr.Src1.Reg)
			w.WriteString(fmt.Sprintf("    lea %s, [rbp%+d]\n", scratch, off))
		} else if instr.Src1.Kind == OpMemory {
			w.WriteString(fmt.Sprintf("    lea %s, [%s%+d]\n", scratch, instr.Src1.MemBase, instr.Src1.MemOffset))
		} else {
			e.nasmLoadToReg(fn, instr.Src1, scratch)
		}
		e.nasmStoreToOperand(fn, instr.Dst, scratch)

	case IRLoad:
		scratch := "r10"
		if instr.Src1.Kind == OpMemory {
			w.WriteString(fmt.Sprintf("    mov %s, qword [%s%+d]\n", scratch, instr.Src1.MemBase, instr.Src1.MemOffset))
		} else if instr.Src1.Kind == OpStringRef {
			label := e.mod.Strings[instr.Src1.Imm].Label
			w.WriteString(fmt.Sprintf("    lea %s, [rel %s]\n", scratch, label))
		} else {
			e.nasmLoadToReg(fn, instr.Src1, scratch)
		}
		e.nasmStoreToOperand(fn, instr.Dst, scratch)

	case IRStore:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		if instr.Dst.Kind == OpMemory {
			w.WriteString(fmt.Sprintf("    mov qword [%s%+d], %s\n", instr.Dst.MemBase, instr.Dst.MemOffset, src))
		} else if instr.Src2.Kind != OpNone {
			addr := e.nasmLoadToReg(fn, instr.Src2, "r11")
			w.WriteString(fmt.Sprintf("    mov [%s], %s\n", addr, src))
		}

	case IRPush:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		w.WriteString(fmt.Sprintf("    push %s\n", src))

	case IRPop:
		if instr.Dst.Kind == OpPhysReg {
			w.WriteString(fmt.Sprintf("    pop %s\n", instr.Dst.PhysReg))
		} else {
			w.WriteString("    pop r10\n")
			e.nasmStoreToOperand(fn, instr.Dst, "r10")
		}

	case IRAdd:
		e.emitNASMBinOp(fn, instr, "add")
	case IRSub:
		e.emitNASMBinOp(fn, instr, "sub")
	case IRMul:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
		w.WriteString(fmt.Sprintf("    mov rax, %s\n", src1))
		w.WriteString(fmt.Sprintf("    imul rax, %s\n", src2))
		e.nasmStoreToOperand(fn, instr.Dst, "rax")
	case IRDiv:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
		w.WriteString(fmt.Sprintf("    mov rax, %s\n", src1))
		w.WriteString("    cqo\n")
		w.WriteString(fmt.Sprintf("    mov rcx, %s\n", src2))
		w.WriteString("    idiv rcx\n")
		e.nasmStoreToOperand(fn, instr.Dst, "rax")
	case IRMod:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
		w.WriteString(fmt.Sprintf("    mov rax, %s\n", src1))
		w.WriteString("    cqo\n")
		w.WriteString(fmt.Sprintf("    mov rcx, %s\n", src2))
		w.WriteString("    idiv rcx\n")
		e.nasmStoreToOperand(fn, instr.Dst, "rdx")

	case IRNeg:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		if src != "r10" {
			w.WriteString(fmt.Sprintf("    mov r10, %s\n", src))
		}
		w.WriteString("    neg r10\n")
		e.nasmStoreToOperand(fn, instr.Dst, "r10")

	case IRNot:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		if src != "r10" {
			w.WriteString(fmt.Sprintf("    mov r10, %s\n", src))
		}
		w.WriteString("    xor r10, 1\n")
		e.nasmStoreToOperand(fn, instr.Dst, "r10")

	case IRAnd:
		e.emitNASMBinOp(fn, instr, "and")
	case IROr:
		e.emitNASMBinOp(fn, instr, "or")
	case IRXor:
		e.emitNASMBinOp(fn, instr, "xor")

	case IRCmpEq:
		e.emitNASMCmp(fn, instr, "sete")
	case IRCmpNe:
		e.emitNASMCmp(fn, instr, "setne")
	case IRCmpLt:
		e.emitNASMCmp(fn, instr, "setl")
	case IRCmpLe:
		e.emitNASMCmp(fn, instr, "setle")
	case IRCmpGt:
		e.emitNASMCmp(fn, instr, "setg")
	case IRCmpGe:
		e.emitNASMCmp(fn, instr, "setge")

	case IRJmp:
		w.WriteString(fmt.Sprintf("    jmp %s\n", instr.Dst.Label))
	case IRJmpIf:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		w.WriteString(fmt.Sprintf("    test %s, %s\n", src, src))
		w.WriteString(fmt.Sprintf("    jnz %s\n", instr.Dst.Label))
	case IRJmpNot:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		w.WriteString(fmt.Sprintf("    test %s, %s\n", src, src))
		w.WriteString(fmt.Sprintf("    jz %s\n", instr.Dst.Label))

	case IRCall:
		e.emitNASMCall(fn, instr)

	case IRRet:
		if instr.Src1.Kind != OpNone {
			src := e.nasmLoadToReg(fn, instr.Src1, "r10")
			if src != "rax" {
				w.WriteString(fmt.Sprintf("    mov rax, %s\n", src))
			}
		}
		frameSize := e.computeFrameSize(fn)
		if frameSize > 0 {
			w.WriteString(fmt.Sprintf("    add rsp, %d\n", frameSize))
		}
		w.WriteString("    pop rbp\n")
		w.WriteString("    ret\n")

	case IRSyscall:
		w.WriteString("    syscall\n")

	case IRInt:
		if instr.Src1.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    int 0x%x\n", instr.Src1.Imm))
		} else {
			w.WriteString("    int 0x80\n")
		}

	case IRNop:
		w.WriteString("    nop\n")

	case IRSetReg:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		e.nasmStoreToOperand(fn, instr.Dst, src)

	case IRGetReg:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		e.nasmStoreToOperand(fn, instr.Dst, src)

	case IRSetFlag, IRGetFlag:
		w.WriteString("    ; flag manipulation (not yet implemented)\n")

	case IRStrLen:
		e.emitNASMStrLen(fn, instr)
	case IRStrIndex:
		e.emitNASMStrIndex(fn, instr)
	case IRStoreByte:
		addr := e.nasmLoadToReg(fn, instr.Dst, "r11")
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		w.WriteString(fmt.Sprintf("    mov byte [%s], %sb\n", addr, src))
	case IRStrConcat:
		e.emitNASMStrConcat(fn, instr)
	case IRGetTimeNs:
		e.emitNASMGetTimeNs(fn, instr)
	}
}

func (e *x86_64Emitter) emitNASMBinOp(fn *IRFunc, instr IRInstr, mnemonic string) {
	w := e.b
	src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
	src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
	if src1 != "r10" {
		w.WriteString(fmt.Sprintf("    mov r10, %s\n", src1))
	}
	w.WriteString(fmt.Sprintf("    %s r10, %s\n", mnemonic, src2))
	e.nasmStoreToOperand(fn, instr.Dst, "r10")
}

func (e *x86_64Emitter) emitNASMCmp(fn *IRFunc, instr IRInstr, setcc string) {
	w := e.b
	src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
	src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
	if src1 != "r10" {
		w.WriteString(fmt.Sprintf("    mov r10, %s\n", src1))
	}
	w.WriteString(fmt.Sprintf("    cmp r10, %s\n", src2))
	w.WriteString(fmt.Sprintf("    %s r10b\n", setcc))
	w.WriteString("    movzx r10, r10b\n")
	e.nasmStoreToOperand(fn, instr.Dst, "r10")
}

func (e *x86_64Emitter) emitNASMCall(fn *IRFunc, instr IRInstr) {
	w := e.b
	argRegs := e.target.ArgRegs
	stackArgs := 0

	if len(instr.Args) > len(argRegs) {
		for i := len(instr.Args) - 1; i >= len(argRegs); i-- {
			src := e.nasmLoadToReg(fn, instr.Args[i], "r10")
			w.WriteString(fmt.Sprintf("    push %s\n", src))
			stackArgs++
		}
	}

	for i := 0; i < len(instr.Args) && i < len(argRegs); i++ {
		src := e.nasmLoadToReg(fn, instr.Args[i], "r10")
		if src != argRegs[i] {
			w.WriteString(fmt.Sprintf("    mov %s, %s\n", argRegs[i], src))
		}
	}

	if e.target.OS == OS_Windows {
		w.WriteString("    sub rsp, 32\n")
	}

	label := instr.Src1.Label
	w.WriteString(fmt.Sprintf("    call %s\n", label))

	if e.target.OS == OS_Windows {
		w.WriteString("    add rsp, 32\n")
	}

	if stackArgs > 0 {
		w.WriteString(fmt.Sprintf("    add rsp, %d\n", stackArgs*8))
	}

	if instr.Dst.Kind != OpNone {
		e.nasmStoreToOperand(fn, instr.Dst, "rax")
	}
}

func (e *x86_64Emitter) emitNASMStrLen(fn *IRFunc, instr IRInstr) {
	w := e.b
	src := e.nasmLoadToReg(fn, instr.Src1, "r10")

	id := e.uniqueID()
	startLabel := fmt.Sprintf(".strlen_s_%d", id)
	doneLabel := fmt.Sprintf(".strlen_d_%d", id)

	w.WriteString(fmt.Sprintf("    mov rdi, %s\n", src))
	w.WriteString("    xor rcx, rcx\n")
	w.WriteString(fmt.Sprintf("%s:\n", startLabel))
	w.WriteString("    cmp byte [rdi+rcx], 0\n")
	w.WriteString(fmt.Sprintf("    je %s\n", doneLabel))
	w.WriteString("    inc rcx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", startLabel))
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	e.nasmStoreToOperand(fn, instr.Dst, "rcx")
}

func (e *x86_64Emitter) emitNASMStrIndex(fn *IRFunc, instr IRInstr) {
	w := e.b
	src := e.nasmLoadToReg(fn, instr.Src1, "r10")
	idx := e.nasmLoadToReg(fn, instr.Src2, "r11")
	w.WriteString(fmt.Sprintf("    mov rdi, %s\n", src))
	w.WriteString(fmt.Sprintf("    mov rsi, %s\n", idx))
	w.WriteString("    xor rax, rax\n")
	w.WriteString("    mov al, [rdi+rsi]\n")
	e.nasmStoreToOperand(fn, instr.Dst, "rax")
}

// emitNASMGetTimeNs emits inline NASM assembly to get time in nanoseconds.
// Windows x86_64: stub returning 0 (no raw syscall equivalent).
// Linux NASM: clock_gettime syscall 228.
func (e *x86_64Emitter) emitNASMGetTimeNs(fn *IRFunc, instr IRInstr) {
	w := e.b
	w.WriteString("    ; __time_ns: get current time in nanoseconds\n")
	w.WriteString("    sub rsp, 16\n") // allocate 16 bytes for struct

	if e.target.OS == OS_Linux {
		// clock_gettime(rdi=CLOCK_MONOTONIC=1, rsi=ptr), syscall 228
		w.WriteString("    mov rdi, 1\n")
		w.WriteString("    mov rsi, rsp\n")
		w.WriteString("    mov rax, 228\n")
		w.WriteString("    syscall\n")
		// rsp+0 = tv_sec, rsp+8 = tv_nsec
		w.WriteString("    mov r10, [rsp]\n")   // tv_sec
		w.WriteString("    mov r11, [rsp+8]\n") // tv_nsec
		w.WriteString("    imul r10, r10, 1000000000\n")
		w.WriteString("    add r10, r11\n")
	} else {
		// Windows or other: return 0 as stub
		w.WriteString("    xor r10, r10\n")
	}

	w.WriteString("    add rsp, 16\n") // deallocate
	e.nasmStoreToOperand(fn, instr.Dst, "r10")
}

func (e *x86_64Emitter) emitNASMStrConcat(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
	src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")

	id := e.uniqueID()
	copy1Label := fmt.Sprintf(".sc1_%d", id)
	copy2Label := fmt.Sprintf(".sc2_%d", id)
	doneLabel := fmt.Sprintf(".scd_%d", id)
	selALabel := fmt.Sprintf(".sca_%d", id)

	if src1 != "r10" {
		w.WriteString(fmt.Sprintf("    mov r10, %s\n", src1))
	}
	if src2 != "r11" {
		w.WriteString(fmt.Sprintf("    mov r11, %s\n", src2))
	}

	w.WriteString("    push rdi\n")
	w.WriteString("    push rcx\n")
	w.WriteString("    push rax\n")

	// Toggle buffer selector and pick destination buffer.
	w.WriteString("    lea rax, [rel _novus_strcat_sel]\n")
	w.WriteString("    xor byte [rax], 1\n")
	w.WriteString("    movzx eax, byte [rax]\n")
	w.WriteString("    lea rdi, [rel _novus_strcat_buf_a]\n")
	w.WriteString("    test eax, eax\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", selALabel))
	w.WriteString("    lea rdi, [rel _novus_strcat_buf_b]\n")
	w.WriteString(fmt.Sprintf("%s:\n", selALabel))
	w.WriteString("    push rdi\n")

	w.WriteString(fmt.Sprintf("%s:\n", copy1Label))
	w.WriteString("    mov cl, [r10]\n")
	w.WriteString("    test cl, cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", copy2Label))
	w.WriteString("    mov [rdi], cl\n")
	w.WriteString("    inc r10\n")
	w.WriteString("    inc rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy1Label))

	w.WriteString(fmt.Sprintf("%s:\n", copy2Label))
	w.WriteString("    mov cl, [r11]\n")
	w.WriteString("    mov [rdi], cl\n")
	w.WriteString("    test cl, cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", doneLabel))
	w.WriteString("    inc r11\n")
	w.WriteString("    inc rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy2Label))

	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	w.WriteString("    pop r10\n")
	w.WriteString("    pop rax\n")
	w.WriteString("    pop rcx\n")
	w.WriteString("    pop rdi\n")
	e.nasmStoreToOperand(fn, instr.Dst, "r10")
}

func (e *x86_64Emitter) usesStrConcat() bool {
	for _, fn := range e.mod.Functions {
		for _, instr := range fn.Instrs {
			if instr.Op == IRStrConcat {
				return true
			}
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// Shared utilities
// ---------------------------------------------------------------------------

func stripPercent(s string) string {
	return strings.TrimPrefix(s, "%")
}

func gasQuoteString(s string) string {
	var b strings.Builder
	b.WriteByte('"')
	for i := 0; i < len(s); i++ {
		ch := s[i]
		switch ch {
		case '\n':
			b.WriteString("\\n")
		case '\r':
			b.WriteString("\\r")
		case '\t':
			b.WriteString("\\t")
		case '\\':
			b.WriteString("\\\\")
		case '"':
			b.WriteString("\\\"")
		case 0:
			b.WriteString("\\0")
		default:
			if ch < 32 || ch > 126 {
				b.WriteString(fmt.Sprintf("\\%03o", ch))
			} else {
				b.WriteByte(ch)
			}
		}
	}
	b.WriteByte('"')
	return b.String()
}

func nasmQuoteString(s string) string {
	if len(s) == 0 {
		return `""`
	}
	var parts []string
	var current strings.Builder
	inString := false

	flush := func() {
		if inString && current.Len() > 0 {
			parts = append(parts, fmt.Sprintf(`"%s"`, current.String()))
			current.Reset()
			inString = false
		}
	}

	for i := 0; i < len(s); i++ {
		ch := s[i]
		if ch < 32 || ch > 126 || ch == '"' {
			flush()
			parts = append(parts, fmt.Sprintf("%d", ch))
		} else {
			if !inString {
				inString = true
			}
			current.WriteByte(ch)
		}
	}
	flush()

	return strings.Join(parts, ", ")
}
