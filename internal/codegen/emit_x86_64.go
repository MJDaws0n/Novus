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
	target.EnsureHeapDefaults()
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

	// --- Global variables in data section ---
	if len(e.mod.Globals) > 0 {
		if len(e.mod.Strings) == 0 {
			if e.target.OS == OS_Linux {
				w.WriteString(".data\n")
			} else {
				w.WriteString(".section __DATA,__data\n")
			}
		}
		for _, g := range e.mod.Globals {
			label := e.target.Sym(g.Name)
			w.WriteString(fmt.Sprintf(".p2align 3\n"))
			w.WriteString(fmt.Sprintf("%s:\n", label))
			if g.InitStr >= 0 {
				strLabel := e.target.Sym(e.mod.Strings[g.InitStr].Label)
				w.WriteString(fmt.Sprintf("    .quad %s\n", strLabel))
			} else {
				w.WriteString(fmt.Sprintf("    .quad %d\n", g.InitImm))
			}
		}
		w.WriteString("\n")
	}

	// --- BSS: bump-allocator heap ---
	if e.usesHeap() {
		heapSym := e.target.Sym("_novus_heap")
		heapPtrSym := e.target.Sym("_novus_heap_ptr")
		w.WriteString(fmt.Sprintf(".lcomm %s, %d\n", heapSym, e.target.HeapSize))
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n", heapPtrSym))

		// GC metadata.
		gcTable := e.target.Sym("_novus_gc_table")
		gcCount := e.target.Sym("_novus_gc_count")
		gcThreshold := e.target.Sym("_novus_gc_threshold")
		gcFreelist := e.target.Sym("_novus_gc_freelist")
		gcStackBot := e.target.Sym("_novus_gc_stack_bottom")
		w.WriteString(fmt.Sprintf(".lcomm %s, %d\n", gcTable, e.target.GCEntries*24))
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n", gcCount))
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n", gcThreshold))
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n", gcFreelist))
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n\n", gcStackBot))
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
	// The kernel places argc at (%rsp) and argv at 8(%rsp).
	// Load them into rdi/rsi (the SysV calling convention arg registers) before calling main.
	if e.target.OS == OS_Linux {
		w.WriteString(".globl _start\n")
		w.WriteString("_start:\n")
		if e.usesHeap() {
			gcStackBot := e.target.Sym("_novus_gc_stack_bottom")
			w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcStackBot))
			w.WriteString("    movq %rsp, (%rax)\n")
			gcThreshold := e.target.Sym("_novus_gc_threshold")
			w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcThreshold))
			w.WriteString("    movq $256, (%rax)\n")
		}
		w.WriteString("    movq (%rsp), %rdi\n")
		w.WriteString("    leaq 8(%rsp), %rsi\n")
		w.WriteString(fmt.Sprintf("    call %s\n", entryFuncName))
		w.WriteString("    movq %rax, %rdi\n")
		w.WriteString("    movq $60, %rax\n")
		w.WriteString("    syscall\n\n")
	}

	for _, fn := range e.mod.Functions {
		e.emitGASFunction(fn)
	}

	// Emit GC runtime functions.
	if e.usesHeap() {
		e.emitGASGCRuntime()
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

	// GC init in main function (macOS entry — no _start).
	if fn.Name == "main" && e.target.OS == OS_Darwin && e.usesHeap() {
		gcStackBot := e.target.Sym("_novus_gc_stack_bottom")
		w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcStackBot))
		w.WriteString("    leaq 16(%%rbp), %%rcx\n") // entry SP above saved rbp/ret
		w.WriteString("    movq %%rcx, (%%rax)\n")
		gcThreshold := e.target.Sym("_novus_gc_threshold")
		w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcThreshold))
		w.WriteString("    movq $256, (%rax)\n")
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
	case IRUDiv:
		e.emitGASUDiv(fn, instr)
	case IRMod:
		e.emitGASMod(fn, instr)
	case IRUMod:
		e.emitGASUMod(fn, instr)

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
	case IRShl:
		src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
		if src1 != "%r10" {
			w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src1))
		}
		if instr.Src2.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    shlq $%d, %%r10\n", instr.Src2.Imm))
		} else {
			src2 := e.gasLoadToReg(fn, instr.Src2, "%rcx")
			if src2 != "%rcx" {
				w.WriteString(fmt.Sprintf("    movq %s, %%rcx\n", src2))
			}
			w.WriteString("    shlq %cl, %r10\n")
		}
		e.gasStoreToOperand(fn, instr.Dst, "%r10")
	case IRShr:
		src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
		if src1 != "%r10" {
			w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src1))
		}
		if instr.Src2.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    sarq $%d, %%r10\n", instr.Src2.Imm))
		} else {
			src2 := e.gasLoadToReg(fn, instr.Src2, "%rcx")
			if src2 != "%rcx" {
				w.WriteString(fmt.Sprintf("    movq %s, %%rcx\n", src2))
			}
			w.WriteString("    sarq %cl, %r10\n")
		}
		e.gasStoreToOperand(fn, instr.Dst, "%r10")
	case IRBitNot:
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		if src != "%r10" {
			w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src))
		}
		w.WriteString("    notq %r10\n")
		e.gasStoreToOperand(fn, instr.Dst, "%r10")

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
		e.emitGASFlagOp(fn, instr)

	case IRStrConcat:
		e.emitGASStrConcat(fn, instr)
	case IRStrLen:
		e.emitGASStrLen(fn, instr)
	case IRStrIndex:
		e.emitGASStrIndex(fn, instr)
	case IRStrCmpEq:
		e.emitGASStrCmpEq(fn, instr)
	case IRStoreByte:
		addr := e.gasLoadToReg(fn, instr.Dst, "%r11")
		src := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    movb %sb, (%s)\n", src, addr))

	// Memory load (raw address read)
	case IRLoad8:
		addr := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    movzbl (%s), %%r11d\n", addr))
		e.gasStoreToOperand(fn, instr.Dst, "%r11")
	case IRLoad32:
		addr := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    movl (%s), %%r11d\n", addr))
		e.gasStoreToOperand(fn, instr.Dst, "%r11")
	case IRLoad64:
		addr := e.gasLoadToReg(fn, instr.Src1, "%r10")
		w.WriteString(fmt.Sprintf("    movq (%s), %%r11\n", addr))
		e.gasStoreToOperand(fn, instr.Dst, "%r11")

	// Array operations
	case IRArrayNew:
		e.emitGASArrayNew(fn, instr)
	case IRArrayGet:
		e.emitGASArrayGet(fn, instr)
	case IRArraySet:
		e.emitGASArraySet(fn, instr)
	case IRArrayAppend:
		e.emitGASArrayAppend(fn, instr)
	case IRArrayPop:
		e.emitGASArrayPop(fn, instr)
	case IRArrayLen:
		e.emitGASArrayLen(fn, instr)

	case IRWinCall:
		w.WriteString("    ## win_call: Windows API calls not supported on this target\n")

	case IRLoadGlobal:
		if instr.Src1.Kind == OpLabel {
			globalSym := e.target.Sym(instr.Src1.Label)
			if e.target.OS == OS_Darwin {
				w.WriteString(fmt.Sprintf("    movq %s@GOTPCREL(%%rip), %%r11\n", globalSym))
				w.WriteString("    movq (%r11), %r10\n")
			} else {
				w.WriteString(fmt.Sprintf("    movq %s(%%rip), %%r10\n", globalSym))
			}
			e.gasSpillIfNeeded(fn, instr.Dst, "%r10")
		}

	case IRStoreGlobal:
		if instr.Dst.Kind == OpLabel {
			globalSym := e.target.Sym(instr.Dst.Label)
			src := e.gasLoadToReg(fn, instr.Src1, "%r10")
			if e.target.OS == OS_Darwin {
				w.WriteString(fmt.Sprintf("    movq %s@GOTPCREL(%%rip), %%r11\n", globalSym))
				w.WriteString(fmt.Sprintf("    movq %s, (%%r11)\n", src))
			} else {
				w.WriteString(fmt.Sprintf("    movq %s, %s(%%rip)\n", src, globalSym))
			}
		}

	case IRData:
		// handled in data section

	case IRGCCollect:
		gcCollectSym := e.target.Sym("_novus_gc_collect")
		w.WriteString(fmt.Sprintf("    call %s\n", gcCollectSym))
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

func (e *x86_64Emitter) emitGASUDiv(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")
	w.WriteString(fmt.Sprintf("    movq %s, %%rax\n", src1))
	w.WriteString("    xorq %rdx, %rdx\n")
	if src2 == "%r11" || src2 == "%rcx" {
		w.WriteString(fmt.Sprintf("    divq %s\n", src2))
	} else {
		w.WriteString(fmt.Sprintf("    movq %s, %%rcx\n", src2))
		w.WriteString("    divq %rcx\n")
	}
	e.gasStoreToOperand(fn, instr.Dst, "%rax")
}

func (e *x86_64Emitter) emitGASUMod(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")
	w.WriteString(fmt.Sprintf("    movq %s, %%rax\n", src1))
	w.WriteString("    xorq %rdx, %rdx\n")
	if src2 == "%r11" || src2 == "%rcx" {
		w.WriteString(fmt.Sprintf("    divq %s\n", src2))
	} else {
		w.WriteString(fmt.Sprintf("    movq %s, %%rcx\n", src2))
		w.WriteString("    divq %rcx\n")
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

func (e *x86_64Emitter) emitGASStrConcat(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")

	id := e.uniqueID()
	len1Label := fmt.Sprintf(".Lscl1_%d", id)
	len1Done := fmt.Sprintf(".Lscl1d_%d", id)
	len2Label := fmt.Sprintf(".Lscl2_%d", id)
	len2Done := fmt.Sprintf(".Lscl2d_%d", id)
	copy1Label := fmt.Sprintf(".Lsc1_%d", id)
	copy2Label := fmt.Sprintf(".Lsc2_%d", id)
	doneLabel := fmt.Sprintf(".Lscd_%d", id)

	gcAllocSym := e.target.Sym("_novus_gc_alloc")

	if src1 != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src1))
	}
	if src2 != "%r11" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r11\n", src2))
	}

	// Compute strlen(s1) → rcx.
	w.WriteString("    xorq %rcx, %rcx\n")
	w.WriteString(fmt.Sprintf("%s:\n", len1Label))
	w.WriteString("    cmpb $0, (%r10,%rcx)\n")
	w.WriteString(fmt.Sprintf("    je %s\n", len1Done))
	w.WriteString("    incq %rcx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", len1Label))
	w.WriteString(fmt.Sprintf("%s:\n", len1Done))
	w.WriteString("    pushq %rcx\n") // save len1

	// Compute strlen(s2) → rdx.
	w.WriteString("    xorq %rdx, %rdx\n")
	w.WriteString(fmt.Sprintf("%s:\n", len2Label))
	w.WriteString("    cmpb $0, (%r11,%rdx)\n")
	w.WriteString(fmt.Sprintf("    je %s\n", len2Done))
	w.WriteString("    incq %rdx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", len2Label))
	w.WriteString(fmt.Sprintf("%s:\n", len2Done))

	// Allocate len1 + len2 + 1 via GC allocator.
	w.WriteString("    popq %rcx\n") // restore len1
	w.WriteString("    pushq %r10\n") // save s1
	w.WriteString("    pushq %r11\n") // save s2
	w.WriteString("    pushq %rcx\n") // save len1
	w.WriteString("    pushq %rdx\n") // save len2
	w.WriteString("    leaq 1(%rcx,%rdx), %rdi\n") // rdi = len1+len2+1
	w.WriteString(fmt.Sprintf("    call %s\n", gcAllocSym))
	w.WriteString("    popq %rdx\n")  // restore len2
	w.WriteString("    popq %rcx\n")  // restore len1
	w.WriteString("    popq %r11\n")  // restore s2
	w.WriteString("    popq %r10\n")  // restore s1
	// rax = allocated buffer.
	w.WriteString("    movq %rax, %rdi\n") // rdi = write cursor

	// Copy s1.
	w.WriteString(fmt.Sprintf("%s:\n", copy1Label))
	w.WriteString("    movb (%r10), %cl\n")
	w.WriteString("    testb %cl, %cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", copy2Label))
	w.WriteString("    movb %cl, (%rdi)\n")
	w.WriteString("    incq %r10\n")
	w.WriteString("    incq %rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy1Label))

	// Copy s2 (including null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy2Label))
	w.WriteString("    movb (%r11), %cl\n")
	w.WriteString("    movb %cl, (%rdi)\n")
	w.WriteString("    testb %cl, %cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", doneLabel))
	w.WriteString("    incq %r11\n")
	w.WriteString("    incq %rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy2Label))

	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	e.gasStoreToOperand(fn, instr.Dst, "%rax")
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

// emitGASStrCmpEq emits an inline byte-by-byte string comparison (GAS/AT&T).
// dst = 1 if strings are equal, 0 otherwise.
func (e *x86_64Emitter) emitGASStrCmpEq(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.gasLoadToReg(fn, instr.Src1, "%r10")
	src2 := e.gasLoadToReg(fn, instr.Src2, "%r11")

	if src1 != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", src1))
	}
	if src2 != "%r11" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r11\n", src2))
	}

	id := e.uniqueID()
	loopLabel := fmt.Sprintf(".Lstrcmp_loop_%d", id)
	neLabel := fmt.Sprintf(".Lstrcmp_ne_%d", id)
	eqLabel := fmt.Sprintf(".Lstrcmp_eq_%d", id)
	endLabel := fmt.Sprintf(".Lstrcmp_end_%d", id)

	w.WriteString(fmt.Sprintf("%s:\n", loopLabel))
	w.WriteString("    movzbl (%r10), %eax\n")
	w.WriteString("    movzbl (%r11), %ecx\n")
	w.WriteString("    cmpb %cl, %al\n")
	w.WriteString(fmt.Sprintf("    jne %s\n", neLabel))
	w.WriteString("    testb %al, %al\n")
	w.WriteString(fmt.Sprintf("    je %s\n", eqLabel))
	w.WriteString("    incq %r10\n")
	w.WriteString("    incq %r11\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", loopLabel))
	w.WriteString(fmt.Sprintf("%s:\n", neLabel))
	w.WriteString("    xorq %rax, %rax\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", endLabel))
	w.WriteString(fmt.Sprintf("%s:\n", eqLabel))
	w.WriteString("    movq $1, %rax\n")
	w.WriteString(fmt.Sprintf("%s:\n", endLabel))
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

	// BSS: bump-allocator heap.
	if e.usesHeap() || e.needsWinMainArgs() {
		w.WriteString("section .bss\n")
		if e.usesHeap() {
			w.WriteString(fmt.Sprintf("    _novus_heap: resb %d\n", e.target.HeapSize))
			w.WriteString("    _novus_heap_ptr: resq 1\n")
			// GC metadata.
			w.WriteString(fmt.Sprintf("    _novus_gc_table: resb %d\n", e.target.GCEntries*24))
			w.WriteString("    _novus_gc_count: resq 1\n")
			w.WriteString("    _novus_gc_threshold: resq 1\n")
			w.WriteString("    _novus_gc_freelist: resq 1\n")
			w.WriteString("    _novus_gc_stack_bottom: resq 1\n")
		}
		if e.needsWinMainArgs() {
			w.WriteString("    _novus_win_argc: resd 1\n")
			w.WriteString("    _novus_win_argv: resq 1\n")
			w.WriteString("    _novus_win_envp: resq 1\n")
			w.WriteString("    _novus_win_newmode: resd 1\n")
		}
		w.WriteString("\n")
	}

	// Global variables in data section.
	if len(e.mod.Globals) > 0 {
		if len(e.mod.Strings) == 0 {
			w.WriteString("section .data\n")
		}
		for _, g := range e.mod.Globals {
			if g.InitStr >= 0 {
				strLabel := e.mod.Strings[g.InitStr].Label
				w.WriteString(fmt.Sprintf("    %s: dq %s\n", g.Name, strLabel))
			} else {
				w.WriteString(fmt.Sprintf("    %s: dq %d\n", g.Name, g.InitImm))
			}
		}
		w.WriteString("\n")
	}

	// Collect all Windows API functions used by win_call and emit extern declarations.
	winExterns := e.collectWinCallExterns()
	if e.needsWinMainArgs() {
		// Add __getmainargs from msvcrt.dll for Windows argc/argv setup.
		found := false
		for _, name := range winExterns {
			if name == "__getmainargs" {
				found = true
				break
			}
		}
		if !found {
			winExterns = append(winExterns, "__getmainargs")
		}
	}
	if len(winExterns) > 0 {
		for _, name := range winExterns {
			w.WriteString(fmt.Sprintf("extern %s\n", name))
		}
		w.WriteString("\n")
	}

	w.WriteString("section .text\n")
	for _, fn := range e.mod.Functions {
		w.WriteString(fmt.Sprintf("    global %s\n", fn.Name))
	}
	w.WriteString("\n")

	for _, fn := range e.mod.Functions {
		e.emitNASMFunction(fn)
	}

	// Emit GC runtime for NASM.
	if e.usesHeap() {
		e.emitNASMGCRuntime()
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

	// GC init in main function.
	if fn.Name == "main" && e.usesHeap() {
		w.WriteString("    lea rax, [rel _novus_gc_stack_bottom]\n")
		w.WriteString("    lea rcx, [rbp+16]\n") // entry SP above saved rbp/ret
		w.WriteString("    mov [rax], rcx\n")
		w.WriteString("    lea rax, [rel _novus_gc_threshold]\n")
		w.WriteString("    mov qword [rax], 256\n")
	}

	// On Windows, the entry point doesn't receive argc/argv in registers.
	// Call __getmainargs from msvcrt.dll to populate them.
	if fn.Name == "main" && fn.ParamCount >= 2 && e.target.OS == OS_Windows {
		w.WriteString("    ; --- Windows argc/argv setup via __getmainargs ---\n")
		w.WriteString("    sub rsp, 48\n")                                     // shadow space (32) + 5th arg (8) + align (8)
		w.WriteString("    lea rcx, [rel _novus_win_argc]\n")                  // &argc
		w.WriteString("    lea rdx, [rel _novus_win_argv]\n")                  // &argv
		w.WriteString("    lea r8, [rel _novus_win_envp]\n")                   // &envp
		w.WriteString("    xor r9, r9\n")                                      // expand_wildcards = 0
		w.WriteString("    lea rax, [rel _novus_win_newmode]\n")               // &new_mode
		w.WriteString("    mov qword [rsp+32], rax\n")                         // 5th arg on stack
		w.WriteString("    call __getmainargs\n")
		w.WriteString("    add rsp, 48\n")
		w.WriteString("    mov ecx, dword [rel _novus_win_argc]\n")            // argc -> rcx
		w.WriteString("    mov rdx, qword [rel _novus_win_argv]\n")            // argv -> rdx
		w.WriteString("    ; --- End Windows argc/argv setup ---\n")
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
	case IRUDiv:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
		w.WriteString(fmt.Sprintf("    mov rax, %s\n", src1))
		w.WriteString("    xor rdx, rdx\n")
		w.WriteString(fmt.Sprintf("    mov rcx, %s\n", src2))
		w.WriteString("    div rcx\n")
		e.nasmStoreToOperand(fn, instr.Dst, "rax")
	case IRMod:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
		w.WriteString(fmt.Sprintf("    mov rax, %s\n", src1))
		w.WriteString("    cqo\n")
		w.WriteString(fmt.Sprintf("    mov rcx, %s\n", src2))
		w.WriteString("    idiv rcx\n")
		e.nasmStoreToOperand(fn, instr.Dst, "rdx")
	case IRUMod:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")
		w.WriteString(fmt.Sprintf("    mov rax, %s\n", src1))
		w.WriteString("    xor rdx, rdx\n")
		w.WriteString(fmt.Sprintf("    mov rcx, %s\n", src2))
		w.WriteString("    div rcx\n")
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
	case IRShl:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		if src1 != "r10" {
			w.WriteString(fmt.Sprintf("    mov r10, %s\n", src1))
		}
		if instr.Src2.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    shl r10, %d\n", instr.Src2.Imm))
		} else {
			src2 := e.nasmLoadToReg(fn, instr.Src2, "rcx")
			if src2 != "rcx" {
				w.WriteString(fmt.Sprintf("    mov rcx, %s\n", src2))
			}
			w.WriteString("    shl r10, cl\n")
		}
		e.nasmStoreToOperand(fn, instr.Dst, "r10")
	case IRShr:
		src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
		if src1 != "r10" {
			w.WriteString(fmt.Sprintf("    mov r10, %s\n", src1))
		}
		if instr.Src2.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    sar r10, %d\n", instr.Src2.Imm))
		} else {
			src2 := e.nasmLoadToReg(fn, instr.Src2, "rcx")
			if src2 != "rcx" {
				w.WriteString(fmt.Sprintf("    mov rcx, %s\n", src2))
			}
			w.WriteString("    sar r10, cl\n")
		}
		e.nasmStoreToOperand(fn, instr.Dst, "r10")
	case IRBitNot:
		src := e.nasmLoadToReg(fn, instr.Src1, "r10")
		if src != "r10" {
			w.WriteString(fmt.Sprintf("    mov r10, %s\n", src))
		}
		w.WriteString("    not r10\n")
		e.nasmStoreToOperand(fn, instr.Dst, "r10")

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
		e.emitNASMFlagOp(fn, instr)

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
	case IRStrCmpEq:
		e.emitNASMStrCmpEq(fn, instr)

	// Memory load (raw address read)
	case IRLoad8:
		addr := e.nasmLoadToReg(fn, instr.Src1, "r10")
		w.WriteString(fmt.Sprintf("    movzx r11d, byte [%s]\n", addr))
		e.nasmStoreToOperand(fn, instr.Dst, "r11")
	case IRLoad32:
		addr := e.nasmLoadToReg(fn, instr.Src1, "r10")
		w.WriteString(fmt.Sprintf("    mov r11d, dword [%s]\n", addr))
		e.nasmStoreToOperand(fn, instr.Dst, "r11")
	case IRLoad64:
		addr := e.nasmLoadToReg(fn, instr.Src1, "r10")
		w.WriteString(fmt.Sprintf("    mov r11, qword [%s]\n", addr))
		e.nasmStoreToOperand(fn, instr.Dst, "r11")

	// Array operations
	case IRArrayNew:
		e.emitNASMArrayNew(fn, instr)
	case IRArrayGet:
		e.emitNASMArrayGet(fn, instr)
	case IRArraySet:
		e.emitNASMArraySet(fn, instr)
	case IRArrayAppend:
		e.emitNASMArrayAppend(fn, instr)
	case IRArrayPop:
		e.emitNASMArrayPop(fn, instr)
	case IRArrayLen:
		e.emitNASMArrayLen(fn, instr)

	// Windows API call
	case IRWinCall:
		e.emitNASMWinCall(fn, instr)

	case IRLoadGlobal:
		if instr.Src1.Kind == OpLabel {
			globalSym := instr.Src1.Label
			w.WriteString(fmt.Sprintf("    mov r10, [rel %s]\n", globalSym))
			e.nasmSpillIfNeeded(fn, instr.Dst, "r10")
		}

	case IRStoreGlobal:
		if instr.Dst.Kind == OpLabel {
			globalSym := instr.Dst.Label
			src := e.nasmLoadToReg(fn, instr.Src1, "r10")
			w.WriteString(fmt.Sprintf("    mov [rel %s], %s\n", globalSym, src))
		}

	case IRGCCollect:
		w.WriteString("    call _novus_gc_collect\n")
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

	alignPad := 0
	if len(instr.Args) > len(argRegs) {
		// On Windows, ensure 16-byte alignment: if odd number of stack args, add padding.
		extraArgs := len(instr.Args) - len(argRegs)
		if e.target.OS == OS_Windows && extraArgs%2 != 0 {
			w.WriteString("    sub rsp, 8\n") // alignment padding
			alignPad = 8
		}
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

	if stackArgs > 0 || alignPad > 0 {
		w.WriteString(fmt.Sprintf("    add rsp, %d\n", stackArgs*8+alignPad))
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

// emitNASMStrCmpEq emits an inline byte-by-byte string comparison (NASM/Intel).
// dst = 1 if strings are equal, 0 otherwise.
func (e *x86_64Emitter) emitNASMStrCmpEq(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
	src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")

	if src1 != "r10" {
		w.WriteString(fmt.Sprintf("    mov r10, %s\n", src1))
	}
	if src2 != "r11" {
		w.WriteString(fmt.Sprintf("    mov r11, %s\n", src2))
	}

	id := e.uniqueID()
	loopLabel := fmt.Sprintf(".strcmp_loop_%d", id)
	neLabel := fmt.Sprintf(".strcmp_ne_%d", id)
	eqLabel := fmt.Sprintf(".strcmp_eq_%d", id)
	endLabel := fmt.Sprintf(".strcmp_end_%d", id)

	w.WriteString(fmt.Sprintf("%s:\n", loopLabel))
	w.WriteString("    movzx eax, byte [r10]\n")
	w.WriteString("    movzx ecx, byte [r11]\n")
	w.WriteString("    cmp al, cl\n")
	w.WriteString(fmt.Sprintf("    jne %s\n", neLabel))
	w.WriteString("    test al, al\n")
	w.WriteString(fmt.Sprintf("    je %s\n", eqLabel))
	w.WriteString("    inc r10\n")
	w.WriteString("    inc r11\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", loopLabel))
	w.WriteString(fmt.Sprintf("%s:\n", neLabel))
	w.WriteString("    xor rax, rax\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", endLabel))
	w.WriteString(fmt.Sprintf("%s:\n", eqLabel))
	w.WriteString("    mov rax, 1\n")
	w.WriteString(fmt.Sprintf("%s:\n", endLabel))
	e.nasmStoreToOperand(fn, instr.Dst, "rax")
}

func (e *x86_64Emitter) emitNASMStrConcat(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.nasmLoadToReg(fn, instr.Src1, "r10")
	src2 := e.nasmLoadToReg(fn, instr.Src2, "r11")

	id := e.uniqueID()
	len1Label := fmt.Sprintf(".scl1_%d", id)
	len1Done := fmt.Sprintf(".scl1d_%d", id)
	len2Label := fmt.Sprintf(".scl2_%d", id)
	len2Done := fmt.Sprintf(".scl2d_%d", id)
	copy1Label := fmt.Sprintf(".sc1_%d", id)
	copy2Label := fmt.Sprintf(".sc2_%d", id)
	doneLabel := fmt.Sprintf(".scd_%d", id)

	if src1 != "r10" {
		w.WriteString(fmt.Sprintf("    mov r10, %s\n", src1))
	}
	if src2 != "r11" {
		w.WriteString(fmt.Sprintf("    mov r11, %s\n", src2))
	}

	// Compute strlen(s1) → rcx.
	w.WriteString("    xor rcx, rcx\n")
	w.WriteString(fmt.Sprintf("%s:\n", len1Label))
	w.WriteString("    cmp byte [r10+rcx], 0\n")
	w.WriteString(fmt.Sprintf("    je %s\n", len1Done))
	w.WriteString("    inc rcx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", len1Label))
	w.WriteString(fmt.Sprintf("%s:\n", len1Done))
	w.WriteString("    push rcx\n") // save len1

	// Compute strlen(s2) → rdx.
	w.WriteString("    xor rdx, rdx\n")
	w.WriteString(fmt.Sprintf("%s:\n", len2Label))
	w.WriteString("    cmp byte [r11+rdx], 0\n")
	w.WriteString(fmt.Sprintf("    je %s\n", len2Done))
	w.WriteString("    inc rdx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", len2Label))
	w.WriteString(fmt.Sprintf("%s:\n", len2Done))

	// Allocate via GC allocator. On Windows, first arg is rcx.
	w.WriteString("    pop rcx\n")   // len1
	w.WriteString("    push r10\n")  // save s1
	w.WriteString("    push r11\n")  // save s2
	w.WriteString("    push rcx\n")  // save len1
	w.WriteString("    push rdx\n")  // save len2
	w.WriteString("    lea rcx, [rcx+rdx+1]\n") // size = len1+len2+1
	w.WriteString("    sub rsp, 32\n") // shadow space
	w.WriteString("    call _novus_gc_alloc\n")
	w.WriteString("    add rsp, 32\n")
	w.WriteString("    pop rdx\n")
	w.WriteString("    pop rcx\n")
	w.WriteString("    pop r11\n")
	w.WriteString("    pop r10\n")
	// rax = buffer.
	w.WriteString("    mov rdi, rax\n") // write cursor

	// Copy s1.
	w.WriteString(fmt.Sprintf("%s:\n", copy1Label))
	w.WriteString("    mov cl, [r10]\n")
	w.WriteString("    test cl, cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", copy2Label))
	w.WriteString("    mov [rdi], cl\n")
	w.WriteString("    inc r10\n")
	w.WriteString("    inc rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy1Label))

	// Copy s2 (including null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy2Label))
	w.WriteString("    mov cl, [r11]\n")
	w.WriteString("    mov [rdi], cl\n")
	w.WriteString("    test cl, cl\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", doneLabel))
	w.WriteString("    inc r11\n")
	w.WriteString("    inc rdi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy2Label))

	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	e.nasmStoreToOperand(fn, instr.Dst, "rax")
}

// ---------------------------------------------------------------------------
// GAS array operations (x86_64)
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) emitGASArrayNew(fn *IRFunc, instr IRInstr) {
	w := e.b
	cap := instr.Src2.Imm
	if cap < 4 {
		cap = 4
	}
	gcAllocSym := e.target.Sym("_novus_gc_alloc")

	// Allocate header (24 bytes) + data (cap*8 bytes) via GC allocator.
	totalSize := 24 + cap*8
	w.WriteString(fmt.Sprintf("    movq $%d, %%rdi\n", totalSize))
	w.WriteString(fmt.Sprintf("    call %s\n", gcAllocSym))
	// rax = allocated block.
	w.WriteString("    leaq 24(%rax), %r10\n")                       // r10 = data start
	w.WriteString("    movq %r10, (%rax)\n")                         // header[0] = data_ptr
	w.WriteString("    movq $0, 8(%rax)\n")                          // header[8] = len = 0
	w.WriteString(fmt.Sprintf("    movq $%d, 16(%%rax)\n", cap))    // header[16] = cap
	e.gasStoreToOperand(fn, instr.Dst, "%rax")
}

func (e *x86_64Emitter) emitGASArrayGet(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.gasLoadToReg(fn, instr.Src1, "%r10")
	idx := e.gasLoadToReg(fn, instr.Src2, "%r11")
	w.WriteString(fmt.Sprintf("    movq (%s), %%r10\n", arrPtr)) // data_ptr
	w.WriteString(fmt.Sprintf("    movq (%%r10,%s,8), %%r10\n", idx))
	e.gasStoreToOperand(fn, instr.Dst, "%r10")
}

func (e *x86_64Emitter) emitGASArraySet(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.gasLoadToReg(fn, instr.Dst, "%r10")
	idx := e.gasLoadToReg(fn, instr.Src1, "%r11")
	val := e.gasLoadToReg(fn, instr.Src2, "%rcx")
	w.WriteString(fmt.Sprintf("    movq (%s), %%r10\n", arrPtr))
	w.WriteString(fmt.Sprintf("    movq %s, (%%r10,%s,8)\n", val, idx))
}

func (e *x86_64Emitter) emitGASArrayAppend(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.gasLoadToReg(fn, instr.Dst, "%r10")
	val := e.gasLoadToReg(fn, instr.Src1, "%r11")

	id := e.uniqueID()
	noGrowLabel := fmt.Sprintf(".Laang_%d", id)
	capOkLabel := fmt.Sprintf(".Laaco_%d", id)
	copyLabel := fmt.Sprintf(".Laacp_%d", id)
	copyDoneLabel := fmt.Sprintf(".Laacd_%d", id)
	gcAllocSym := e.target.Sym("_novus_gc_alloc")

	if arrPtr != "%r10" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r10\n", arrPtr))
	}
	if val != "%r11" {
		w.WriteString(fmt.Sprintf("    movq %s, %%r11\n", val))
	}
	// Load len and cap.
	w.WriteString("    movq 8(%r10), %rcx\n")  // rcx = len
	w.WriteString("    movq 16(%r10), %rdi\n") // rdi = cap
	w.WriteString("    cmpq %rdi, %rcx\n")
	w.WriteString(fmt.Sprintf("    jl %s\n", noGrowLabel))

	// --- GROW ---
	w.WriteString("    shlq $1, %rdi\n")
	w.WriteString("    cmpq $4, %rdi\n")
	w.WriteString(fmt.Sprintf("    jge %s\n", capOkLabel))
	w.WriteString("    movq $4, %rdi\n")
	w.WriteString(fmt.Sprintf("%s:\n", capOkLabel))

	// Save r10(arrPtr), r11(val), rcx(len), rdi(new_cap).
	w.WriteString("    pushq %r10\n")
	w.WriteString("    pushq %r11\n")
	w.WriteString("    pushq %rcx\n")
	w.WriteString("    pushq %rdi\n")

	// Allocate new_cap * 8 bytes via GC allocator.
	w.WriteString("    shlq $3, %rdi\n") // rdi = new_cap * 8 (rdi is first arg)
	w.WriteString(fmt.Sprintf("    call %s\n", gcAllocSym))
	// rax = new data block.

	w.WriteString("    popq %rdi\n")  // new_cap
	w.WriteString("    popq %rcx\n")  // len
	w.WriteString("    popq %r11\n")  // val
	w.WriteString("    movq %rax, %r8\n") // r8 = new data
	w.WriteString("    popq %r10\n")  // arrPtr

	// Copy old data to new data.
	w.WriteString("    pushq %rsi\n")
	w.WriteString("    movq (%r10), %rsi\n") // rsi = old data_ptr
	w.WriteString("    pushq %rdx\n")
	w.WriteString("    xorq %rdx, %rdx\n")
	w.WriteString(fmt.Sprintf("%s:\n", copyLabel))
	w.WriteString("    cmpq %rcx, %rdx\n")
	w.WriteString(fmt.Sprintf("    jge %s\n", copyDoneLabel))
	w.WriteString("    movq (%rsi,%rdx,8), %r9\n")
	w.WriteString("    movq %r9, (%r8,%rdx,8)\n")
	w.WriteString("    incq %rdx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copyLabel))
	w.WriteString(fmt.Sprintf("%s:\n", copyDoneLabel))
	w.WriteString("    popq %rdx\n")
	w.WriteString("    popq %rsi\n")

	// Update header.
	w.WriteString("    movq %r8, (%r10)\n")
	w.WriteString("    movq %rdi, 16(%r10)\n")

	// --- NO GROW ---
	w.WriteString(fmt.Sprintf("%s:\n", noGrowLabel))
	w.WriteString("    movq (%r10), %rdi\n")
	w.WriteString("    movq 8(%r10), %rcx\n")
	w.WriteString("    movq %r11, (%rdi,%rcx,8)\n")
	w.WriteString("    incq %rcx\n")
	w.WriteString("    movq %rcx, 8(%r10)\n")
}

func (e *x86_64Emitter) emitGASArrayPop(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.gasLoadToReg(fn, instr.Src1, "%r10")
	w.WriteString(fmt.Sprintf("    movq 8(%s), %%r11\n", arrPtr)) // len
	w.WriteString("    decq %r11\n")
	w.WriteString(fmt.Sprintf("    movq %%r11, 8(%s)\n", arrPtr)) // save len
	w.WriteString(fmt.Sprintf("    movq (%s), %%r10\n", arrPtr))  // data_ptr
	w.WriteString("    movq (%r10,%r11,8), %r10\n")
	e.gasStoreToOperand(fn, instr.Dst, "%r10")
}

func (e *x86_64Emitter) emitGASArrayLen(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.gasLoadToReg(fn, instr.Src1, "%r10")
	w.WriteString(fmt.Sprintf("    movq 8(%s), %%r10\n", arrPtr))
	e.gasStoreToOperand(fn, instr.Dst, "%r10")
}

// ---------------------------------------------------------------------------
// NASM array operations (x86_64)
// ---------------------------------------------------------------------------

func (e *x86_64Emitter) emitNASMArrayNew(fn *IRFunc, instr IRInstr) {
	w := e.b
	cap := instr.Src2.Imm
	if cap < 4 {
		cap = 4
	}

	// Allocate header (24 bytes) + data (cap*8 bytes) via GC allocator.
	totalSize := 24 + cap*8
	w.WriteString(fmt.Sprintf("    mov rcx, %d\n", totalSize)) // Windows: first arg in rcx
	w.WriteString("    sub rsp, 32\n") // shadow space
	w.WriteString("    call _novus_gc_alloc\n")
	w.WriteString("    add rsp, 32\n")
	// rax = header pointer.
	w.WriteString("    lea r10, [rax+24]\n") // data starts after header
	w.WriteString("    mov [rax], r10\n")    // header.data_ptr
	w.WriteString("    mov qword [rax+8], 0\n")  // header.len = 0
	w.WriteString(fmt.Sprintf("    mov qword [rax+16], %d\n", cap)) // header.cap
	e.nasmStoreToOperand(fn, instr.Dst, "rax")
}

func (e *x86_64Emitter) emitNASMArrayGet(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.nasmLoadToReg(fn, instr.Src1, "r10")
	idx := e.nasmLoadToReg(fn, instr.Src2, "r11")
	w.WriteString(fmt.Sprintf("    mov r10, [%s]\n", arrPtr))
	w.WriteString(fmt.Sprintf("    mov r10, [r10+%s*8]\n", idx))
	e.nasmStoreToOperand(fn, instr.Dst, "r10")
}

func (e *x86_64Emitter) emitNASMArraySet(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.nasmLoadToReg(fn, instr.Dst, "r10")
	idx := e.nasmLoadToReg(fn, instr.Src1, "r11")
	val := e.nasmLoadToReg(fn, instr.Src2, "rcx")
	w.WriteString(fmt.Sprintf("    mov r10, [%s]\n", arrPtr))
	w.WriteString(fmt.Sprintf("    mov [r10+%s*8], %s\n", idx, val))
}

func (e *x86_64Emitter) emitNASMArrayAppend(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.nasmLoadToReg(fn, instr.Dst, "r10")
	val := e.nasmLoadToReg(fn, instr.Src1, "r11")

	id := e.uniqueID()
	noGrowLabel := fmt.Sprintf(".aang_%d", id)
	capOkLabel := fmt.Sprintf(".aaco_%d", id)
	copyLabel := fmt.Sprintf(".aacp_%d", id)
	copyDoneLabel := fmt.Sprintf(".aacd_%d", id)

	if arrPtr != "r10" {
		w.WriteString(fmt.Sprintf("    mov r10, %s\n", arrPtr))
	}
	if val != "r11" {
		w.WriteString(fmt.Sprintf("    mov r11, %s\n", val))
	}
	w.WriteString("    push rdi\n")
	w.WriteString("    push rcx\n")
	// Load len and cap.
	w.WriteString("    mov rcx, [r10+8]\n")  // rcx = len
	w.WriteString("    mov rdi, [r10+16]\n") // rdi = cap
	w.WriteString("    cmp rcx, rdi\n")
	w.WriteString(fmt.Sprintf("    jl %s\n", noGrowLabel))

	// --- GROW ---
	w.WriteString("    shl rdi, 1\n")
	w.WriteString("    cmp rdi, 4\n")
	w.WriteString(fmt.Sprintf("    jge %s\n", capOkLabel))
	w.WriteString("    mov rdi, 4\n")
	w.WriteString(fmt.Sprintf("%s:\n", capOkLabel))

	// Save context for gc_alloc call.
	w.WriteString("    push r10\n") // arrPtr
	w.WriteString("    push r11\n") // val
	w.WriteString("    push rcx\n") // len
	w.WriteString("    push rdi\n") // new_cap

	// Allocate new data block: new_cap * 8 via GC allocator.
	w.WriteString("    mov rcx, rdi\n")    // rcx = new_cap (Windows first arg)
	w.WriteString("    shl rcx, 3\n")      // rcx = new_cap * 8
	w.WriteString("    sub rsp, 32\n")     // shadow space
	w.WriteString("    call _novus_gc_alloc\n")
	w.WriteString("    add rsp, 32\n")
	// rax = new data block.

	// Restore saved values.
	w.WriteString("    pop rdi\n")  // new_cap
	w.WriteString("    pop rcx\n")  // len
	w.WriteString("    pop r11\n")  // val
	w.WriteString("    mov r8, rax\n") // r8 = new data
	w.WriteString("    pop r10\n")  // arrPtr

	// Copy old data to new data.
	w.WriteString("    push rsi\n")
	w.WriteString("    mov rsi, [r10]\n") // rsi = old data_ptr
	w.WriteString("    push rdx\n")
	w.WriteString("    xor rdx, rdx\n") // i = 0
	w.WriteString(fmt.Sprintf("%s:\n", copyLabel))
	w.WriteString("    cmp rdx, rcx\n")
	w.WriteString(fmt.Sprintf("    jge %s\n", copyDoneLabel))
	w.WriteString("    mov r9, [rsi+rdx*8]\n")
	w.WriteString("    mov [r8+rdx*8], r9\n")
	w.WriteString("    inc rdx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copyLabel))
	w.WriteString(fmt.Sprintf("%s:\n", copyDoneLabel))
	w.WriteString("    pop rdx\n")
	w.WriteString("    pop rsi\n")

	// Update header.
	w.WriteString("    mov [r10], r8\n")
	w.WriteString("    mov [r10+16], rdi\n")

	// --- NO GROW ---
	w.WriteString(fmt.Sprintf("%s:\n", noGrowLabel))
	w.WriteString("    mov rdi, [r10]\n")        // data_ptr
	w.WriteString("    mov rcx, [r10+8]\n")      // len (reload)
	w.WriteString("    mov [rdi+rcx*8], r11\n")  // data[len] = val
	w.WriteString("    inc rcx\n")
	w.WriteString("    mov [r10+8], rcx\n")      // len++
	w.WriteString("    pop rcx\n")
	w.WriteString("    pop rdi\n")
}

func (e *x86_64Emitter) emitNASMArrayPop(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.nasmLoadToReg(fn, instr.Src1, "r10")
	w.WriteString(fmt.Sprintf("    mov r11, [%s+8]\n", arrPtr))
	w.WriteString("    dec r11\n")
	w.WriteString(fmt.Sprintf("    mov [%s+8], r11\n", arrPtr))
	w.WriteString(fmt.Sprintf("    mov r10, [%s]\n", arrPtr))
	w.WriteString("    mov r10, [r10+r11*8]\n")
	e.nasmStoreToOperand(fn, instr.Dst, "r10")
}

func (e *x86_64Emitter) emitNASMArrayLen(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.nasmLoadToReg(fn, instr.Src1, "r10")
	w.WriteString(fmt.Sprintf("    mov r10, [%s+8]\n", arrPtr))
	e.nasmStoreToOperand(fn, instr.Dst, "r10")
}

// collectWinCallExterns scans all IR functions for IRWinCall instructions
// and returns a deduplicated list of Windows API function names to extern.
func (e *x86_64Emitter) collectWinCallExterns() []string {
	seen := map[string]bool{}
	var result []string
	for _, fn := range e.mod.Functions {
		for _, instr := range fn.Instrs {
			if instr.Op == IRWinCall && instr.Src1.Kind == OpLabel {
				name := instr.Src1.Label
				if !seen[name] {
					seen[name] = true
					result = append(result, name)
				}
			}
		}
	}
	return result
}

// emitNASMWinCall emits a Windows API call using the x64 calling convention:
//   - First 4 args in rcx, rdx, r8, r9
//   - 32-byte shadow space always allocated
//   - Additional args pushed right-to-left on stack
//   - Stack aligned to 16 bytes before call
//   - Return value in rax
func (e *x86_64Emitter) emitNASMWinCall(fn *IRFunc, instr IRInstr) {
	w := e.b
	apiName := instr.Src1.Label
	winArgRegs := []string{"rcx", "rdx", "r8", "r9"}

	w.WriteString(fmt.Sprintf("    ; win_call %s (%d args)\n", apiName, len(instr.Args)))

	// Push any extra stack arguments (beyond 4) right-to-left.
	stackArgs := 0
	if len(instr.Args) > 4 {
		// Ensure 16-byte alignment: if odd number of stack args, add padding.
		extraArgs := len(instr.Args) - 4
		if extraArgs%2 != 0 {
			w.WriteString("    sub rsp, 8\n") // alignment padding
			stackArgs++
		}
		for i := len(instr.Args) - 1; i >= 4; i-- {
			src := e.nasmLoadToReg(fn, instr.Args[i], "r10")
			w.WriteString(fmt.Sprintf("    push %s\n", src))
			stackArgs++
		}
	}

	// Move first 4 args into register args.
	for i := 0; i < len(instr.Args) && i < 4; i++ {
		src := e.nasmLoadToReg(fn, instr.Args[i], "r10")
		if src != winArgRegs[i] {
			w.WriteString(fmt.Sprintf("    mov %s, %s\n", winArgRegs[i], src))
		}
	}

	// Allocate 32-byte shadow space (required by Windows x64 ABI).
	w.WriteString("    sub rsp, 32\n")

	// Call the API function.
	w.WriteString(fmt.Sprintf("    call %s\n", apiName))

	// Clean up shadow space.
	w.WriteString("    add rsp, 32\n")

	// Clean up extra stack args.
	if stackArgs > 0 {
		w.WriteString(fmt.Sprintf("    add rsp, %d\n", stackArgs*8))
	}

	// Store return value (rax) if destination is specified.
	if instr.Dst.Kind != OpNone {
		e.nasmStoreToOperand(fn, instr.Dst, "rax")
	}
}

// emitNASMGCRuntime emits the GC runtime functions for x86_64 NASM (Intel syntax, Windows ABI).
// Includes: _novus_gc_register, _novus_gc_collect, _novus_gc_alloc.
func (e *x86_64Emitter) emitNASMGCRuntime() {
	w := e.b

	// ---- _novus_gc_register(ptr=rcx, size=rdx) ----
	w.WriteString("\n_novus_gc_register:\n")
	w.WriteString("    push rbx\n")
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov rbx, [rax]\n") // rbx = count
	w.WriteString(fmt.Sprintf("    cmp rbx, %d\n", e.target.GCEntries))
	w.WriteString("    jge .gcr_full\n")
	// table[count] = {ptr, size, 0}
	w.WriteString("    lea rax, [rel _novus_gc_table]\n")
	w.WriteString("    imul r8, rbx, 24\n")
	w.WriteString("    add rax, r8\n")       // rax = &table[count]
	w.WriteString("    mov [rax], rcx\n")    // ptr
	w.WriteString("    mov [rax+8], rdx\n")  // size
	w.WriteString("    mov qword [rax+16], 0\n") // mark = 0
	// count++
	w.WriteString("    inc rbx\n")
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov [rax], rbx\n")
	w.WriteString(".gcr_full:\n")
	w.WriteString("    pop rbx\n")
	w.WriteString("    ret\n")

	// ---- _novus_gc_collect() ----
	w.WriteString("\n_novus_gc_collect:\n")
	w.WriteString("    push rbx\n")
	w.WriteString("    push rsi\n")
	w.WriteString("    push rdi\n")
	w.WriteString("    push r12\n")
	w.WriteString("    push r13\n")
	w.WriteString("    push r14\n")
	w.WriteString("    push r15\n")

	// Clear all marks.
	w.WriteString("    lea rsi, [rel _novus_gc_table]\n")
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov rcx, [rax]\n") // rcx = count
	w.WriteString("    xor rbx, rbx\n")
	w.WriteString(".gcc_clear:\n")
	w.WriteString("    cmp rbx, rcx\n")
	w.WriteString("    jge .gcc_mark_stack\n")
	w.WriteString("    imul rax, rbx, 24\n")
	w.WriteString("    mov qword [rsi+rax+16], 0\n")
	w.WriteString("    inc rbx\n")
	w.WriteString("    jmp .gcc_clear\n")

	// Mark phase: scan stack.
	w.WriteString(".gcc_mark_stack:\n")
	w.WriteString("    mov r12, rsp\n") // scan from current SP
	w.WriteString("    lea rax, [rel _novus_gc_stack_bottom]\n")
	w.WriteString("    mov r13, [rax]\n") // to stack_bottom

	w.WriteString(".gcc_scan_stack:\n")
	w.WriteString("    cmp r12, r13\n")
	w.WriteString("    jge .gcc_mark_globals\n")
	w.WriteString("    mov r14, [r12]\n") // potential pointer
	// Check against all table entries.
	w.WriteString("    lea rsi, [rel _novus_gc_table]\n")
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov rcx, [rax]\n")
	w.WriteString("    xor rbx, rbx\n")
	w.WriteString(".gcc_check_entry:\n")
	w.WriteString("    cmp rbx, rcx\n")
	w.WriteString("    jge .gcc_next_word\n")
	w.WriteString("    imul rax, rbx, 24\n")
	w.WriteString("    mov r15, [rsi+rax]\n")   // entry.ptr
	w.WriteString("    cmp r14, r15\n")
	w.WriteString("    jb .gcc_next_entry\n")
	w.WriteString("    mov rdi, [rsi+rax+8]\n") // entry.size
	w.WriteString("    add rdi, r15\n")          // entry.ptr + entry.size
	w.WriteString("    cmp r14, rdi\n")
	w.WriteString("    jae .gcc_next_entry\n")
	// Mark it.
	w.WriteString("    mov qword [rsi+rax+16], 1\n")
	w.WriteString(".gcc_next_entry:\n")
	w.WriteString("    inc rbx\n")
	w.WriteString("    jmp .gcc_check_entry\n")
	w.WriteString(".gcc_next_word:\n")
	w.WriteString("    add r12, 8\n")
	w.WriteString("    jmp .gcc_scan_stack\n")

	// Mark phase: scan globals.
	w.WriteString(".gcc_mark_globals:\n")
	if len(e.mod.Globals) > 0 {
		for _, g := range e.mod.Globals {
			w.WriteString(fmt.Sprintf("    mov r14, [rel %s]\n", g.Name))
			w.WriteString("    lea rsi, [rel _novus_gc_table]\n")
			w.WriteString("    lea rax, [rel _novus_gc_count]\n")
			w.WriteString("    mov rcx, [rax]\n")
			w.WriteString("    xor rbx, rbx\n")
			w.WriteString(fmt.Sprintf(".gcc_glob_%s:\n", g.Name))
			w.WriteString("    cmp rbx, rcx\n")
			w.WriteString(fmt.Sprintf("    jge .gcc_glob_%s_done\n", g.Name))
			w.WriteString("    imul rax, rbx, 24\n")
			w.WriteString("    mov r15, [rsi+rax]\n")
			w.WriteString("    cmp r14, r15\n")
			w.WriteString(fmt.Sprintf("    jb .gcc_glob_%s_next\n", g.Name))
			w.WriteString("    mov rdi, [rsi+rax+8]\n")
			w.WriteString("    add rdi, r15\n")
			w.WriteString("    cmp r14, rdi\n")
			w.WriteString(fmt.Sprintf("    jae .gcc_glob_%s_next\n", g.Name))
			w.WriteString("    mov qword [rsi+rax+16], 1\n")
			w.WriteString(fmt.Sprintf(".gcc_glob_%s_next:\n", g.Name))
			w.WriteString("    inc rbx\n")
			w.WriteString(fmt.Sprintf("    jmp .gcc_glob_%s\n", g.Name))
			w.WriteString(fmt.Sprintf(".gcc_glob_%s_done:\n", g.Name))
		}
	}

	// Transitive marking — scan contents of marked allocations.
	w.WriteString(".gcc_trans:\n")
	w.WriteString("    xor rbx, rbx\n")          // i = 0
	w.WriteString("    xor rdi, rdi\n")           // changed = 0
	w.WriteString(".gcc_trans_loop:\n")
	w.WriteString("    lea rsi, [rel _novus_gc_table]\n")
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov rcx, [rax]\n")
	w.WriteString("    cmp rbx, rcx\n")
	w.WriteString("    jge .gcc_trans_check\n")
	w.WriteString("    imul rax, rbx, 24\n")
	w.WriteString("    cmp qword [rsi+rax+16], 0\n") // marked?
	w.WriteString("    je .gcc_trans_next\n")
	w.WriteString("    mov r8, [rsi+rax]\n")       // ptr
	w.WriteString("    mov r9, [rsi+rax+8]\n")     // size
	w.WriteString("    xor r10, r10\n")            // offset = 0
	w.WriteString(".gcc_trans_scan:\n")
	w.WriteString("    lea r11, [r10+8]\n")
	w.WriteString("    cmp r11, r9\n")
	w.WriteString("    jg .gcc_trans_next\n")
	w.WriteString("    mov r14, [r8+r10]\n")        // potential interior ptr
	w.WriteString("    push rbx\n")
	w.WriteString("    push r10\n")
	w.WriteString("    push r9\n")
	w.WriteString("    push r8\n")
	w.WriteString("    xor rbx, rbx\n")             // j = 0
	w.WriteString(".gcc_trans_match:\n")
	w.WriteString("    lea rsi, [rel _novus_gc_table]\n")
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov rcx, [rax]\n")
	w.WriteString("    cmp rbx, rcx\n")
	w.WriteString("    jge .gcc_trans_scan_next\n")
	w.WriteString("    imul rax, rbx, 24\n")
	w.WriteString("    cmp qword [rsi+rax+16], 0\n") // already marked?
	w.WriteString("    jne .gcc_trans_match_next\n")
	w.WriteString("    mov r15, [rsi+rax]\n")        // entry.ptr
	w.WriteString("    cmp r14, r15\n")
	w.WriteString("    jb .gcc_trans_match_next\n")
	w.WriteString("    mov r11, [rsi+rax+8]\n")      // entry.size
	w.WriteString("    add r11, r15\n")
	w.WriteString("    cmp r14, r11\n")
	w.WriteString("    jae .gcc_trans_match_next\n")
	w.WriteString("    mov qword [rsi+rax+16], 1\n") // mark
	w.WriteString("    mov rdi, 1\n")                 // changed = true (rdi kept across iter)
	w.WriteString(".gcc_trans_match_next:\n")
	w.WriteString("    inc rbx\n")
	w.WriteString("    jmp .gcc_trans_match\n")
	w.WriteString(".gcc_trans_scan_next:\n")
	w.WriteString("    pop r8\n")
	w.WriteString("    pop r9\n")
	w.WriteString("    pop r10\n")
	w.WriteString("    pop rbx\n")
	w.WriteString("    add r10, 8\n")
	w.WriteString("    jmp .gcc_trans_scan\n")
	w.WriteString(".gcc_trans_next:\n")
	w.WriteString("    inc rbx\n")
	w.WriteString("    jmp .gcc_trans_loop\n")
	w.WriteString(".gcc_trans_check:\n")
	w.WriteString("    test rdi, rdi\n")
	w.WriteString("    jnz .gcc_trans\n")

	// Sweep phase: free unmarked, add to free list.
	w.WriteString(".gcc_sweep:\n")
	w.WriteString("    lea rsi, [rel _novus_gc_table]\n")
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov rcx, [rax]\n")
	w.WriteString("    xor rbx, rbx\n")   // read index
	w.WriteString("    xor rdi, rdi\n")   // write index
	w.WriteString(".gcc_sweep_loop:\n")
	w.WriteString("    cmp rbx, rcx\n")
	w.WriteString("    jge .gcc_sweep_done\n")
	w.WriteString("    imul rax, rbx, 24\n")
	w.WriteString("    cmp qword [rsi+rax+16], 0\n")
	w.WriteString("    jne .gcc_sweep_keep\n")
	// Unmarked — add to free list if size >= 16.
	w.WriteString("    mov r14, [rsi+rax]\n")   // ptr
	w.WriteString("    mov r15, [rsi+rax+8]\n") // size
	w.WriteString("    cmp r15, 16\n")
	w.WriteString("    jl .gcc_sweep_skip\n")
	w.WriteString("    lea rax, [rel _novus_gc_freelist]\n")
	w.WriteString("    mov r8, [rax]\n")     // old head
	w.WriteString("    mov [r14], r8\n")     // block.next = old head
	w.WriteString("    mov [r14+8], r15\n")  // block.size = size
	w.WriteString("    mov [rax], r14\n")    // freelist = block
	w.WriteString(".gcc_sweep_skip:\n")
	w.WriteString("    inc rbx\n")
	w.WriteString("    jmp .gcc_sweep_loop\n")
	// Marked — compact.
	w.WriteString(".gcc_sweep_keep:\n")
	w.WriteString("    cmp rbx, rdi\n")
	w.WriteString("    je .gcc_sweep_no_copy\n")
	w.WriteString("    imul rax, rbx, 24\n")
	w.WriteString("    imul r8, rdi, 24\n")
	w.WriteString("    mov r14, [rsi+rax]\n")
	w.WriteString("    mov [rsi+r8], r14\n")
	w.WriteString("    mov r14, [rsi+rax+8]\n")
	w.WriteString("    mov [rsi+r8+8], r14\n")
	w.WriteString("    mov qword [rsi+r8+16], 0\n") // clear mark for next cycle
	w.WriteString(".gcc_sweep_no_copy:\n")
	w.WriteString("    inc rdi\n")
	w.WriteString("    inc rbx\n")
	w.WriteString("    jmp .gcc_sweep_loop\n")
	w.WriteString(".gcc_sweep_done:\n")
	// Update count.
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov [rax], rdi\n")
	// Double threshold.
	w.WriteString("    lea rax, [rel _novus_gc_threshold]\n")
	w.WriteString("    mov rcx, [rax]\n")
	w.WriteString("    shl rcx, 1\n")
	w.WriteString("    mov [rax], rcx\n")

	w.WriteString("    pop r15\n")
	w.WriteString("    pop r14\n")
	w.WriteString("    pop r13\n")
	w.WriteString("    pop r12\n")
	w.WriteString("    pop rdi\n")
	w.WriteString("    pop rsi\n")
	w.WriteString("    pop rbx\n")
	w.WriteString("    ret\n")

	// ---- _novus_gc_alloc(size=rcx) → rax ----
	w.WriteString("\n_novus_gc_alloc:\n")
	w.WriteString("    push rbx\n")
	w.WriteString("    push rsi\n")
	w.WriteString("    push rdi\n")
	// Align size to 8 bytes, min 16.
	w.WriteString("    add rcx, 7\n")
	w.WriteString("    and rcx, -8\n")
	w.WriteString("    cmp rcx, 16\n")
	w.WriteString("    jge .gca_size_ok\n")
	w.WriteString("    mov rcx, 16\n")
	w.WriteString(".gca_size_ok:\n")
	w.WriteString("    mov rsi, rcx\n") // rsi = aligned size

	// Check free list (first-fit).
	w.WriteString("    lea rbx, [rel _novus_gc_freelist]\n") // rbx = prev ptr (initially &freelist)
	w.WriteString(".gca_fl_loop:\n")
	w.WriteString("    mov rdi, [rbx]\n") // rdi = current block
	w.WriteString("    test rdi, rdi\n")
	w.WriteString("    jz .gca_fl_miss\n")
	w.WriteString("    cmp [rdi+8], rsi\n") // block.size >= needed?
	w.WriteString("    jge .gca_fl_hit\n")
	w.WriteString("    mov rbx, rdi\n") // prev = current (next ptr is at [block+0])
	w.WriteString("    jmp .gca_fl_loop\n")
	w.WriteString(".gca_fl_hit:\n")
	// Unlink from free list.
	w.WriteString("    mov rax, [rdi]\n")   // rax = block.next
	w.WriteString("    mov [rbx], rax\n")   // prev.next = block.next
	w.WriteString("    mov rax, rdi\n")     // return block
	w.WriteString("    jmp .gca_register\n")

	// Free list miss — bump allocate.
	w.WriteString(".gca_fl_miss:\n")
	// Check if gc_collect needed.
	w.WriteString("    lea rax, [rel _novus_gc_count]\n")
	w.WriteString("    mov rbx, [rax]\n")
	w.WriteString("    lea rax, [rel _novus_gc_threshold]\n")
	w.WriteString("    mov rdi, [rax]\n")
	w.WriteString("    cmp rbx, rdi\n")
	w.WriteString("    jl .gca_bump\n")
	// Trigger GC.
	w.WriteString("    push rcx\n")
	w.WriteString("    push rsi\n")
	w.WriteString("    sub rsp, 32\n") // shadow space
	w.WriteString("    call _novus_gc_collect\n")
	w.WriteString("    add rsp, 32\n")
	w.WriteString("    pop rsi\n")
	w.WriteString("    pop rcx\n")
	// Try free list again after collection.
	w.WriteString("    lea rbx, [rel _novus_gc_freelist]\n")
	w.WriteString(".gca_fl_retry:\n")
	w.WriteString("    mov rdi, [rbx]\n")
	w.WriteString("    test rdi, rdi\n")
	w.WriteString("    jz .gca_bump\n")
	w.WriteString("    cmp [rdi+8], rsi\n")
	w.WriteString("    jge .gca_fl_hit2\n")
	w.WriteString("    mov rbx, rdi\n")
	w.WriteString("    jmp .gca_fl_retry\n")
	w.WriteString(".gca_fl_hit2:\n")
	w.WriteString("    mov rax, [rdi]\n")
	w.WriteString("    mov [rbx], rax\n")
	w.WriteString("    mov rax, rdi\n")
	w.WriteString("    jmp .gca_register\n")

	// Bump allocate from heap.
	w.WriteString(".gca_bump:\n")
	w.WriteString("    lea rax, [rel _novus_heap_ptr]\n")
	w.WriteString("    mov rbx, [rax]\n")
	w.WriteString("    test rbx, rbx\n")
	w.WriteString("    jnz .gca_bump_ok\n")
	w.WriteString("    lea rbx, [rel _novus_heap]\n")
	w.WriteString(".gca_bump_ok:\n")
	w.WriteString("    lea rdi, [rbx+rsi]\n") // new heap ptr
	w.WriteString("    lea rax, [rel _novus_heap_ptr]\n")
	w.WriteString("    mov [rax], rdi\n")
	w.WriteString("    mov rax, rbx\n") // return old ptr

	// Register allocation.
	w.WriteString(".gca_register:\n")
	w.WriteString("    push rax\n")   // save result
	w.WriteString("    mov rcx, rax\n") // ptr (Windows ABI: first arg)
	w.WriteString("    mov rdx, rsi\n") // size (Windows ABI: second arg)
	w.WriteString("    sub rsp, 32\n")  // shadow space
	w.WriteString("    call _novus_gc_register\n")
	w.WriteString("    add rsp, 32\n")
	w.WriteString("    pop rax\n")
	w.WriteString("    pop rdi\n")
	w.WriteString("    pop rsi\n")
	w.WriteString("    pop rbx\n")
	w.WriteString("    ret\n")
}

// emitGASGCRuntime emits the GC runtime functions for x86_64 GAS (AT&T syntax).
// Includes: _novus_gc_register, _novus_gc_collect, _novus_gc_alloc.
func (e *x86_64Emitter) emitGASGCRuntime() {
	w := e.b

	gcTable := e.target.Sym("_novus_gc_table")
	gcCount := e.target.Sym("_novus_gc_count")
	gcThreshold := e.target.Sym("_novus_gc_threshold")
	gcFreelist := e.target.Sym("_novus_gc_freelist")
	gcStackBot := e.target.Sym("_novus_gc_stack_bottom")
	heapSym := e.target.Sym("_novus_heap")
	heapPtrSym := e.target.Sym("_novus_heap_ptr")
	gcRegSym := e.target.Sym("_novus_gc_register")
	gcCollectSym := e.target.Sym("_novus_gc_collect")
	gcAllocSym := e.target.Sym("_novus_gc_alloc")

	// ---- _novus_gc_register(rdi=ptr, rsi=size) ----
	w.WriteString(fmt.Sprintf("\n%s:\n", gcRegSym))
	w.WriteString("    pushq %rbp\n")
	w.WriteString("    movq %rsp, %rbp\n")
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcCount))
	w.WriteString("    movq (%rax), %rcx\n") // rcx = count
	w.WriteString(fmt.Sprintf("    cmpq $%d, %%rcx\n", e.target.GCEntries))
	w.WriteString("    jge .Lgcreg_done_gas\n")
	// Entry address: table + count * 24.
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rdx\n", gcTable))
	w.WriteString("    imulq $24, %rcx, %r8\n")
	w.WriteString("    addq %r8, %rdx\n")
	w.WriteString("    movq %rdi, (%rdx)\n")       // ptr
	w.WriteString("    movq %rsi, 8(%rdx)\n")      // size
	w.WriteString("    movq $0, 16(%rdx)\n")       // mark = 0
	w.WriteString("    incq %rcx\n")
	w.WriteString("    movq %rcx, (%rax)\n")       // count++
	w.WriteString(".Lgcreg_done_gas:\n")
	w.WriteString("    popq %rbp\n")
	w.WriteString("    ret\n")

	// ---- _novus_gc_collect() ----
	w.WriteString(fmt.Sprintf("\n%s:\n", gcCollectSym))
	w.WriteString("    pushq %rbp\n")
	w.WriteString("    movq %rsp, %rbp\n")
	w.WriteString("    pushq %rbx\n")
	w.WriteString("    pushq %r12\n")
	w.WriteString("    pushq %r13\n")
	w.WriteString("    pushq %r14\n")
	w.WriteString("    pushq %r15\n")

	// Load count → r12.
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcCount))
	w.WriteString("    movq (%rax), %r12\n")
	w.WriteString("    testq %r12, %r12\n")
	w.WriteString("    jz .Lgc_done_gas\n")

	// Table base → r13.
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%r13\n", gcTable))

	// Phase 1: Clear marks.
	w.WriteString("    xorq %rcx, %rcx\n")
	w.WriteString(".Lgc_clear_gas:\n")
	w.WriteString("    cmpq %r12, %rcx\n")
	w.WriteString("    jge .Lgc_scan_gas\n")
	w.WriteString("    imulq $24, %rcx, %rax\n")
	w.WriteString("    movq $0, 16(%r13,%rax)\n") // mark = 0
	w.WriteString("    incq %rcx\n")
	w.WriteString("    jmp .Lgc_clear_gas\n")

	// Phase 2: Conservative stack scan.
	w.WriteString(".Lgc_scan_gas:\n")
	w.WriteString("    movq %rsp, %r14\n") // scan cursor
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcStackBot))
	w.WriteString("    movq (%rax), %r15\n") // stack bottom
	w.WriteString("    testq %r15, %r15\n")
	w.WriteString("    jz .Lgc_scan_globals_gas\n")

	w.WriteString(".Lgc_scan_loop_gas:\n")
	w.WriteString("    cmpq %r15, %r14\n")
	w.WriteString("    jge .Lgc_scan_globals_gas\n")
	w.WriteString("    movq (%r14), %rbx\n") // potential pointer

	w.WriteString("    xorq %rcx, %rcx\n") // j = 0
	w.WriteString(".Lgc_match_gas:\n")
	w.WriteString("    cmpq %r12, %rcx\n")
	w.WriteString("    jge .Lgc_next_word_gas\n")
	w.WriteString("    imulq $24, %rcx, %rax\n")
	w.WriteString("    movq (%r13,%rax), %rsi\n")   // entry.ptr
	w.WriteString("    cmpq %rsi, %rbx\n")
	w.WriteString("    jb .Lgc_match_next_gas\n")   // value < ptr
	w.WriteString("    movq 8(%r13,%rax), %rdi\n")  // entry.size
	w.WriteString("    addq %rsi, %rdi\n")           // end = ptr+size
	w.WriteString("    cmpq %rdi, %rbx\n")
	w.WriteString("    jae .Lgc_match_next_gas\n")  // value >= end
	w.WriteString("    movq $1, 16(%r13,%rax)\n") // mark
	w.WriteString("    jmp .Lgc_next_word_gas\n")
	w.WriteString(".Lgc_match_next_gas:\n")
	w.WriteString("    incq %rcx\n")
	w.WriteString("    jmp .Lgc_match_gas\n")

	w.WriteString(".Lgc_next_word_gas:\n")
	w.WriteString("    addq $8, %r14\n")
	w.WriteString("    jmp .Lgc_scan_loop_gas\n")

	// Phase 2b: Scan globals.
	w.WriteString(".Lgc_scan_globals_gas:\n")
	for _, g := range e.mod.Globals {
		gSym := e.target.Sym(g.Name)
		id := e.uniqueID()
		matchLabel := fmt.Sprintf(".Lgc_gm_%d", id)
		nextLabel := fmt.Sprintf(".Lgc_gn_%d", id)
		matchNext := fmt.Sprintf(".Lgc_gmn_%d", id)

		w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gSym))
		w.WriteString("    movq (%rax), %rbx\n")
		w.WriteString("    xorq %rcx, %rcx\n")
		w.WriteString(fmt.Sprintf("%s:\n", matchLabel))
		w.WriteString("    cmpq %r12, %rcx\n")
		w.WriteString(fmt.Sprintf("    jge %s\n", nextLabel))
		w.WriteString("    imulq $24, %rcx, %rax\n")
		w.WriteString("    movq (%r13,%rax), %rsi\n")   // entry.ptr
		w.WriteString("    cmpq %rsi, %rbx\n")
		w.WriteString(fmt.Sprintf("    jb %s\n", matchNext))
		w.WriteString("    movq 8(%r13,%rax), %rdi\n")  // entry.size
		w.WriteString("    addq %rsi, %rdi\n")
		w.WriteString("    cmpq %rdi, %rbx\n")
		w.WriteString(fmt.Sprintf("    jae %s\n", matchNext))
		w.WriteString("    movq $1, 16(%r13,%rax)\n")
		w.WriteString(fmt.Sprintf("    jmp %s\n", nextLabel))
		w.WriteString(fmt.Sprintf("%s:\n", matchNext))
		w.WriteString("    incq %rcx\n")
		w.WriteString(fmt.Sprintf("    jmp %s\n", matchLabel))
		w.WriteString(fmt.Sprintf("%s:\n", nextLabel))
	}

	// Phase 2c: Transitive marking — scan contents of marked allocations.
	w.WriteString(".Lgc_trans_gas:\n")
	w.WriteString("    xorq %rcx, %rcx\n")       // i = 0
	w.WriteString("    xorq %rsi, %rsi\n")        // changed = 0
	w.WriteString(".Lgc_trans_loop_gas:\n")
	w.WriteString("    cmpq %r12, %rcx\n")
	w.WriteString("    jge .Lgc_trans_check_gas\n")
	w.WriteString("    imulq $24, %rcx, %rax\n")
	w.WriteString("    cmpq $0, 16(%r13,%rax)\n") // marked?
	w.WriteString("    je .Lgc_trans_next_gas\n")
	w.WriteString("    movq (%r13,%rax), %r8\n")   // ptr
	w.WriteString("    movq 8(%r13,%rax), %r9\n")  // size
	w.WriteString("    xorq %r10, %r10\n")         // offset = 0
	w.WriteString(".Lgc_trans_scan_gas:\n")
	w.WriteString("    leaq 8(%r10), %r11\n")
	w.WriteString("    cmpq %r9, %r11\n")
	w.WriteString("    jg .Lgc_trans_next_gas\n")
	w.WriteString("    movq (%r8,%r10), %rbx\n")   // potential ptr
	w.WriteString("    pushq %rcx\n")               // save i
	w.WriteString("    xorq %rcx, %rcx\n")          // j = 0
	w.WriteString(".Lgc_trans_match_gas:\n")
	w.WriteString("    cmpq %r12, %rcx\n")
	w.WriteString("    jge .Lgc_trans_scan_next_gas\n")
	w.WriteString("    imulq $24, %rcx, %rax\n")
	w.WriteString("    cmpq $0, 16(%r13,%rax)\n")  // already marked?
	w.WriteString("    jne .Lgc_trans_match_next_gas\n")
	w.WriteString("    movq (%r13,%rax), %rdi\n")   // entry.ptr
	w.WriteString("    cmpq %rdi, %rbx\n")
	w.WriteString("    jb .Lgc_trans_match_next_gas\n")
	w.WriteString("    movq 8(%r13,%rax), %r11\n")  // entry.size
	w.WriteString("    addq %rdi, %r11\n")           // end = ptr+size
	w.WriteString("    cmpq %r11, %rbx\n")
	w.WriteString("    jae .Lgc_trans_match_next_gas\n")
	w.WriteString("    movq $1, 16(%r13,%rax)\n")   // mark it
	w.WriteString("    movq $1, %rsi\n")             // changed = true
	w.WriteString(".Lgc_trans_match_next_gas:\n")
	w.WriteString("    incq %rcx\n")
	w.WriteString("    jmp .Lgc_trans_match_gas\n")
	w.WriteString(".Lgc_trans_scan_next_gas:\n")
	w.WriteString("    popq %rcx\n")                 // restore i
	w.WriteString("    addq $8, %r10\n")
	w.WriteString("    jmp .Lgc_trans_scan_gas\n")
	w.WriteString(".Lgc_trans_next_gas:\n")
	w.WriteString("    incq %rcx\n")
	w.WriteString("    jmp .Lgc_trans_loop_gas\n")
	w.WriteString(".Lgc_trans_check_gas:\n")
	w.WriteString("    testq %rsi, %rsi\n")
	w.WriteString("    jnz .Lgc_trans_gas\n")

	// Phase 3: Sweep.
	w.WriteString("    xorq %rcx, %rcx\n") // read index
	w.WriteString("    xorq %rdx, %rdx\n") // write index
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%r14\n", gcFreelist))

	w.WriteString(".Lgc_sweep_gas:\n")
	w.WriteString("    cmpq %r12, %rcx\n")
	w.WriteString("    jge .Lgc_sweep_done_gas\n")
	w.WriteString("    imulq $24, %rcx, %rax\n")
	w.WriteString("    cmpq $0, 16(%r13,%rax)\n")
	w.WriteString("    jnz .Lgc_sweep_keep_gas\n")

	// Unmarked: add to free list.
	w.WriteString("    movq (%r13,%rax), %rbx\n")    // ptr
	w.WriteString("    movq 8(%r13,%rax), %r8\n")    // size
	w.WriteString("    cmpq $16, %r8\n")
	w.WriteString("    jl .Lgc_sweep_skip_gas\n")
	w.WriteString("    movq (%r14), %r9\n")           // old head
	w.WriteString("    movq %r9, (%rbx)\n")           // block.next = old head
	w.WriteString("    movq %r8, 8(%rbx)\n")          // block.size = size
	w.WriteString("    movq %rbx, (%r14)\n")          // freelist = block
	w.WriteString(".Lgc_sweep_skip_gas:\n")
	w.WriteString("    incq %rcx\n")
	w.WriteString("    jmp .Lgc_sweep_gas\n")

	// Keep: compact.
	w.WriteString(".Lgc_sweep_keep_gas:\n")
	w.WriteString("    cmpq %rcx, %rdx\n")
	w.WriteString("    je .Lgc_sweep_keep_skip_gas\n")
	w.WriteString("    imulq $24, %rdx, %rbx\n")
	w.WriteString("    movq (%r13,%rax), %r8\n")
	w.WriteString("    movq %r8, (%r13,%rbx)\n")     // copy ptr
	w.WriteString("    movq 8(%r13,%rax), %r8\n")
	w.WriteString("    movq %r8, 8(%r13,%rbx)\n")    // copy size
	w.WriteString("    movq $0, 16(%r13,%rbx)\n")    // clear mark
	w.WriteString(".Lgc_sweep_keep_skip_gas:\n")
	w.WriteString("    incq %rdx\n")
	w.WriteString("    incq %rcx\n")
	w.WriteString("    jmp .Lgc_sweep_gas\n")

	w.WriteString(".Lgc_sweep_done_gas:\n")
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcCount))
	w.WriteString("    movq %rdx, (%rax)\n") // gc_count = compacted

	w.WriteString(".Lgc_done_gas:\n")
	w.WriteString("    popq %r15\n")
	w.WriteString("    popq %r14\n")
	w.WriteString("    popq %r13\n")
	w.WriteString("    popq %r12\n")
	w.WriteString("    popq %rbx\n")
	w.WriteString("    popq %rbp\n")
	w.WriteString("    ret\n")

	// ---- _novus_gc_alloc(rdi=size) → rax=ptr ----
	w.WriteString(fmt.Sprintf("\n%s:\n", gcAllocSym))
	w.WriteString("    pushq %rbp\n")
	w.WriteString("    movq %rsp, %rbp\n")
	w.WriteString("    pushq %rbx\n")
	w.WriteString("    pushq %r12\n")

	// Align size, min 16.
	w.WriteString("    addq $7, %rdi\n")
	w.WriteString("    andq $-8, %rdi\n")
	w.WriteString("    cmpq $16, %rdi\n")
	w.WriteString("    jge .Lgca_size_ok_gas\n")
	w.WriteString("    movq $16, %rdi\n")
	w.WriteString(".Lgca_size_ok_gas:\n")
	w.WriteString("    movq %rdi, %r12\n") // r12 = aligned size

	// Check if GC needed.
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcCount))
	w.WriteString("    movq (%rax), %rcx\n")
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcThreshold))
	w.WriteString("    movq (%rax), %rdx\n")
	w.WriteString("    cmpq %rdx, %rcx\n")
	w.WriteString("    jl .Lgca_try_free_gas\n")
	// Trigger collection.
	w.WriteString(fmt.Sprintf("    call %s\n", gcCollectSym))
	// Double threshold.
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", gcThreshold))
	w.WriteString("    shlq $1, (%rax)\n")

	// Try free list.
	w.WriteString(".Lgca_try_free_gas:\n")
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rbx\n", gcFreelist))
	w.WriteString("    movq (%rbx), %rax\n") // current block
	w.WriteString(".Lgca_fl_loop_gas:\n")
	w.WriteString("    testq %rax, %rax\n")
	w.WriteString("    jz .Lgca_bump_gas\n")
	w.WriteString("    cmpq %r12, 8(%rax)\n")
	w.WriteString("    jge .Lgca_fl_found_gas\n")
	w.WriteString("    movq %rax, %rbx\n")    // prev = current
	w.WriteString("    movq (%rax), %rax\n")  // current = next
	w.WriteString("    jmp .Lgca_fl_loop_gas\n")

	// Found free block.
	w.WriteString(".Lgca_fl_found_gas:\n")
	w.WriteString("    movq (%rax), %rcx\n")  // next
	w.WriteString("    movq %rcx, (%rbx)\n")  // unlink
	w.WriteString("    movq %rax, %rbx\n")    // save block ptr
	// Register.
	w.WriteString("    movq %rbx, %rdi\n")
	w.WriteString("    movq %r12, %rsi\n")
	w.WriteString("    pushq %rbx\n")
	w.WriteString(fmt.Sprintf("    call %s\n", gcRegSym))
	w.WriteString("    popq %rax\n")          // return block
	w.WriteString("    jmp .Lgca_ret_gas\n")

	// Bump allocate.
	w.WriteString(".Lgca_bump_gas:\n")
	// Inline bump allocator (same as old _novus_heap logic).
	readyLabel := fmt.Sprintf(".Lgca_ready_%d", e.uniqueID())
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rcx\n", heapPtrSym))
	w.WriteString("    movq (%rcx), %rax\n")
	w.WriteString("    testq %rax, %rax\n")
	w.WriteString(fmt.Sprintf("    jnz %s\n", readyLabel))
	w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n", heapSym))
	w.WriteString(fmt.Sprintf("%s:\n", readyLabel))
	w.WriteString("    movq %rax, %rbx\n")    // save alloc start
	w.WriteString("    addq %r12, %rax\n")    // new heap ptr
	w.WriteString("    movq %rax, (%rcx)\n")  // update heap ptr
	// Register with GC.
	w.WriteString("    movq %rbx, %rdi\n")
	w.WriteString("    movq %r12, %rsi\n")
	w.WriteString("    pushq %rbx\n")
	w.WriteString(fmt.Sprintf("    call %s\n", gcRegSym))
	w.WriteString("    popq %rax\n")

	w.WriteString(".Lgca_ret_gas:\n")
	w.WriteString("    popq %r12\n")
	w.WriteString("    popq %rbx\n")
	w.WriteString("    popq %rbp\n")
	w.WriteString("    ret\n")
}

func (e *x86_64Emitter) usesHeap() bool {
	for _, fn := range e.mod.Functions {
		for _, instr := range fn.Instrs {
			switch instr.Op {
			case IRStrConcat, IRArrayNew, IRArrayAppend, IRGCCollect:
				return true
			}
		}
	}
	return false
}

// needsWinMainArgs checks if the main function takes argc/argv parameters
// on Windows, requiring a __getmainargs prologue.
func (e *x86_64Emitter) needsWinMainArgs() bool {
	if e.target.OS != OS_Windows {
		return false
	}
	for _, fn := range e.mod.Functions {
		if fn.Name == "main" && fn.ParamCount >= 2 {
			return true
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

// ---------------------------------------------------------------------------
// x86_64 Flag operations
// ---------------------------------------------------------------------------

// flagNameToX86Setcc maps flag names to x86 SETcc instruction suffixes.
func flagNameToX86Setcc(name string) string {
	switch name {
	case "c", "carry", "cs", "cf":
		return "setc" // carry flag
	case "z", "zero", "zero_flag":
		return "setz" // zero flag
	case "n", "negative", "negative_flag":
		return "sets" // sign flag
	case "v", "overflow", "overflow_flag":
		return "seto" // overflow flag
	}
	return ""
}

func (e *x86_64Emitter) emitGASFlagOp(fn *IRFunc, instr IRInstr) {
	w := e.b

	flagName := ""
	if instr.Op == IRGetFlag {
		if instr.Src1.Kind == OpPhysReg {
			flagName = instr.Src1.PhysReg
		} else if instr.Src1.Kind == OpLabel {
			flagName = instr.Src1.Label
		}
	} else {
		if instr.Dst.Kind == OpPhysReg {
			flagName = instr.Dst.PhysReg
		} else if instr.Dst.Kind == OpLabel {
			flagName = instr.Dst.Label
		}
	}

	if instr.Op == IRGetFlag {
		setcc := flagNameToX86Setcc(flagName)
		if setcc != "" {
			w.WriteString("    xorq %r10, %r10\n")
			w.WriteString(fmt.Sprintf("    %s %%r10b\n", setcc))
		} else {
			w.WriteString(fmt.Sprintf("    ## getflag: unrecognised flag %q\n", flagName))
			w.WriteString("    xorq %r10, %r10\n")
		}
		e.gasStoreToOperand(fn, instr.Dst, "%r10")
	} else {
		w.WriteString(fmt.Sprintf("    ## setflag %q not directly supported on x86\n", flagName))
	}
}

func (e *x86_64Emitter) emitNASMFlagOp(fn *IRFunc, instr IRInstr) {
	w := e.b

	flagName := ""
	if instr.Op == IRGetFlag {
		if instr.Src1.Kind == OpPhysReg {
			flagName = instr.Src1.PhysReg
		} else if instr.Src1.Kind == OpLabel {
			flagName = instr.Src1.Label
		}
	} else {
		if instr.Dst.Kind == OpPhysReg {
			flagName = instr.Dst.PhysReg
		} else if instr.Dst.Kind == OpLabel {
			flagName = instr.Dst.Label
		}
	}

	if instr.Op == IRGetFlag {
		setcc := flagNameToX86Setcc(flagName)
		if setcc != "" {
			w.WriteString("    xor r10, r10\n")
			w.WriteString(fmt.Sprintf("    %s r10b\n", setcc))
		} else {
			w.WriteString(fmt.Sprintf("    ; getflag: unrecognised flag %q\n", flagName))
			w.WriteString("    xor r10, r10\n")
		}
		e.nasmStoreToOperand(fn, instr.Dst, "r10")
	} else {
		w.WriteString(fmt.Sprintf("    ; setflag %q not directly supported on x86\n", flagName))
	}
}
