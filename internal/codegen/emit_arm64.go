package codegen

import (
	"fmt"
	"strings"
)

// ---------------------------------------------------------------------------
// ARM64 (AArch64) Assembly Emitter
//
// Produces GAS assembly for macOS ARM64 and Linux ARM64 (aarch64).
// Uses the AAPCS64 calling convention: x0–x7 for args, x0 for return.
//
// Virtual registers and local variables are placed in the stack frame.
// Slot N is at [x29 - (N+1)*8].  For offsets outside the ldur/stur
// range of [-256, 255], we use x17 as a scratch to compute the address.
// ---------------------------------------------------------------------------

// EmitARM64 converts an IRModule to ARM64 assembly text.
func EmitARM64(mod *IRModule, target *Target) string {
	e := &arm64Emitter{
		mod:    mod,
		target: target,
		b:      &strings.Builder{},
	}
	e.emit()
	return peepholeARM64(e.b.String())
}

type arm64Emitter struct {
	mod    *IRModule
	target *Target
	b      *strings.Builder
	uid    int
}

func (e *arm64Emitter) uniqueID() int {
	e.uid++
	return e.uid
}

// toW converts an ARM64 x-register name to its w-register (32-bit) counterpart,
// e.g. "x10" → "w10".  Used for byte-level operations (ldrb/strb).
func toW(reg string) string {
	if len(reg) > 0 && reg[0] == 'x' {
		return "w" + reg[1:]
	}
	return reg
}

// ---------------------------------------------------------------------------
// Top-level emission
// ---------------------------------------------------------------------------

func (e *arm64Emitter) emit() {
	w := e.b

	// Data section.
	if len(e.mod.Strings) > 0 {
		if e.target.OS == OS_Darwin {
			w.WriteString(".section __DATA,__data\n")
		} else {
			w.WriteString(".data\n")
		}
		for _, s := range e.mod.Strings {
			label := e.target.Sym(s.Label)
			w.WriteString(".p2align 3\n")
			w.WriteString(fmt.Sprintf("%s:\n", label))
			w.WriteString(fmt.Sprintf("    .asciz %s\n", gasQuoteString(s.Value)))
			w.WriteString(fmt.Sprintf("%s_len:\n", label))
			w.WriteString(fmt.Sprintf("    .quad %d\n", len(s.Value)))
		}
		w.WriteString("\n")
	}

	// BSS: metadata for the mmap-backed dynamic heap allocator.
	if e.usesHeap() {
		heapPtrSym := e.target.Sym("_novus_heap_ptr")
		heapEndSym := e.target.Sym("_novus_heap_end")
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n", heapPtrSym))
		w.WriteString(fmt.Sprintf(".lcomm %s, 8\n\n", heapEndSym))
	}

	// Text section.
	if e.target.OS == OS_Darwin {
		w.WriteString(".section __TEXT,__text\n")
	} else {
		w.WriteString(".text\n")
	}

	entryFuncName := e.target.Sym("main")
	w.WriteString(fmt.Sprintf(".globl %s\n", entryFuncName))
	w.WriteString(".p2align 2\n\n")

	// Linux ARM64 needs a _start entry.
	if e.target.OS == OS_Linux {
		w.WriteString(".globl _start\n")
		w.WriteString("_start:\n")
		w.WriteString(fmt.Sprintf("    bl %s\n", entryFuncName))
		w.WriteString("    mov x8, #93\n")
		w.WriteString("    svc #0\n\n")
	}

	for _, fn := range e.mod.Functions {
		e.emitFunction(fn)
	}

	// Emit runtime heap allocator (uses mmap for growable memory).
	if e.usesHeap() {
		e.emitHeapAllocator()
	}
}

// computeFrameSize returns the total frame size for the function.
func (e *arm64Emitter) computeFrameSize(fn *IRFunc) int {
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
	totalSlots := fn.Locals + maxVReg + 4
	size := totalSlots * 8
	if size%16 != 0 {
		size += 16 - (size % 16)
	}
	if size < 16 {
		size = 16
	}
	return size
}

// vregOffset returns the frame offset for virtual register N.
// Locals occupy slots 0..locals-1 → [x29 - 8] to [x29 - locals*8].
// VRegs start after: [x29 - (locals + N + 1)*8].
func (e *arm64Emitter) vregOffset(fn *IRFunc, reg int) int {
	return -((fn.Locals + reg + 1) * 8)
}

func (e *arm64Emitter) emitFunction(fn *IRFunc) {
	w := e.b
	sym := e.target.Sym(fn.Name)
	frameSize := e.computeFrameSize(fn)

	// Precompute vreg use counts for compare+branch fusion decisions.
	vregUses := make(map[int]int)
	for _, instr := range fn.Instrs {
		for _, op := range []Operand{instr.Src1, instr.Src2} {
			if op.Kind == OpVirtReg {
				vregUses[op.Reg]++
			}
		}
		for _, op := range instr.Args {
			if op.Kind == OpVirtReg {
				vregUses[op.Reg]++
			}
		}
	}

	w.WriteString(fmt.Sprintf("%s:\n", sym))
	w.WriteString("    stp x29, x30, [sp, #-16]!\n")
	w.WriteString("    mov x29, sp\n")
	if frameSize > 0 {
		w.WriteString(fmt.Sprintf("    sub sp, sp, #%d\n", frameSize))
	}

	for i := 0; i < len(fn.Instrs); i++ {
		instr := fn.Instrs[i]

		// Optimization: fuse compare + conditional branch into a single
		// cmp + b.XX, eliminating cset/spill/reload/cbz overhead.
		if isCmpOp(instr.Op) && i+1 < len(fn.Instrs) {
			next := fn.Instrs[i+1]
			if canFuseCmpBranch(instr, next, vregUses) {
				e.emitFusedCmpBranch(fn, instr, next)
				i++ // skip the branch instruction
				continue
			}
		}

		e.emitInstr(fn, instr)
	}
	w.WriteString("\n")
}

// ---------------------------------------------------------------------------
// Instruction emission
// ---------------------------------------------------------------------------

func (e *arm64Emitter) emitInstr(fn *IRFunc, instr IRInstr) {
	w := e.b

	switch instr.Op {
	case IRLabel:
		if instr.Dst.Kind == OpLabel && instr.Dst.Label != fn.Name {
			w.WriteString(fmt.Sprintf("%s:\n", instr.Dst.Label))
		}

	case IRComment:
		if instr.Src1.Kind == OpLabel {
			w.WriteString(fmt.Sprintf("    // %s\n", instr.Src1.Label))
		}

	case IRMov:
		dst := e.dstReg(fn, instr.Dst, "x10")
		if instr.Src1.Kind == OpImmediate {
			e.loadImm(dst, instr.Src1.Imm)
		} else {
			src := e.loadToReg(fn, instr.Src1, "x11")
			w.WriteString(fmt.Sprintf("    mov %s, %s\n", dst, src))
		}
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRLea:
		dst := e.dstReg(fn, instr.Dst, "x10")
		if instr.Src1.Kind == OpStringRef {
			e.loadStringAddr(dst, int(instr.Src1.Imm))
		} else if instr.Src1.Kind == OpMemory {
			off := instr.Src1.MemOffset
			base := e.armBaseReg(instr.Src1.MemBase)
			if off >= 0 && off <= 4095 {
				w.WriteString(fmt.Sprintf("    add %s, %s, #%d\n", dst, base, off))
			} else if off >= -4095 && off < 0 {
				w.WriteString(fmt.Sprintf("    sub %s, %s, #%d\n", dst, base, -off))
			} else {
				e.loadImm("x17", off)
				w.WriteString(fmt.Sprintf("    add %s, %s, x17\n", dst, base))
			}
		} else {
			src := e.loadToReg(fn, instr.Src1, "x11")
			w.WriteString(fmt.Sprintf("    mov %s, %s\n", dst, src))
		}
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRLoad:
		dst := e.dstReg(fn, instr.Dst, "x10")
		if instr.Src1.Kind == OpMemory {
			e.emitLoadMem(dst, instr.Src1.MemBase, int(instr.Src1.MemOffset))
		} else if instr.Src1.Kind == OpStringRef {
			e.loadStringAddr(dst, int(instr.Src1.Imm))
		} else {
			src := e.loadToReg(fn, instr.Src1, "x11")
			w.WriteString(fmt.Sprintf("    ldr %s, [%s]\n", dst, src))
		}
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRStore:
		src := e.loadToReg(fn, instr.Src1, "x10")
		if instr.Dst.Kind == OpMemory {
			e.emitStoreMem(src, instr.Dst.MemBase, int(instr.Dst.MemOffset))
		} else if instr.Src2.Kind != OpNone {
			addr := e.loadToReg(fn, instr.Src2, "x11")
			w.WriteString(fmt.Sprintf("    str %s, [%s]\n", src, addr))
		}

	case IRPush:
		src := e.loadToReg(fn, instr.Src1, "x10")
		w.WriteString(fmt.Sprintf("    str %s, [sp, #-16]!\n", src))

	case IRPop:
		dst := e.dstReg(fn, instr.Dst, "x10")
		w.WriteString(fmt.Sprintf("    ldr %s, [sp], #16\n", dst))
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRAdd:
		e.emitBinOp(fn, instr, "add")
	case IRSub:
		e.emitBinOp(fn, instr, "sub")
	case IRMul:
		e.emitBinOp(fn, instr, "mul")
	case IRDiv:
		e.emitBinOp(fn, instr, "sdiv")
	case IRMod:
		dst := e.dstReg(fn, instr.Dst, "x10")
		src1 := e.loadToReg(fn, instr.Src1, "x11")
		src2 := e.loadToReg(fn, instr.Src2, "x12")
		w.WriteString(fmt.Sprintf("    sdiv x13, %s, %s\n", src1, src2))
		w.WriteString(fmt.Sprintf("    msub %s, x13, %s, %s\n", dst, src2, src1))
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRNeg:
		dst := e.dstReg(fn, instr.Dst, "x10")
		src := e.loadToReg(fn, instr.Src1, "x11")
		w.WriteString(fmt.Sprintf("    neg %s, %s\n", dst, src))
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRNot:
		dst := e.dstReg(fn, instr.Dst, "x10")
		src := e.loadToReg(fn, instr.Src1, "x11")
		w.WriteString(fmt.Sprintf("    eor %s, %s, #1\n", dst, src))
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRAnd:
		e.emitBinOp(fn, instr, "and")
	case IROr:
		e.emitBinOp(fn, instr, "orr")
	case IRXor:
		e.emitBinOp(fn, instr, "eor")

	case IRCmpEq:
		e.emitCmp(fn, instr, "eq")
	case IRCmpNe:
		e.emitCmp(fn, instr, "ne")
	case IRCmpLt:
		e.emitCmp(fn, instr, "lt")
	case IRCmpLe:
		e.emitCmp(fn, instr, "le")
	case IRCmpGt:
		e.emitCmp(fn, instr, "gt")
	case IRCmpGe:
		e.emitCmp(fn, instr, "ge")

	case IRJmp:
		w.WriteString(fmt.Sprintf("    b %s\n", instr.Dst.Label))

	case IRJmpIf:
		src := e.loadToReg(fn, instr.Src1, "x10")
		w.WriteString(fmt.Sprintf("    cbnz %s, %s\n", src, instr.Dst.Label))

	case IRJmpNot:
		src := e.loadToReg(fn, instr.Src1, "x10")
		w.WriteString(fmt.Sprintf("    cbz %s, %s\n", src, instr.Dst.Label))

	case IRCall:
		e.emitCall(fn, instr)

	case IRRet:
		if instr.Src1.Kind != OpNone {
			if instr.Src1.Kind == OpImmediate {
				e.loadImm("x0", instr.Src1.Imm)
			} else {
				src := e.loadToReg(fn, instr.Src1, "x10")
				if src != "x0" {
					w.WriteString(fmt.Sprintf("    mov x0, %s\n", src))
				}
			}
		}
		frameSize := e.computeFrameSize(fn)
		if frameSize > 0 {
			w.WriteString(fmt.Sprintf("    add sp, sp, #%d\n", frameSize))
		}
		w.WriteString("    ldp x29, x30, [sp], #16\n")
		w.WriteString("    ret\n")

	case IRSyscall:
		w.WriteString(fmt.Sprintf("    %s\n", e.target.SyscallInstr))

	case IRInt:
		if instr.Src1.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    svc #0x%x\n", instr.Src1.Imm))
		} else {
			w.WriteString(fmt.Sprintf("    %s\n", e.target.SyscallInstr))
		}

	case IRNop:
		w.WriteString("    nop\n")

	case IRSetReg:
		dst := e.dstReg(fn, instr.Dst, "x10")
		if instr.Src1.Kind == OpImmediate {
			e.loadImm(dst, instr.Src1.Imm)
		} else {
			src := e.loadToReg(fn, instr.Src1, "x11")
			w.WriteString(fmt.Sprintf("    mov %s, %s\n", dst, src))
		}
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRGetReg:
		src := e.loadToReg(fn, instr.Src1, "x11")
		dst := e.dstReg(fn, instr.Dst, "x10")
		w.WriteString(fmt.Sprintf("    mov %s, %s\n", dst, src))
		e.spillIfNeeded(fn, instr.Dst, dst)

	case IRSetFlag, IRGetFlag:
		w.WriteString("    // flag manipulation (not yet implemented on ARM64)\n")

	case IRStrLen:
		e.emitStrLen(fn, instr)
	case IRStrIndex:
		e.emitStrIndex(fn, instr)
	case IRStoreByte:
		addr := e.loadToReg(fn, instr.Dst, "x11")
		src := e.loadToReg(fn, instr.Src1, "x10")
		w.WriteString(fmt.Sprintf("    strb %s, [%s]\n", toW(src), addr))
	case IRStrConcat:
		e.emitStrConcat(fn, instr)

	// Memory load (raw address read)
	case IRLoad8:
		addr := e.loadToReg(fn, instr.Src1, "x10")
		dst := e.dstReg(fn, instr.Dst, "x11")
		w.WriteString(fmt.Sprintf("    ldrb %s, [%s]\n", toW(dst), addr))
		e.spillIfNeeded(fn, instr.Dst, dst)
	case IRLoad32:
		addr := e.loadToReg(fn, instr.Src1, "x10")
		dst := e.dstReg(fn, instr.Dst, "x11")
		w.WriteString(fmt.Sprintf("    ldr %s, [%s]\n", toW(dst), addr))
		e.spillIfNeeded(fn, instr.Dst, dst)
	case IRLoad64:
		addr := e.loadToReg(fn, instr.Src1, "x10")
		dst := e.dstReg(fn, instr.Dst, "x11")
		w.WriteString(fmt.Sprintf("    ldr %s, [%s]\n", dst, addr))
		e.spillIfNeeded(fn, instr.Dst, dst)

	// Array operations
	case IRArrayNew:
		e.emitArrayNew(fn, instr)
	case IRArrayGet:
		e.emitArrayGet(fn, instr)
	case IRArraySet:
		e.emitArraySet(fn, instr)
	case IRArrayAppend:
		e.emitArrayAppend(fn, instr)
	case IRArrayPop:
		e.emitArrayPop(fn, instr)
	case IRArrayLen:
		e.emitArrayLen(fn, instr)
	case IRWinCall:
		w.WriteString("    // win_call: Windows API calls not supported on ARM64\n")
	}
}

// ---------------------------------------------------------------------------
// Binary / comparison helpers
// ---------------------------------------------------------------------------

func (e *arm64Emitter) emitBinOp(fn *IRFunc, instr IRInstr, mnemonic string) {
	w := e.b
	dst := e.dstReg(fn, instr.Dst, "x10")
	src1 := e.loadToReg(fn, instr.Src1, "x11")

	// Optimization: use immediate form for add/sub with small constants.
	if instr.Src2.Kind == OpImmediate {
		imm := instr.Src2.Imm
		mn := mnemonic
		// Normalize: sub with negative → add with positive, and vice versa.
		if mn == "sub" && imm < 0 {
			imm = -imm
			mn = "add"
		} else if mn == "add" && imm < 0 {
			imm = -imm
			mn = "sub"
		}
		if (mn == "add" || mn == "sub") && imm >= 0 && imm <= 4095 {
			w.WriteString(fmt.Sprintf("    %s %s, %s, #%d\n", mn, dst, src1, imm))
			e.spillIfNeeded(fn, instr.Dst, dst)
			return
		}
	}

	src2 := e.loadToReg(fn, instr.Src2, "x12")
	w.WriteString(fmt.Sprintf("    %s %s, %s, %s\n", mnemonic, dst, src1, src2))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

func (e *arm64Emitter) emitCmp(fn *IRFunc, instr IRInstr, cond string) {
	w := e.b
	dst := e.dstReg(fn, instr.Dst, "x10")
	src1 := e.loadToReg(fn, instr.Src1, "x11")

	// Optimization: use immediate form for cmp with small constants.
	if instr.Src2.Kind == OpImmediate && instr.Src2.Imm >= 0 && instr.Src2.Imm <= 4095 {
		w.WriteString(fmt.Sprintf("    cmp %s, #%d\n", src1, instr.Src2.Imm))
	} else {
		src2 := e.loadToReg(fn, instr.Src2, "x12")
		w.WriteString(fmt.Sprintf("    cmp %s, %s\n", src1, src2))
	}
	w.WriteString(fmt.Sprintf("    cset %s, %s\n", dst, cond))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

func (e *arm64Emitter) emitCall(fn *IRFunc, instr IRInstr) {
	w := e.b
	argRegs := e.target.ArgRegs

	if len(instr.Args) > len(argRegs) {
		for i := len(instr.Args) - 1; i >= len(argRegs); i-- {
			src := e.loadToReg(fn, instr.Args[i], "x10")
			w.WriteString(fmt.Sprintf("    str %s, [sp, #-16]!\n", src))
		}
	}

	for i := 0; i < len(instr.Args) && i < len(argRegs); i++ {
		if instr.Args[i].Kind == OpImmediate {
			e.loadImm(argRegs[i], instr.Args[i].Imm)
		} else {
			src := e.loadToReg(fn, instr.Args[i], "x10")
			if src != argRegs[i] {
				w.WriteString(fmt.Sprintf("    mov %s, %s\n", argRegs[i], src))
			}
		}
	}

	label := instr.Src1.Label
	callTarget := e.target.Sym(label)
	w.WriteString(fmt.Sprintf("    bl %s\n", callTarget))

	stackArgs := len(instr.Args) - len(argRegs)
	if stackArgs > 0 {
		w.WriteString(fmt.Sprintf("    add sp, sp, #%d\n", stackArgs*16))
	}

	if instr.Dst.Kind != OpNone {
		dst := e.dstReg(fn, instr.Dst, "x10")
		if dst != "x0" {
			w.WriteString(fmt.Sprintf("    mov %s, x0\n", dst))
		}
		e.spillIfNeeded(fn, instr.Dst, dst)
	}
}

// ---------------------------------------------------------------------------
// String operations
// ---------------------------------------------------------------------------

func (e *arm64Emitter) emitStrLen(fn *IRFunc, instr IRInstr) {
	w := e.b
	src := e.loadToReg(fn, instr.Src1, "x13")
	dst := e.dstReg(fn, instr.Dst, "x10")
	id := e.uniqueID()
	startLabel := fmt.Sprintf(".Lstrlen_s_%d", id)
	doneLabel := fmt.Sprintf(".Lstrlen_d_%d", id)

	w.WriteString(fmt.Sprintf("    mov x14, %s\n", src))
	w.WriteString("    mov x15, #0\n")
	w.WriteString(fmt.Sprintf("%s:\n", startLabel))
	w.WriteString("    ldrb w16, [x14, x15]\n")
	w.WriteString(fmt.Sprintf("    cbz w16, %s\n", doneLabel))
	w.WriteString("    add x15, x15, #1\n")
	w.WriteString(fmt.Sprintf("    b %s\n", startLabel))
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	w.WriteString(fmt.Sprintf("    mov %s, x15\n", dst))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

func (e *arm64Emitter) emitStrIndex(fn *IRFunc, instr IRInstr) {
	w := e.b
	src := e.loadToReg(fn, instr.Src1, "x13")
	idx := e.loadToReg(fn, instr.Src2, "x14")
	dst := e.dstReg(fn, instr.Dst, "x10")
	w.WriteString(fmt.Sprintf("    ldrb w15, [%s, %s]\n", src, idx))
	w.WriteString(fmt.Sprintf("    uxtb %s, w15\n", dst))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

func (e *arm64Emitter) emitStrConcat(fn *IRFunc, instr IRInstr) {
	w := e.b
	src1 := e.loadToReg(fn, instr.Src1, "x13")
	src2 := e.loadToReg(fn, instr.Src2, "x14")
	dst := e.dstReg(fn, instr.Dst, "x10")

	id := e.uniqueID()
	len1Label := fmt.Sprintf(".Lscl1_%d", id)
	len1Done := fmt.Sprintf(".Lscl1d_%d", id)
	len2Label := fmt.Sprintf(".Lscl2_%d", id)
	len2Done := fmt.Sprintf(".Lscl2d_%d", id)
	copy1Label := fmt.Sprintf(".Lsc1_%d", id)
	copy2Label := fmt.Sprintf(".Lsc2_%d", id)

	allocSym := e.target.Sym("_novus_heap_alloc")

	// Move sources into dedicated scratch registers.
	if src1 != "x13" {
		w.WriteString(fmt.Sprintf("    mov x13, %s\n", src1))
	}
	if src2 != "x14" {
		w.WriteString(fmt.Sprintf("    mov x14, %s\n", src2))
	}

	// Compute strlen(s1) → x9.
	w.WriteString("    mov x15, #0\n")
	w.WriteString(fmt.Sprintf("%s:\n", len1Label))
	w.WriteString("    ldrb w16, [x13, x15]\n")
	w.WriteString(fmt.Sprintf("    cbz w16, %s\n", len1Done))
	w.WriteString("    add x15, x15, #1\n")
	w.WriteString(fmt.Sprintf("    b %s\n", len1Label))
	w.WriteString(fmt.Sprintf("%s:\n", len1Done))
	w.WriteString("    mov x9, x15\n") // x9 = len(s1)

	// Compute strlen(s2) → x15.
	w.WriteString("    mov x15, #0\n")
	w.WriteString(fmt.Sprintf("%s:\n", len2Label))
	w.WriteString("    ldrb w16, [x14, x15]\n")
	w.WriteString(fmt.Sprintf("    cbz w16, %s\n", len2Done))
	w.WriteString("    add x15, x15, #1\n")
	w.WriteString(fmt.Sprintf("    b %s\n", len2Label))
	w.WriteString(fmt.Sprintf("%s:\n", len2Done))
	// x15 = len(s2)

	// Allocate len(s1) + len(s2) + 1 bytes.
	w.WriteString("    add x0, x9, x15\n")
	w.WriteString("    add x0, x0, #1\n")
	// Save s1, s2, len1, len2 across the call.
	w.WriteString("    stp x13, x14, [sp, #-16]!\n")
	w.WriteString("    stp x9, x15, [sp, #-16]!\n")
	w.WriteString(fmt.Sprintf("    bl %s\n", allocSym))
	w.WriteString("    ldp x9, x15, [sp], #16\n")
	w.WriteString("    ldp x13, x14, [sp], #16\n")
	// x0 = allocated buffer.

	w.WriteString("    mov x12, x0\n") // x12 = result start
	w.WriteString("    mov x11, x0\n") // x11 = write cursor

	// Copy s1 bytes (skip null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy1Label))
	w.WriteString("    ldrb w16, [x13], #1\n")
	w.WriteString(fmt.Sprintf("    cbz w16, %s\n", copy2Label))
	w.WriteString("    strb w16, [x11], #1\n")
	w.WriteString(fmt.Sprintf("    b %s\n", copy1Label))

	// Copy s2 bytes (including null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy2Label))
	w.WriteString("    ldrb w16, [x14], #1\n")
	w.WriteString("    strb w16, [x11], #1\n")
	w.WriteString(fmt.Sprintf("    cbnz w16, %s\n", copy2Label))

	// Result = start of concatenated string.
	w.WriteString(fmt.Sprintf("    mov %s, x12\n", dst))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

// emitLoadGlobal emits an adrp+add pair to load the address of a global
// symbol into dstReg, handling Darwin vs Linux page-offset syntax.
func (e *arm64Emitter) emitLoadGlobal(dstReg, sym string) {
	w := e.b
	if e.target.OS == OS_Darwin {
		w.WriteString(fmt.Sprintf("    adrp %s, %s@PAGE\n", dstReg, sym))
		w.WriteString(fmt.Sprintf("    add %s, %s, %s@PAGEOFF\n", dstReg, dstReg, sym))
	} else {
		w.WriteString(fmt.Sprintf("    adrp %s, %s\n", dstReg, sym))
		w.WriteString(fmt.Sprintf("    add %s, %s, :lo12:%s\n", dstReg, dstReg, sym))
	}
}

// emitMmapSyscall emits an mmap(NULL, 256MB, PROT_READ|PROT_WRITE,
// MAP_PRIVATE|MAP_ANON, -1, 0) system call.  Result in x0.
// Clobbers x0-x5 and the syscall-number register (x16 Darwin, x8 Linux).
func (e *arm64Emitter) emitMmapSyscall() {
	w := e.b
	w.WriteString("    mov x0, #0\n")      // addr = NULL (let kernel choose)
	w.WriteString("    mov x1, #256\n")    // 256 (will shift to get MB)
	w.WriteString("    lsl x1, x1, #20\n") // x1 = 256 * 1048576 = 256 MB
	w.WriteString("    mov x2, #3\n")      // PROT_READ | PROT_WRITE
	if e.target.OS == OS_Darwin {
		w.WriteString("    mov x3, #0x1002\n") // MAP_PRIVATE | MAP_ANON (macOS)
		w.WriteString("    mov x4, #-1\n")     // fd = -1
		w.WriteString("    mov x5, #0\n")      // offset = 0
		w.WriteString("    mov x16, #197\n")   // SYS_mmap
		w.WriteString("    svc #0x80\n")
	} else {
		w.WriteString("    mov x3, #0x22\n") // MAP_PRIVATE | MAP_ANONYMOUS (Linux)
		w.WriteString("    mov x4, #-1\n")   // fd = -1
		w.WriteString("    mov x5, #0\n")    // offset = 0
		w.WriteString("    mov x8, #222\n")  // SYS_mmap (aarch64 Linux)
		w.WriteString("    svc #0\n")
	}
}

// emitHeapAllocator emits the _novus_heap_alloc runtime function.
// It provides a mmap-backed bump allocator that automatically grows by
// requesting new 256 MB virtual-memory regions from the kernel when the
// current region is exhausted.  Only pages actually touched consume
// physical RAM (demand-paged by the OS), so reserving a large region is
// essentially free.
//
//	Input:  x0 = requested size in bytes
//	Output: x0 = pointer to allocated block (8-byte aligned)
//	Clobbers: x0-x5, x9-x16 (caller-saved)
func (e *arm64Emitter) emitHeapAllocator() {
	w := e.b
	allocSym := e.target.Sym("_novus_heap_alloc")
	heapPtrSym := e.target.Sym("_novus_heap_ptr")
	heapEndSym := e.target.Sym("_novus_heap_end")

	w.WriteString("\n// ---- Runtime: mmap-backed heap allocator ----\n")
	w.WriteString(".p2align 2\n")
	w.WriteString(fmt.Sprintf("%s:\n", allocSym))
	w.WriteString("    stp x29, x30, [sp, #-16]!\n")
	w.WriteString("    mov x29, sp\n")

	// Align requested size to 8 bytes.
	w.WriteString("    add x0, x0, #7\n")
	w.WriteString("    bic x0, x0, #7\n")

	// Load heap_ptr.
	e.emitLoadGlobal("x9", heapPtrSym)
	w.WriteString("    ldr x10, [x9]\n") // x10 = current heap_ptr

	// If heap_ptr == 0 → first call, need to mmap initial region.
	w.WriteString("    cbz x10, .Lhalloc_init\n")

	w.WriteString(".Lhalloc_try:\n")
	// Compute new_ptr = heap_ptr + size.
	w.WriteString("    add x11, x10, x0\n")
	// Load heap_end.
	e.emitLoadGlobal("x12", heapEndSym)
	w.WriteString("    ldr x13, [x12]\n") // x13 = heap_end
	// If new_ptr > heap_end → need to grow.
	w.WriteString("    cmp x11, x13\n")
	w.WriteString("    b.hi .Lhalloc_grow\n")

	// ---- fast path: bump allocate ----
	w.WriteString("    str x11, [x9]\n") // heap_ptr = new_ptr
	w.WriteString("    mov x0, x10\n")   // return value = old heap_ptr
	w.WriteString("    ldp x29, x30, [sp], #16\n")
	w.WriteString("    ret\n")

	// ---- init path: mmap first 256 MB region ----
	w.WriteString(".Lhalloc_init:\n")
	// Save requested size and &heap_ptr across mmap syscall.
	w.WriteString("    stp x0, x9, [sp, #-16]!\n")
	e.emitMmapSyscall()
	// x0 = mmap base (or -1 on error — we rely on the kernel succeeding).
	w.WriteString("    mov x10, x0\n")
	w.WriteString("    ldp x0, x9, [sp], #16\n") // restore size, &heap_ptr
	// Store heap_ptr = mmap base.
	w.WriteString("    str x10, [x9]\n")
	// Store heap_end = base + 256 MB.
	w.WriteString("    mov x14, #256\n")
	w.WriteString("    lsl x14, x14, #20\n")
	w.WriteString("    add x14, x10, x14\n")
	e.emitLoadGlobal("x12", heapEndSym)
	w.WriteString("    str x14, [x12]\n")
	w.WriteString("    b .Lhalloc_try\n")

	// ---- grow path: mmap another 256 MB region ----
	w.WriteString(".Lhalloc_grow:\n")
	w.WriteString("    str x0, [sp, #-16]!\n") // save requested size
	e.emitMmapSyscall()
	w.WriteString("    mov x10, x0\n")       // x10 = new mmap base
	w.WriteString("    ldr x0, [sp], #16\n") // restore requested size
	// Update heap_ptr to new region base.
	e.emitLoadGlobal("x9", heapPtrSym)
	w.WriteString("    str x10, [x9]\n")
	// Update heap_end = base + 256 MB.
	w.WriteString("    mov x14, #256\n")
	w.WriteString("    lsl x14, x14, #20\n")
	w.WriteString("    add x14, x10, x14\n")
	e.emitLoadGlobal("x12", heapEndSym)
	w.WriteString("    str x14, [x12]\n")
	w.WriteString("    b .Lhalloc_try\n")
	w.WriteString("\n")
}

// ---------------------------------------------------------------------------
// Array operations (ARM64)
//
// Array layout on heap (24 bytes):
//   [0]  data_ptr  — pointer to element storage
//   [8]  len       — current number of elements
//   [16] cap       — allocated capacity (in elements)
// Elements are always 8 bytes each.
// ---------------------------------------------------------------------------

// emitArrayNew allocates a new array header + data on the heap.
// Dst = pointer to header. Src1 = elem_size (imm, always 8). Src2 = initial cap (imm).
func (e *arm64Emitter) emitArrayNew(fn *IRFunc, instr IRInstr) {
	w := e.b
	dst := e.dstReg(fn, instr.Dst, "x10")
	cap := instr.Src2.Imm
	if cap < 4 {
		cap = 4
	}

	allocSym := e.target.Sym("_novus_heap_alloc")

	// Allocate header (24 bytes) + data (cap * 8 bytes) in one call.
	totalSize := 24 + cap*8
	e.loadImm("x0", totalSize)
	w.WriteString(fmt.Sprintf("    bl %s\n", allocSym))
	// x0 = allocated block: [header 24B | data cap*8 B]
	w.WriteString("    mov x12, x0\n")      // x12 = header start
	w.WriteString("    add x13, x0, #24\n") // x13 = data start
	// Initialize header: data_ptr, len=0, cap.
	w.WriteString("    str x13, [x12, #0]\n") // header[0] = data_ptr
	w.WriteString("    str xzr, [x12, #8]\n") // header[8] = len = 0
	e.loadImm("x14", cap)
	w.WriteString("    str x14, [x12, #16]\n") // header[16] = cap
	// Result = header pointer.
	w.WriteString(fmt.Sprintf("    mov %s, x12\n", dst))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

// emitArrayGet: dst = arr[index]. Src1 = arrPtr, Src2 = index.
func (e *arm64Emitter) emitArrayGet(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.loadToReg(fn, instr.Src1, "x11")
	idx := e.loadToReg(fn, instr.Src2, "x12")
	dst := e.dstReg(fn, instr.Dst, "x10")

	// Load data_ptr from header.
	w.WriteString(fmt.Sprintf("    ldr x13, [%s, #0]\n", arrPtr)) // x13 = data_ptr
	// Load element: data_ptr[index * 8].
	w.WriteString(fmt.Sprintf("    ldr %s, [x13, %s, lsl #3]\n", dst, idx))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

// emitArraySet: arr[index] = val. Dst = arrPtr, Src1 = index, Src2 = val.
func (e *arm64Emitter) emitArraySet(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.loadToReg(fn, instr.Dst, "x11")
	idx := e.loadToReg(fn, instr.Src1, "x12")
	val := e.loadToReg(fn, instr.Src2, "x13")

	// Load data_ptr from header.
	w.WriteString(fmt.Sprintf("    ldr x14, [%s, #0]\n", arrPtr)) // x14 = data_ptr
	// Store element: data_ptr[index * 8] = val.
	w.WriteString(fmt.Sprintf("    str %s, [x14, %s, lsl #3]\n", val, idx))
}

// emitArrayAppend: append val to arr. Dst = arrPtr, Src1 = val.
// If len == cap, grow the array (allocate new data, copy, update header).
func (e *arm64Emitter) emitArrayAppend(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.loadToReg(fn, instr.Dst, "x11")
	val := e.loadToReg(fn, instr.Src1, "x12")

	id := e.uniqueID()
	noGrowLabel := fmt.Sprintf(".Laang_%d", id)
	capOkLabel := fmt.Sprintf(".Laaco_%d", id)
	copyLabel := fmt.Sprintf(".Laacp_%d", id)
	copyDoneLabel := fmt.Sprintf(".Laacd_%d", id)

	allocSym := e.target.Sym("_novus_heap_alloc")

	// Load len and cap from header.
	if arrPtr != "x11" {
		w.WriteString(fmt.Sprintf("    mov x11, %s\n", arrPtr))
	}
	w.WriteString("    ldr x13, [x11, #8]\n")  // x13 = len
	w.WriteString("    ldr x14, [x11, #16]\n") // x14 = cap
	// Check if full.
	w.WriteString("    cmp x13, x14\n")
	w.WriteString(fmt.Sprintf("    b.lt %s\n", noGrowLabel))

	// --- GROW ---
	// new_cap = cap * 2 (min 4).
	w.WriteString("    lsl x14, x14, #1\n")
	w.WriteString("    cmp x14, #4\n")
	w.WriteString(fmt.Sprintf("    b.ge %s\n", capOkLabel))
	w.WriteString("    mov x14, #4\n")
	w.WriteString(fmt.Sprintf("%s:\n", capOkLabel))

	// Allocate new data via heap allocator (new_cap * 8 bytes).
	// Save x11(arrPtr), x12(val), x13(len), x14(new_cap) across the call.
	w.WriteString("    stp x11, x12, [sp, #-16]!\n")
	w.WriteString("    stp x13, x14, [sp, #-16]!\n")
	w.WriteString("    lsl x0, x14, #3\n") // x0 = new_cap * 8
	w.WriteString(fmt.Sprintf("    bl %s\n", allocSym))
	w.WriteString("    mov x17, x0\n") // x17 = new data block
	w.WriteString("    ldp x13, x14, [sp], #16\n")
	w.WriteString("    ldp x11, x12, [sp], #16\n")

	// Copy old data to new data.
	w.WriteString("    ldr x16, [x11, #0]\n") // x16 = old data_ptr
	w.WriteString("    mov x9, #0\n")         // i = 0
	w.WriteString(fmt.Sprintf("%s:\n", copyLabel))
	w.WriteString("    cmp x9, x13\n") // while i < len
	w.WriteString(fmt.Sprintf("    b.ge %s\n", copyDoneLabel))
	w.WriteString("    ldr x18, [x16, x9, lsl #3]\n") // load old[i]
	w.WriteString("    str x18, [x17, x9, lsl #3]\n") // store new[i]
	w.WriteString("    add x9, x9, #1\n")
	w.WriteString(fmt.Sprintf("    b %s\n", copyLabel))
	w.WriteString(fmt.Sprintf("%s:\n", copyDoneLabel))

	// Update header: data_ptr = new_data, cap = new_cap.
	w.WriteString("    str x17, [x11, #0]\n")
	w.WriteString("    str x14, [x11, #16]\n")

	// --- NO GROW --- (or fall through after grow)
	w.WriteString(fmt.Sprintf("%s:\n", noGrowLabel))
	// Append value: data[len] = val.
	w.WriteString("    ldr x16, [x11, #0]\n") // x16 = data_ptr
	if val != "x12" {
		w.WriteString(fmt.Sprintf("    mov x12, %s\n", val))
	}
	w.WriteString("    str x12, [x16, x13, lsl #3]\n") // data[len] = val
	// len++.
	w.WriteString("    add x13, x13, #1\n")
	w.WriteString("    str x13, [x11, #8]\n") // header[8] = len
}

// emitArrayPop: dst = pop(arr). Src1 = arrPtr.
func (e *arm64Emitter) emitArrayPop(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.loadToReg(fn, instr.Src1, "x11")
	dst := e.dstReg(fn, instr.Dst, "x10")

	// len--.
	w.WriteString(fmt.Sprintf("    ldr x13, [%s, #8]\n", arrPtr)) // x13 = len
	w.WriteString("    sub x13, x13, #1\n")
	w.WriteString(fmt.Sprintf("    str x13, [%s, #8]\n", arrPtr)) // save len
	// Load element at data[len] (the one we just decremented to).
	w.WriteString(fmt.Sprintf("    ldr x14, [%s, #0]\n", arrPtr)) // x14 = data_ptr
	w.WriteString(fmt.Sprintf("    ldr %s, [x14, x13, lsl #3]\n", dst))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

// emitArrayLen: dst = len(arr). Src1 = arrPtr.
func (e *arm64Emitter) emitArrayLen(fn *IRFunc, instr IRInstr) {
	w := e.b
	arrPtr := e.loadToReg(fn, instr.Src1, "x11")
	dst := e.dstReg(fn, instr.Dst, "x10")
	w.WriteString(fmt.Sprintf("    ldr %s, [%s, #8]\n", dst, arrPtr))
	e.spillIfNeeded(fn, instr.Dst, dst)
}

func (e *arm64Emitter) usesHeap() bool {
	for _, fn := range e.mod.Functions {
		for _, instr := range fn.Instrs {
			switch instr.Op {
			case IRStrConcat, IRArrayNew, IRArrayAppend:
				return true
			}
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// Memory access helpers — handle arbitrary offsets via scratch register x17
// ---------------------------------------------------------------------------

func (e *arm64Emitter) emitLoadMem(dst, baseName string, offset int) {
	w := e.b
	base := e.armBaseReg(baseName)
	if offset >= -256 && offset <= 255 {
		w.WriteString(fmt.Sprintf("    ldur %s, [%s, #%d]\n", dst, base, offset))
	} else if offset >= 0 && offset <= 32760 && offset%8 == 0 {
		w.WriteString(fmt.Sprintf("    ldr %s, [%s, #%d]\n", dst, base, offset))
	} else {
		e.loadImm("x17", int64(offset))
		w.WriteString(fmt.Sprintf("    ldr %s, [%s, x17]\n", dst, base))
	}
}

func (e *arm64Emitter) emitStoreMem(src, baseName string, offset int) {
	w := e.b
	base := e.armBaseReg(baseName)
	if offset >= -256 && offset <= 255 {
		w.WriteString(fmt.Sprintf("    stur %s, [%s, #%d]\n", src, base, offset))
	} else if offset >= 0 && offset <= 32760 && offset%8 == 0 {
		w.WriteString(fmt.Sprintf("    str %s, [%s, #%d]\n", src, base, offset))
	} else {
		e.loadImm("x17", int64(offset))
		w.WriteString(fmt.Sprintf("    str %s, [%s, x17]\n", src, base))
	}
}

func (e *arm64Emitter) loadStringAddr(dst string, idx int) {
	w := e.b
	label := e.mod.Strings[idx].Label
	sym := e.target.Sym(label)
	if e.target.OS == OS_Darwin {
		w.WriteString(fmt.Sprintf("    adrp %s, %s@PAGE\n", dst, sym))
		w.WriteString(fmt.Sprintf("    add %s, %s, %s@PAGEOFF\n", dst, dst, sym))
	} else {
		w.WriteString(fmt.Sprintf("    adrp %s, %s\n", dst, sym))
		w.WriteString(fmt.Sprintf("    add %s, %s, :lo12:%s\n", dst, dst, sym))
	}
}

// ---------------------------------------------------------------------------
// Register management helpers
// ---------------------------------------------------------------------------

// ensureReg returns the register name for an operand.
// For VRegs: loads from stack into scratch; for imm: loads into scratch.
func (e *arm64Emitter) ensureReg(fn *IRFunc, op Operand, scratch string) string {
	switch op.Kind {
	case OpPhysReg:
		return e.mapPhysReg(op.PhysReg)
	case OpVirtReg:
		off := e.vregOffset(fn, op.Reg)
		e.emitLoadMem(scratch, "x29", off)
		return scratch
	case OpImmediate:
		e.loadImm(scratch, op.Imm)
		return scratch
	case OpStringRef:
		e.loadStringAddr(scratch, int(op.Imm))
		return scratch
	case OpMemory:
		e.emitLoadMem(scratch, op.MemBase, int(op.MemOffset))
		return scratch
	case OpLabel:
		return op.Label
	}
	return scratch
}

func (e *arm64Emitter) loadToReg(fn *IRFunc, op Operand, scratch string) string {
	return e.ensureReg(fn, op, scratch)
}

func (e *arm64Emitter) spillIfNeeded(fn *IRFunc, op Operand, reg string) {
	if op.Kind == OpVirtReg {
		off := e.vregOffset(fn, op.Reg)
		e.emitStoreMem(reg, "x29", off)
	}
}

func (e *arm64Emitter) loadImm(reg string, val int64) {
	w := e.b
	if val >= 0 && val <= 65535 {
		w.WriteString(fmt.Sprintf("    mov %s, #%d\n", reg, val))
	} else if val >= -65536 && val < 0 {
		w.WriteString(fmt.Sprintf("    mov %s, #%d\n", reg, val))
	} else {
		uval := uint64(val)
		w.WriteString(fmt.Sprintf("    movz %s, #%d, lsl #0\n", reg, uval&0xFFFF))
		if (uval>>16)&0xFFFF != 0 {
			w.WriteString(fmt.Sprintf("    movk %s, #%d, lsl #16\n", reg, (uval>>16)&0xFFFF))
		}
		if (uval>>32)&0xFFFF != 0 {
			w.WriteString(fmt.Sprintf("    movk %s, #%d, lsl #32\n", reg, (uval>>32)&0xFFFF))
		}
		if (uval>>48)&0xFFFF != 0 {
			w.WriteString(fmt.Sprintf("    movk %s, #%d, lsl #48\n", reg, (uval>>48)&0xFFFF))
		}
	}
}

func (e *arm64Emitter) mapPhysReg(name string) string {
	// Map x86 register names to ARM64 equivalents.
	// Always use 64-bit (x) registers to avoid width mismatches when
	// interacting with scratch registers (x9-x15).
	//
	// Note: Darwin ARM64 uses x16 for syscall numbers. We map the existing
	// Novus register name `r8` to x16 on Darwin so user code can do
	// `mov(r8, 0x2000004); syscall();`.
	if e.target != nil && e.target.OS == OS_Darwin && name == "r8" {
		return "x16"
	}
	mapping := map[string]string{
		"eax": "x0", "ebx": "x1", "ecx": "x2", "edx": "x3",
		"esi": "x4", "edi": "x5", "ebp": "x29", "esp": "sp",
		"rax": "x0", "rbx": "x1", "rcx": "x2", "rdx": "x3",
		"rsi": "x4", "rdi": "x5", "rbp": "x29", "rsp": "sp",
		"r8": "x8", "r9": "x9", "r10": "x10", "r11": "x11",
		"r12": "x12", "r13": "x13", "r14": "x14", "r15": "x15",
	}
	if mapped, ok := mapping[name]; ok {
		return mapped
	}
	return name
}

func (e *arm64Emitter) armBaseReg(name string) string {
	if name == "" {
		return "x29"
	}
	return e.mapPhysReg(name)
}

// dstReg returns the register name for a destination operand WITHOUT
// pre-loading the current value.  Use this for output operands that are
// about to be overwritten — avoids a useless memory load per instruction.
func (e *arm64Emitter) dstReg(fn *IRFunc, op Operand, scratch string) string {
	switch op.Kind {
	case OpPhysReg:
		return e.mapPhysReg(op.PhysReg)
	case OpVirtReg:
		return scratch // don't load — we're about to overwrite
	}
	return scratch
}

// ---------------------------------------------------------------------------
// Compare + Branch Fusion
// ---------------------------------------------------------------------------

func isCmpOp(op IROp) bool {
	switch op {
	case IRCmpEq, IRCmpNe, IRCmpLt, IRCmpLe, IRCmpGt, IRCmpGe:
		return true
	}
	return false
}

func canFuseCmpBranch(cmpInstr, brInstr IRInstr, vregUses map[int]int) bool {
	if brInstr.Op != IRJmpIf && brInstr.Op != IRJmpNot {
		return false
	}
	if cmpInstr.Dst.Kind != OpVirtReg || brInstr.Src1.Kind != OpVirtReg {
		return false
	}
	if cmpInstr.Dst.Reg != brInstr.Src1.Reg {
		return false
	}
	// Only fuse if the comparison result vreg is used exactly once (by the branch).
	return vregUses[cmpInstr.Dst.Reg] == 1
}

// cmpCondForOp returns the ARM64 condition code for a comparison opcode.
func cmpCondForOp(op IROp) string {
	switch op {
	case IRCmpEq:
		return "eq"
	case IRCmpNe:
		return "ne"
	case IRCmpLt:
		return "lt"
	case IRCmpLe:
		return "le"
	case IRCmpGt:
		return "gt"
	case IRCmpGe:
		return "ge"
	}
	return "eq"
}

// invertCond flips an ARM64 condition code.
func invertCond(cond string) string {
	switch cond {
	case "eq":
		return "ne"
	case "ne":
		return "eq"
	case "lt":
		return "ge"
	case "le":
		return "gt"
	case "gt":
		return "le"
	case "ge":
		return "lt"
	}
	return cond
}

// emitFusedCmpBranch emits a compare instruction followed directly by a
// conditional branch, eliminating the cset / spill / reload / cbz overhead.
func (e *arm64Emitter) emitFusedCmpBranch(fn *IRFunc, cmpInstr, brInstr IRInstr) {
	w := e.b

	src1 := e.loadToReg(fn, cmpInstr.Src1, "x11")

	if cmpInstr.Src2.Kind == OpImmediate && cmpInstr.Src2.Imm >= 0 && cmpInstr.Src2.Imm <= 4095 {
		w.WriteString(fmt.Sprintf("    cmp %s, #%d\n", src1, cmpInstr.Src2.Imm))
	} else {
		src2 := e.loadToReg(fn, cmpInstr.Src2, "x12")
		w.WriteString(fmt.Sprintf("    cmp %s, %s\n", src1, src2))
	}

	cond := cmpCondForOp(cmpInstr.Op)
	// JmpNot: branch when comparison is false → invert the condition.
	// JmpIf:  branch when comparison is true  → keep condition as-is.
	if brInstr.Op == IRJmpNot {
		cond = invertCond(cond)
	}
	w.WriteString(fmt.Sprintf("    b.%s %s\n", cond, brInstr.Dst.Label))
}

// ---------------------------------------------------------------------------
// Peephole Optimizer — cleans up redundant load-after-store patterns
// ---------------------------------------------------------------------------

// peepholeARM64 performs a single pass over the emitted assembly lines and
// eliminates (or replaces) redundant store-then-load sequences:
//
//	stur xN, [x29, #-K]    →  (keep store)
//	ldur xN, [x29, #-K]    →  (eliminate — xN already has the value)
//
//	stur xN, [x29, #-K]    →  (keep store)
//	ldur xM, [x29, #-K]    →  mov xM, xN  (cheaper than a memory load)
func peepholeARM64(asm string) string {
	lines := strings.Split(asm, "\n")
	result := make([]string, 0, len(lines))

	for i := 0; i < len(lines); i++ {
		if i+1 < len(lines) {
			stReg, stMem, isSt := parseARM64MemOp(lines[i], true)
			ldReg, ldMem, isLd := parseARM64MemOp(lines[i+1], false)
			if isSt && isLd && stMem == ldMem {
				result = append(result, lines[i]) // keep the store
				if stReg != ldReg {
					result = append(result, fmt.Sprintf("    mov %s, %s", ldReg, stReg))
				}
				i++ // skip the load
				continue
			}
		}
		result = append(result, lines[i])
	}

	return strings.Join(result, "\n")
}

// parseARM64MemOp extracts register and memory-reference from a store or load
// instruction.  isStore=true matches stur/str, isStore=false matches ldur/ldr.
func parseARM64MemOp(line string, isStore bool) (reg, mem string, ok bool) {
	trimmed := strings.TrimSpace(line)
	var prefixes []string
	if isStore {
		prefixes = []string{"stur ", "str "}
	} else {
		prefixes = []string{"ldur ", "ldr "}
	}
	for _, prefix := range prefixes {
		if strings.HasPrefix(trimmed, prefix) {
			rest := trimmed[len(prefix):]
			idx := strings.Index(rest, ", [")
			if idx >= 0 {
				reg = strings.TrimSpace(rest[:idx])
				mem = strings.TrimSpace(rest[idx+2:])
				return reg, mem, true
			}
		}
	}
	return "", "", false
}
