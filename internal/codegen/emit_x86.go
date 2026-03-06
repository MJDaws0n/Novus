package codegen

import (
	"fmt"
	"strings"
)

// ---------------------------------------------------------------------------
// x86 (32-bit) Assembly Emitter
//
// Produces GAS (AT&T syntax) assembly for 32-bit Linux (ELF).
// Uses cdecl calling convention: all arguments on stack, return in eax.
// ---------------------------------------------------------------------------

// EmitX86 converts an IRModule to x86 (32-bit) assembly text.
func EmitX86(mod *IRModule, target *Target) string {
	e := &x86Emitter{
		mod:    mod,
		target: target,
		b:      &strings.Builder{},
	}
	e.emit()
	return e.b.String()
}

type x86Emitter struct {
	mod    *IRModule
	target *Target
	b      *strings.Builder
	nextID int
}

func (e *x86Emitter) uniqueID() int {
	id := e.nextID
	e.nextID++
	return id
}

func (e *x86Emitter) emit() {
	w := e.b

	// Data section.
	if len(e.mod.Strings) > 0 {
		w.WriteString(".data\n")
		for _, s := range e.mod.Strings {
			w.WriteString(fmt.Sprintf("%s:\n", s.Label))
			w.WriteString(fmt.Sprintf("    .asciz %s\n", gasQuoteString(s.Value)))
			w.WriteString(fmt.Sprintf("%s_len:\n", s.Label))
			w.WriteString(fmt.Sprintf("    .long %d\n", len(s.Value)))
		}
		w.WriteString("\n")
	}

	// BSS: bump-allocator heap.
	if e.usesHeap() {
		w.WriteString(".bss\n")
		w.WriteString("_novus_heap:\n")
		w.WriteString("    .space 1048576\n")
		w.WriteString("_novus_heap_ptr:\n")
		w.WriteString("    .space 4\n")
		// GC metadata (32-bit: 4-byte pointers, entries are {ptr(4), size(4), mark(4)} = 12 bytes).
		w.WriteString("_novus_gc_table:\n")
		w.WriteString(fmt.Sprintf("    .space %d\n", 8192*12))
		w.WriteString("_novus_gc_count:\n")
		w.WriteString("    .space 4\n")
		w.WriteString("_novus_gc_threshold:\n")
		w.WriteString("    .space 4\n")
		w.WriteString("_novus_gc_freelist:\n")
		w.WriteString("    .space 4\n")
		w.WriteString("_novus_gc_stack_bottom:\n")
		w.WriteString("    .space 4\n\n")
	}

	// Global variables in data section.
	if len(e.mod.Globals) > 0 {
		if len(e.mod.Strings) == 0 {
			w.WriteString(".data\n")
		}
		for _, g := range e.mod.Globals {
			w.WriteString(fmt.Sprintf("%s:\n", g.Name))
			if g.InitStr >= 0 {
				strLabel := e.mod.Strings[g.InitStr].Label
				w.WriteString(fmt.Sprintf("    .long %s\n", strLabel))
			} else {
				w.WriteString(fmt.Sprintf("    .long %d\n", int32(g.InitImm)))
			}
		}
		w.WriteString("\n")
	}

	// Text section.
	w.WriteString(".text\n")
	w.WriteString(".globl _start\n")
	w.WriteString(".globl main\n\n")

	// _start entry point calls main and exits.
	w.WriteString("_start:\n")
	if e.usesHeap() {
		w.WriteString("    movl %esp, _novus_gc_stack_bottom\n")
		w.WriteString("    movl $256, _novus_gc_threshold\n")
	}
	w.WriteString("    call main\n")
	w.WriteString("    movl %eax, %ebx\n")
	w.WriteString("    movl $1, %eax\n") // exit syscall
	w.WriteString("    int $0x80\n\n")

	for _, fn := range e.mod.Functions {
		e.emitFunction(fn)
	}

	if e.usesHeap() {
		e.emitX86GCRuntime()
	}
}

func (e *x86Emitter) emitFunction(fn *IRFunc) {
	w := e.b

	w.WriteString(fmt.Sprintf("%s:\n", fn.Name))
	w.WriteString("    pushl %ebp\n")
	w.WriteString("    movl %esp, %ebp\n")
	if fn.FrameSize > 0 {
		w.WriteString(fmt.Sprintf("    subl $%d, %%esp\n", fn.FrameSize))
	}

	for _, instr := range fn.Instrs {
		e.emitInstr(fn, instr)
	}

	w.WriteString("\n")
}

func (e *x86Emitter) emitInstr(fn *IRFunc, instr IRInstr) {
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
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
		}

	case IRLea:
		src := e.leaOperand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    leal %s, %%eax\n", src))
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    leal %s, %s\n", src, dst))
		}

	case IRLoad:
		src := e.memOperand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
		}

	case IRStore:
		src := e.operand(instr.Src1)
		if instr.Src2.Kind != OpNone {
			addr := e.operand(instr.Src2)
			w.WriteString(fmt.Sprintf("    movl %s, (%s)\n", src, stripPercent(addr)))
		} else {
			dst := e.memOperand(instr.Dst)
			if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
				w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
				w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
			} else {
				w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
			}
		}

	case IRPush:
		src := e.operand(instr.Src1)
		w.WriteString(fmt.Sprintf("    pushl %s\n", src))

	case IRPop:
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    popl %s\n", dst))

	case IRAdd:
		e.emitBinOp(instr, "addl")
	case IRSub:
		e.emitBinOp(instr, "subl")
	case IRMul:
		src1 := e.operand(instr.Src1)
		src2 := e.operand(instr.Src2)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
		w.WriteString(fmt.Sprintf("    imull %s, %%eax\n", src2))
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
	case IRDiv:
		src1 := e.operand(instr.Src1)
		src2 := e.operand(instr.Src2)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
		w.WriteString("    cdq\n")
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
		w.WriteString("    idivl %ecx\n")
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
	case IRUDiv:
		src1 := e.operand(instr.Src1)
		src2 := e.operand(instr.Src2)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
		w.WriteString("    xorl %edx, %edx\n")
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
		w.WriteString("    divl %ecx\n")
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
	case IRMod:
		src1 := e.operand(instr.Src1)
		src2 := e.operand(instr.Src2)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
		w.WriteString("    cdq\n")
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
		w.WriteString("    idivl %ecx\n")
		w.WriteString(fmt.Sprintf("    movl %%edx, %s\n", dst))
	case IRUMod:
		src1 := e.operand(instr.Src1)
		src2 := e.operand(instr.Src2)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
		w.WriteString("    xorl %edx, %edx\n")
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
		w.WriteString("    divl %ecx\n")
		w.WriteString(fmt.Sprintf("    movl %%edx, %s\n", dst))

	case IRNeg:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
			w.WriteString("    negl %eax\n")
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
			w.WriteString(fmt.Sprintf("    negl %s\n", dst))
		}

	case IRNot:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
			w.WriteString("    xorl $1, %eax\n")
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
			w.WriteString(fmt.Sprintf("    xorl $1, %s\n", dst))
		}

	case IRAnd:
		e.emitBinOp(instr, "andl")
	case IROr:
		e.emitBinOp(instr, "orl")
	case IRXor:
		e.emitBinOp(instr, "xorl")
	case IRShl:
		src1 := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
			if instr.Src2.Kind == OpImmediate {
				w.WriteString(fmt.Sprintf("    shll $%d, %%eax\n", instr.Src2.Imm))
			} else {
				src2 := e.operand(instr.Src2)
				w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
				w.WriteString("    shll %cl, %eax\n")
			}
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src1, dst))
			if instr.Src2.Kind == OpImmediate {
				w.WriteString(fmt.Sprintf("    shll $%d, %s\n", instr.Src2.Imm, dst))
			} else {
				src2 := e.operand(instr.Src2)
				w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
				w.WriteString(fmt.Sprintf("    shll %%cl, %s\n", dst))
			}
		}
	case IRShr:
		src1 := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
			if instr.Src2.Kind == OpImmediate {
				w.WriteString(fmt.Sprintf("    sarl $%d, %%eax\n", instr.Src2.Imm))
			} else {
				src2 := e.operand(instr.Src2)
				w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
				w.WriteString("    sarl %cl, %eax\n")
			}
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src1, dst))
			if instr.Src2.Kind == OpImmediate {
				w.WriteString(fmt.Sprintf("    sarl $%d, %s\n", instr.Src2.Imm, dst))
			} else {
				src2 := e.operand(instr.Src2)
				w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
				w.WriteString(fmt.Sprintf("    sarl %%cl, %s\n", dst))
			}
		}
	case IRBitNot:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
			w.WriteString("    notl %eax\n")
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
			w.WriteString(fmt.Sprintf("    notl %s\n", dst))
		}

	case IRCmpEq:
		e.emitCmp(instr, "sete")
	case IRCmpNe:
		e.emitCmp(instr, "setne")
	case IRCmpLt:
		e.emitCmp(instr, "setl")
	case IRCmpLe:
		e.emitCmp(instr, "setle")
	case IRCmpGt:
		e.emitCmp(instr, "setg")
	case IRCmpGe:
		e.emitCmp(instr, "setge")

	case IRJmp:
		w.WriteString(fmt.Sprintf("    jmp %s\n", instr.Dst.Label))

	case IRJmpIf:
		src := e.operand(instr.Src1)
		if isMemOperand(instr.Src1) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
			w.WriteString("    testl %eax, %eax\n")
		} else {
			w.WriteString(fmt.Sprintf("    testl %s, %s\n", src, src))
		}
		w.WriteString(fmt.Sprintf("    jnz %s\n", instr.Dst.Label))

	case IRJmpNot:
		src := e.operand(instr.Src1)
		if isMemOperand(instr.Src1) {
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
			w.WriteString("    testl %eax, %eax\n")
		} else {
			w.WriteString(fmt.Sprintf("    testl %s, %s\n", src, src))
		}
		w.WriteString(fmt.Sprintf("    jz %s\n", instr.Dst.Label))

	case IRCall:
		e.emitCall(fn, instr)

	case IRRet:
		if instr.Src1.Kind != OpNone {
			src := e.operand(instr.Src1)
			w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
		}
		if fn.FrameSize > 0 {
			w.WriteString(fmt.Sprintf("    addl $%d, %%esp\n", fn.FrameSize))
		}
		w.WriteString("    popl %ebp\n")
		w.WriteString("    ret\n")

	case IRSyscall:
		w.WriteString("    int $0x80\n")

	case IRInt:
		if instr.Src1.Kind == OpImmediate {
			w.WriteString(fmt.Sprintf("    int $0x%x\n", instr.Src1.Imm))
		} else {
			w.WriteString("    int $0x80\n")
		}

	case IRNop:
		w.WriteString("    nop\n")

	case IRSetReg:
		dst := e.operand(instr.Dst)
		src := e.operand(instr.Src1)
		w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))

	case IRGetReg:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))

	case IRSetFlag, IRGetFlag:
		e.emitFlagOp(instr)

	case IRStrLen:
		e.emitStrLen(instr)
	case IRStrIndex:
		e.emitStrIndex(instr)
	case IRStoreByte:
		addr := e.operand(instr.Dst)
		src := e.operand(instr.Src1)
		w.WriteString(fmt.Sprintf("    movb %s, (%s)\n", src, stripPercent(addr)))
	case IRStrConcat:
		e.emitStrConcat(instr)
	case IRStrCmpEq:
		e.emitStrCmpEq(instr)

	// Memory load (32-bit).
	case IRLoad8:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
		w.WriteString("    movzbl (%eax), %eax\n")
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
	case IRLoad32:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
		w.WriteString("    movl (%eax), %eax\n")
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
	case IRLoad64:
		// 32-bit can only handle lower 32 bits of a 64-bit value.
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src))
		w.WriteString("    movl (%eax), %eax\n")
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))

	// Array operations (32-bit, 4-byte elements).
	case IRArrayNew:
		e.emitArrayNew(instr)
	case IRArrayGet:
		e.emitArrayGet(instr)
	case IRArraySet:
		e.emitArraySet(instr)
	case IRArrayAppend:
		e.emitArrayAppend(instr)
	case IRArrayPop:
		e.emitArrayPop(instr)
	case IRArrayLen:
		e.emitArrayLen(instr)
	case IRWinCall:
		w.WriteString("    ## win_call: Windows API calls not supported on x86-32\n")

	case IRLoadGlobal:
		if instr.Src1.Kind == OpLabel {
			globalSym := e.target.Sym(instr.Src1.Label)
			dst := e.operand(instr.Dst)
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", globalSym, dst))
		}

	case IRStoreGlobal:
		if instr.Dst.Kind == OpLabel {
			globalSym := e.target.Sym(instr.Dst.Label)
			src := e.operand(instr.Src1)
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, globalSym))
		}

	case IRGCCollect:
		w.WriteString("    call _novus_gc_collect\n")
	}
}

func (e *x86Emitter) emitBinOp(instr IRInstr, mnemonic string) {
	w := e.b
	src1 := e.operand(instr.Src1)
	src2 := e.operand(instr.Src2)
	dst := e.operand(instr.Dst)
	// x86 doesn't allow mem-to-mem: route through %eax.
	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
	if isMemOperand(instr.Src2) && isMemOperand(instr.Dst) {
		w.WriteString(fmt.Sprintf("    %s %s, %%eax\n", mnemonic, src2))
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
	} else {
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		w.WriteString(fmt.Sprintf("    %s %s, %s\n", mnemonic, src2, dst))
	}
}

func (e *x86Emitter) emitCmp(instr IRInstr, setcc string) {
	w := e.b
	src1 := e.operand(instr.Src1)
	src2 := e.operand(instr.Src2)
	dst := e.operand(instr.Dst)
	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
	w.WriteString(fmt.Sprintf("    cmpl %s, %%eax\n", src2))
	w.WriteString(fmt.Sprintf("    %s %%al\n", setcc))
	w.WriteString("    movzbl %al, %eax\n")
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

func (e *x86Emitter) emitCall(fn *IRFunc, instr IRInstr) {
	w := e.b

	// cdecl: push all args in reverse order.
	for i := len(instr.Args) - 1; i >= 0; i-- {
		src := e.operand(instr.Args[i])
		w.WriteString(fmt.Sprintf("    pushl %s\n", src))
	}

	label := instr.Src1.Label
	w.WriteString(fmt.Sprintf("    call %s\n", label))

	// Clean up stack args.
	if len(instr.Args) > 0 {
		w.WriteString(fmt.Sprintf("    addl $%d, %%esp\n", len(instr.Args)*4))
	}

	if instr.Dst.Kind != OpNone {
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
	}
}

func (e *x86Emitter) emitStrLen(instr IRInstr) {
	w := e.b
	src := e.operand(instr.Src1)
	dst := e.operand(instr.Dst)
	startLabel := fmt.Sprintf(".Lstrlen32_s_%p", &instr)
	doneLabel := fmt.Sprintf(".Lstrlen32_d_%p", &instr)

	w.WriteString(fmt.Sprintf("    movl %s, %%edi\n", src))
	w.WriteString("    xorl %ecx, %ecx\n")
	w.WriteString(fmt.Sprintf("%s:\n", startLabel))
	w.WriteString("    cmpb $0, (%%edi,%%ecx)\n")
	w.WriteString(fmt.Sprintf("    je %s\n", doneLabel))
	w.WriteString("    incl %ecx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", startLabel))
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	w.WriteString(fmt.Sprintf("    movl %%ecx, %s\n", dst))
}

func (e *x86Emitter) emitStrIndex(instr IRInstr) {
	w := e.b
	src := e.operand(instr.Src1)
	idx := e.operand(instr.Src2)
	dst := e.operand(instr.Dst)
	w.WriteString(fmt.Sprintf("    movl %s, %%edi\n", src))
	w.WriteString(fmt.Sprintf("    movl %s, %%esi\n", idx))
	w.WriteString("    xorl %eax, %eax\n")
	w.WriteString("    movb (%%edi,%%esi), %al\n")
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

func (e *x86Emitter) emitStrConcat(instr IRInstr) {
	w := e.b
	src1 := e.operand(instr.Src1)
	src2 := e.operand(instr.Src2)
	dst := e.operand(instr.Dst)

	id := e.uniqueID()
	len1Label := fmt.Sprintf(".Lscl1_%d", id)
	len1Done := fmt.Sprintf(".Lscl1d_%d", id)
	len2Label := fmt.Sprintf(".Lscl2_%d", id)
	len2Done := fmt.Sprintf(".Lscl2d_%d", id)
	copy1Label := fmt.Sprintf(".Lsc1_%d", id)
	copy2Label := fmt.Sprintf(".Lsc2_%d", id)
	doneLabel := fmt.Sprintf(".Lscd_%d", id)

	// Save callee-saved registers.
	w.WriteString("    pushl %ebx\n")
	w.WriteString("    pushl %edi\n")

	// Load source pointers.
	w.WriteString(fmt.Sprintf("    movl %s, %%esi\n", src1))
	w.WriteString(fmt.Sprintf("    movl %s, %%edx\n", src2))

	// Compute strlen(s1) → ecx.
	w.WriteString("    xorl %ecx, %ecx\n")
	w.WriteString(fmt.Sprintf("%s:\n", len1Label))
	w.WriteString("    cmpb $0, (%%esi,%%ecx)\n")
	w.WriteString(fmt.Sprintf("    je %s\n", len1Done))
	w.WriteString("    incl %ecx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", len1Label))
	w.WriteString(fmt.Sprintf("%s:\n", len1Done))
	w.WriteString("    pushl %ecx\n") // save len1

	// Compute strlen(s2) → eax.
	w.WriteString("    xorl %eax, %eax\n")
	w.WriteString(fmt.Sprintf("%s:\n", len2Label))
	w.WriteString("    cmpb $0, (%%edx,%%eax)\n")
	w.WriteString(fmt.Sprintf("    je %s\n", len2Done))
	w.WriteString("    incl %eax\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", len2Label))
	w.WriteString(fmt.Sprintf("%s:\n", len2Done))

	// size = len1+len2+1
	w.WriteString("    popl %ecx\n") // len1
	w.WriteString("    addl %ecx, %eax\n")
	w.WriteString("    incl %eax\n")
	// Save s1, s2 on stack.
	w.WriteString("    pushl %esi\n")
	w.WriteString("    pushl %edx\n")
	// Call gc_alloc(size) — cdecl.
	w.WriteString("    pushl %eax\n")
	w.WriteString("    call _novus_gc_alloc\n")
	w.WriteString("    addl $4, %esp\n")
	w.WriteString("    popl %edx\n")
	w.WriteString("    popl %esi\n")
	// eax = allocated buffer. Save in ebx (callee-saved).
	w.WriteString("    movl %eax, %ebx\n")
	w.WriteString("    movl %eax, %edi\n") // edi = write cursor

	// Copy s1.
	w.WriteString(fmt.Sprintf("%s:\n", copy1Label))
	w.WriteString("    movb (%esi), %al\n")
	w.WriteString("    testb %al, %al\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", copy2Label))
	w.WriteString("    movb %al, (%edi)\n")
	w.WriteString("    incl %esi\n")
	w.WriteString("    incl %edi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy1Label))

	// Copy s2 (including null).
	w.WriteString(fmt.Sprintf("%s:\n", copy2Label))
	w.WriteString("    movb (%edx), %al\n")
	w.WriteString("    movb %al, (%edi)\n")
	w.WriteString("    testb %al, %al\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", doneLabel))
	w.WriteString("    incl %edx\n")
	w.WriteString("    incl %edi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy2Label))

	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	w.WriteString("    movl %ebx, %eax\n") // result = saved buffer start
	w.WriteString("    popl %edi\n")
	w.WriteString("    popl %ebx\n")
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

func (e *x86Emitter) usesHeap() bool {
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

// ---------------------------------------------------------------------------
// x86 operand formatting (32-bit GAS/AT&T)
// ---------------------------------------------------------------------------

func (e *x86Emitter) operand(op Operand) string {
	switch op.Kind {
	case OpImmediate:
		return fmt.Sprintf("$%d", op.Imm)
	case OpPhysReg:
		return fmt.Sprintf("%%%s", op.PhysReg)
	case OpVirtReg:
		offset := -(op.Reg + 1) * 4
		return fmt.Sprintf("%d(%%ebp)", offset-512) // vregs at ebp-512 and below
	case OpStringRef:
		label := e.mod.Strings[op.Imm].Label
		return fmt.Sprintf("$%s", label)
	case OpLabel:
		return op.Label
	case OpMemory:
		if op.MemBase != "" {
			return fmt.Sprintf("%d(%%%s)", op.MemOffset, op.MemBase)
		}
		return fmt.Sprintf("%d", op.MemOffset)
	case OpNone:
		return "$0"
	}
	return "$0"
}

func (e *x86Emitter) memOperand(op Operand) string {
	if op.Kind == OpMemory {
		return fmt.Sprintf("%d(%%%s)", op.MemOffset, op.MemBase)
	}
	return e.operand(op)
}

func (e *x86Emitter) leaOperand(op Operand) string {
	switch op.Kind {
	case OpMemory:
		return fmt.Sprintf("%d(%%%s)", op.MemOffset, op.MemBase)
	case OpStringRef:
		label := e.mod.Strings[op.Imm].Label
		return label
	default:
		return e.operand(op)
	}
}

// isMemOperand returns true if the operand references memory (stack slot or memory addressing).
func isMemOperand(op Operand) bool {
	return op.Kind == OpVirtReg || op.Kind == OpMemory
}

// ---------------------------------------------------------------------------
// x86 (32-bit) flag operations
// ---------------------------------------------------------------------------

func (e *x86Emitter) emitFlagOp(instr IRInstr) {
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
		dst := e.operand(instr.Dst)
		if setcc != "" {
			w.WriteString("    xorl %eax, %eax\n")
			w.WriteString(fmt.Sprintf("    %s %%al\n", setcc))
			w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
		} else {
			w.WriteString(fmt.Sprintf("    ## getflag: unrecognised flag %q\n", flagName))
			w.WriteString(fmt.Sprintf("    movl $0, %s\n", dst))
		}
	} else {
		w.WriteString(fmt.Sprintf("    ## setflag %q not directly supported on x86\n", flagName))
	}
}

// ---------------------------------------------------------------------------
// x86 (32-bit) string comparison
// ---------------------------------------------------------------------------

func (e *x86Emitter) emitStrCmpEq(instr IRInstr) {
	w := e.b
	src1 := e.operand(instr.Src1)
	src2 := e.operand(instr.Src2)
	dst := e.operand(instr.Dst)

	id := e.uniqueID()
	loopLabel := fmt.Sprintf(".Lsce32_l_%d", id)
	neqLabel := fmt.Sprintf(".Lsce32_n_%d", id)
	doneLabel := fmt.Sprintf(".Lsce32_d_%d", id)

	w.WriteString(fmt.Sprintf("    movl %s, %%esi\n", src1))
	w.WriteString(fmt.Sprintf("    movl %s, %%edi\n", src2))
	w.WriteString(fmt.Sprintf("%s:\n", loopLabel))
	w.WriteString("    movb (%esi), %al\n")
	w.WriteString("    movb (%edi), %cl\n")
	w.WriteString("    cmpb %cl, %al\n")
	w.WriteString(fmt.Sprintf("    jne %s\n", neqLabel))
	w.WriteString("    testb %al, %al\n")
	w.WriteString(fmt.Sprintf("    je %s\n", doneLabel)) // both null → equal
	w.WriteString("    incl %esi\n")
	w.WriteString("    incl %edi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", loopLabel))
	w.WriteString(fmt.Sprintf("%s:\n", neqLabel))
	w.WriteString(fmt.Sprintf("    movl $0, %s\n", dst))
	doneLabel2 := fmt.Sprintf(".Lsce32_e_%d", id)
	w.WriteString(fmt.Sprintf("    jmp %s\n", doneLabel2))
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	w.WriteString(fmt.Sprintf("    movl $1, %s\n", dst))
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel2))
}

// ---------------------------------------------------------------------------
// x86 (32-bit) array operations (4-byte elements, 12-byte header)
// Header layout: [data_ptr(4)][len(4)][cap(4)]
// ---------------------------------------------------------------------------

func (e *x86Emitter) emitArrayNew(instr IRInstr) {
	w := e.b
	cap := instr.Src2.Imm
	if cap < 4 {
		cap = 4
	}

	// Allocate header (12 bytes) + data (cap*4 bytes) via GC allocator.
	totalSize := 12 + cap*4
	w.WriteString(fmt.Sprintf("    pushl $%d\n", totalSize)) // cdecl: arg on stack
	w.WriteString("    call _novus_gc_alloc\n")
	w.WriteString("    addl $4, %esp\n")
	// eax = header pointer.
	w.WriteString("    leal 12(%eax), %ecx\n")                      // ecx = data start
	w.WriteString("    movl %ecx, (%eax)\n")                        // header.data_ptr
	w.WriteString("    movl $0, 4(%eax)\n")                         // header.len = 0
	w.WriteString(fmt.Sprintf("    movl $%d, 8(%%eax)\n", cap))    // header.cap
	dst := e.operand(instr.Dst)
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

func (e *x86Emitter) emitArrayGet(instr IRInstr) {
	w := e.b
	arrPtr := e.operand(instr.Src1)
	idx := e.operand(instr.Src2)
	dst := e.operand(instr.Dst)
	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", arrPtr))
	w.WriteString("    movl (%eax), %eax\n") // data_ptr
	w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", idx))
	w.WriteString("    movl (%eax,%ecx,4), %eax\n")
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

func (e *x86Emitter) emitArraySet(instr IRInstr) {
	w := e.b
	arrPtr := e.operand(instr.Dst)
	idx := e.operand(instr.Src1)
	val := e.operand(instr.Src2)
	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", arrPtr))
	w.WriteString("    movl (%eax), %eax\n") // data_ptr
	w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", idx))
	w.WriteString(fmt.Sprintf("    movl %s, %%edx\n", val))
	w.WriteString("    movl %edx, (%eax,%ecx,4)\n")
}

func (e *x86Emitter) emitArrayAppend(instr IRInstr) {
	w := e.b
	arrPtr := e.operand(instr.Dst)
	val := e.operand(instr.Src1)

	id := e.uniqueID()
	noGrowLabel := fmt.Sprintf(".Laang32_%d", id)
	capOkLabel := fmt.Sprintf(".Laaco32_%d", id)
	copyLabel := fmt.Sprintf(".Laacp32_%d", id)
	copyDoneLabel := fmt.Sprintf(".Laacd32_%d", id)

	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", arrPtr)) // eax = arrPtr
	w.WriteString(fmt.Sprintf("    movl %s, %%edx\n", val))    // edx = val
	w.WriteString("    pushl %edi\n")
	w.WriteString("    pushl %esi\n")
	// Load len and cap.
	w.WriteString("    movl 4(%eax), %ecx\n") // ecx = len
	w.WriteString("    movl 8(%eax), %edi\n") // edi = cap
	w.WriteString("    cmpl %edi, %ecx\n")
	w.WriteString(fmt.Sprintf("    jl %s\n", noGrowLabel))

	// --- GROW ---
	w.WriteString("    shll $1, %edi\n")
	w.WriteString("    cmpl $4, %edi\n")
	w.WriteString(fmt.Sprintf("    jge %s\n", capOkLabel))
	w.WriteString("    movl $4, %edi\n")
	w.WriteString(fmt.Sprintf("%s:\n", capOkLabel))

	// Save context.
	w.WriteString("    pushl %eax\n") // arrPtr
	w.WriteString("    pushl %edx\n") // val
	w.WriteString("    pushl %ecx\n") // len
	w.WriteString("    pushl %edi\n") // new_cap

	// Allocate new data via gc_alloc: new_cap * 4 bytes. cdecl.
	w.WriteString("    movl %edi, %eax\n")
	w.WriteString("    shll $2, %eax\n")    // eax = new_cap * 4
	w.WriteString("    pushl %eax\n")
	w.WriteString("    call _novus_gc_alloc\n")
	w.WriteString("    addl $4, %esp\n")
	// eax = new data block.

	// Restore saved values.
	w.WriteString("    popl %edi\n") // new_cap
	w.WriteString("    popl %ecx\n") // len
	w.WriteString("    popl %edx\n") // val
	w.WriteString("    movl %eax, %esi\n") // esi = new data
	w.WriteString("    popl %eax\n") // arrPtr

	// Copy old data. esi=new_data, ecx=len, eax=arrPtr.
	w.WriteString("    pushl %ebx\n")
	w.WriteString("    movl (%eax), %ebx\n") // ebx = old data_ptr
	w.WriteString("    pushl %ecx\n")        // save len
	w.WriteString("    xorl %ecx, %ecx\n")   // i = 0
	w.WriteString("    movl (%esp), %ecx\n") // reload len from stack
	w.WriteString("    pushl %ecx\n")        // save len as counter limit
	w.WriteString("    xorl %ecx, %ecx\n")
	w.WriteString(fmt.Sprintf("%s:\n", copyLabel))
	w.WriteString("    cmpl (%esp), %ecx\n")
	w.WriteString(fmt.Sprintf("    jge %s\n", copyDoneLabel))
	w.WriteString("    movl (%ebx,%ecx,4), %eax\n")
	w.WriteString("    movl %eax, (%esi,%ecx,4)\n")
	w.WriteString("    incl %ecx\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copyLabel))
	w.WriteString(fmt.Sprintf("%s:\n", copyDoneLabel))
	w.WriteString("    addl $4, %esp\n") // pop counter limit
	w.WriteString("    popl %ecx\n")     // restore len
	w.WriteString("    popl %ebx\n")     // restore ebx
	// Reload arrPtr.
	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", arrPtr))

	// Update header.
	w.WriteString("    movl %esi, (%eax)\n")   // data_ptr = new_data
	w.WriteString("    movl %edi, 8(%eax)\n")  // cap = new_cap

	// --- NO GROW ---
	w.WriteString(fmt.Sprintf("%s:\n", noGrowLabel))
	w.WriteString("    movl (%eax), %esi\n")       // data_ptr
	w.WriteString("    movl 4(%eax), %ecx\n")      // len (reload)
	w.WriteString("    movl %edx, (%esi,%ecx,4)\n") // data[len] = val
	w.WriteString("    incl %ecx\n")
	w.WriteString("    movl %ecx, 4(%eax)\n") // len++
	w.WriteString("    popl %esi\n")
	w.WriteString("    popl %edi\n")
}

func (e *x86Emitter) emitArrayPop(instr IRInstr) {
	w := e.b
	arrPtr := e.operand(instr.Src1)
	dst := e.operand(instr.Dst)
	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", arrPtr))
	w.WriteString("    movl 4(%eax), %ecx\n") // len
	w.WriteString("    decl %ecx\n")
	w.WriteString("    movl %ecx, 4(%eax)\n") // save len
	w.WriteString("    movl (%eax), %eax\n")  // data_ptr
	w.WriteString("    movl (%eax,%ecx,4), %eax\n")
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

func (e *x86Emitter) emitArrayLen(instr IRInstr) {
	w := e.b
	arrPtr := e.operand(instr.Src1)
	dst := e.operand(instr.Dst)
	w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", arrPtr))
	w.WriteString("    movl 4(%eax), %eax\n")
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

// emitX86GCRuntime emits the GC runtime functions for x86 32-bit (AT&T syntax, cdecl).
// GC table entries are 12 bytes: {ptr(4), size(4), mark(4)}.
func (e *x86Emitter) emitX86GCRuntime() {
	w := e.b

	// ---- _novus_gc_register(ptr, size) ----
	// cdecl: args at 8(%ebp), 12(%ebp).
	w.WriteString("\n_novus_gc_register:\n")
	w.WriteString("    pushl %ebp\n")
	w.WriteString("    movl %esp, %ebp\n")
	w.WriteString("    pushl %ebx\n")
	w.WriteString("    movl _novus_gc_count, %eax\n")
	w.WriteString("    cmpl $8192, %eax\n")
	w.WriteString("    jge .gcr32_full\n")
	// table[count] = {ptr, size, 0}. Entry size = 12 bytes.
	w.WriteString("    imull $12, %eax, %ebx\n")
	w.WriteString("    addl $_novus_gc_table, %ebx\n")
	w.WriteString("    movl 8(%ebp), %ecx\n")  // ptr arg
	w.WriteString("    movl %ecx, (%ebx)\n")
	w.WriteString("    movl 12(%ebp), %ecx\n") // size arg
	w.WriteString("    movl %ecx, 4(%ebx)\n")
	w.WriteString("    movl $0, 8(%ebx)\n")    // mark = 0
	w.WriteString("    incl %eax\n")
	w.WriteString("    movl %eax, _novus_gc_count\n")
	w.WriteString(".gcr32_full:\n")
	w.WriteString("    popl %ebx\n")
	w.WriteString("    popl %ebp\n")
	w.WriteString("    ret\n")

	// ---- _novus_gc_collect() ----
	w.WriteString("\n_novus_gc_collect:\n")
	w.WriteString("    pushl %ebp\n")
	w.WriteString("    movl %esp, %ebp\n")
	w.WriteString("    pushl %ebx\n")
	w.WriteString("    pushl %esi\n")
	w.WriteString("    pushl %edi\n")

	// Clear all marks.
	w.WriteString("    movl _novus_gc_count, %ecx\n")
	w.WriteString("    xorl %ebx, %ebx\n")
	w.WriteString(".gcc32_clear:\n")
	w.WriteString("    cmpl %ecx, %ebx\n")
	w.WriteString("    jge .gcc32_mark_stack\n")
	w.WriteString("    imull $12, %ebx, %eax\n")
	w.WriteString("    addl $_novus_gc_table, %eax\n")
	w.WriteString("    movl $0, 8(%eax)\n")
	w.WriteString("    incl %ebx\n")
	w.WriteString("    jmp .gcc32_clear\n")

	// Mark phase: scan stack (4 bytes at a time).
	w.WriteString(".gcc32_mark_stack:\n")
	w.WriteString("    movl %esp, %esi\n")
	w.WriteString("    movl _novus_gc_stack_bottom, %edi\n")
	w.WriteString(".gcc32_scan_stack:\n")
	w.WriteString("    cmpl %edi, %esi\n")
	w.WriteString("    jge .gcc32_trans\n") // go to transitive marking
	w.WriteString("    movl (%esi), %edx\n") // potential pointer
	// Inner loop: check edx against all table entries.
	// Save esi/edi on stack since we need registers.
	w.WriteString("    pushl %esi\n")
	w.WriteString("    pushl %edi\n")
	w.WriteString("    movl _novus_gc_count, %ecx\n")
	w.WriteString("    xorl %ebx, %ebx\n")
	w.WriteString(".gcc32_check:\n")
	w.WriteString("    cmpl %ecx, %ebx\n")
	w.WriteString("    jge .gcc32_check_done\n")
	w.WriteString("    imull $12, %ebx, %eax\n")
	w.WriteString("    addl $_novus_gc_table, %eax\n")
	w.WriteString("    movl (%eax), %esi\n")   // entry.ptr
	w.WriteString("    cmpl %esi, %edx\n")
	w.WriteString("    jb .gcc32_next_e\n")
	w.WriteString("    movl 4(%eax), %edi\n")  // entry.size
	w.WriteString("    addl %esi, %edi\n")     // end = ptr + size
	w.WriteString("    cmpl %edi, %edx\n")
	w.WriteString("    jae .gcc32_next_e\n")
	w.WriteString("    movl $1, 8(%eax)\n")    // mark
	w.WriteString(".gcc32_next_e:\n")
	w.WriteString("    incl %ebx\n")
	w.WriteString("    jmp .gcc32_check\n")
	w.WriteString(".gcc32_check_done:\n")
	w.WriteString("    popl %edi\n")
	w.WriteString("    popl %esi\n")
	w.WriteString("    addl $4, %esi\n")
	w.WriteString("    jmp .gcc32_scan_stack\n")

	// Transitive marking — scan marked allocation contents (4 bytes at a time).
	w.WriteString(".gcc32_trans:\n")
	w.WriteString("    xorl %ebx, %ebx\n")          // i = 0
	w.WriteString("    xorl %edx, %edx\n")           // changed = 0
	w.WriteString(".gcc32_trans_loop:\n")
	w.WriteString("    movl _novus_gc_count, %ecx\n")
	w.WriteString("    cmpl %ecx, %ebx\n")
	w.WriteString("    jge .gcc32_trans_check\n")
	w.WriteString("    imull $12, %ebx, %eax\n")
	w.WriteString("    addl $_novus_gc_table, %eax\n")
	w.WriteString("    cmpl $0, 8(%eax)\n")          // marked?
	w.WriteString("    je .gcc32_trans_next\n")
	w.WriteString("    pushl %ebx\n")                 // save i
	w.WriteString("    pushl %edx\n")                 // save changed
	w.WriteString("    movl (%eax), %esi\n")          // ptr
	w.WriteString("    movl 4(%eax), %edi\n")         // size
	w.WriteString("    xorl %ebx, %ebx\n")            // offset = 0
	w.WriteString(".gcc32_trans_scan:\n")
	w.WriteString("    leal 4(%ebx), %eax\n")
	w.WriteString("    cmpl %edi, %eax\n")
	w.WriteString("    jg .gcc32_trans_scan_done\n")
	w.WriteString("    movl (%esi,%ebx), %edx\n")     // potential ptr
	w.WriteString("    pushl %esi\n")
	w.WriteString("    pushl %edi\n")
	w.WriteString("    pushl %ebx\n")                  // save offset
	w.WriteString("    movl _novus_gc_count, %ecx\n")
	w.WriteString("    xorl %ebx, %ebx\n")             // j = 0
	w.WriteString(".gcc32_trans_match:\n")
	w.WriteString("    cmpl %ecx, %ebx\n")
	w.WriteString("    jge .gcc32_trans_match_done\n")
	w.WriteString("    imull $12, %ebx, %eax\n")
	w.WriteString("    addl $_novus_gc_table, %eax\n")
	w.WriteString("    cmpl $0, 8(%eax)\n")           // already marked?
	w.WriteString("    jne .gcc32_trans_match_next\n")
	w.WriteString("    movl (%eax), %esi\n")           // entry.ptr
	w.WriteString("    cmpl %esi, %edx\n")
	w.WriteString("    jb .gcc32_trans_match_next\n")
	w.WriteString("    movl 4(%eax), %edi\n")          // entry.size
	w.WriteString("    addl %esi, %edi\n")
	w.WriteString("    cmpl %edi, %edx\n")
	w.WriteString("    jae .gcc32_trans_match_next\n")
	w.WriteString("    movl $1, 8(%eax)\n")            // mark
	// Set changed flag on stack (was pushed as 2nd item).
	w.WriteString("    movl $1, 12(%esp)\n")           // changed (offset: ebx=0, edi=4, esi=8, changed=12 on stack)
	w.WriteString(".gcc32_trans_match_next:\n")
	w.WriteString("    incl %ebx\n")
	w.WriteString("    jmp .gcc32_trans_match\n")
	w.WriteString(".gcc32_trans_match_done:\n")
	w.WriteString("    popl %ebx\n")                   // restore offset
	w.WriteString("    popl %edi\n")
	w.WriteString("    popl %esi\n")
	w.WriteString("    addl $4, %ebx\n")
	w.WriteString("    jmp .gcc32_trans_scan\n")
	w.WriteString(".gcc32_trans_scan_done:\n")
	w.WriteString("    popl %edx\n")                   // restore changed
	w.WriteString("    popl %ebx\n")                   // restore i
	w.WriteString(".gcc32_trans_next:\n")
	w.WriteString("    incl %ebx\n")
	w.WriteString("    jmp .gcc32_trans_loop\n")
	w.WriteString(".gcc32_trans_check:\n")
	w.WriteString("    testl %edx, %edx\n")
	w.WriteString("    jnz .gcc32_trans\n")

	// Sweep phase: compact marked entries, free unmarked.
	w.WriteString(".gcc32_sweep:\n")
	w.WriteString("    movl _novus_gc_count, %ecx\n")
	w.WriteString("    xorl %ebx, %ebx\n")  // read index
	w.WriteString("    xorl %edi, %edi\n")  // write index
	w.WriteString(".gcc32_sweep_loop:\n")
	w.WriteString("    cmpl %ecx, %ebx\n")
	w.WriteString("    jge .gcc32_sweep_done\n")
	w.WriteString("    imull $12, %ebx, %eax\n")
	w.WriteString("    addl $_novus_gc_table, %eax\n")
	w.WriteString("    cmpl $0, 8(%eax)\n")
	w.WriteString("    jne .gcc32_keep\n")
	// Unmarked — add to free list if size >= 8.
	w.WriteString("    movl (%eax), %esi\n")     // ptr
	w.WriteString("    movl 4(%eax), %edx\n")    // size
	w.WriteString("    cmpl $8, %edx\n")
	w.WriteString("    jl .gcc32_skip\n")
	w.WriteString("    movl _novus_gc_freelist, %eax\n")
	w.WriteString("    movl %eax, (%esi)\n")     // block.next = old head
	w.WriteString("    movl %edx, 4(%esi)\n")    // block.size
	w.WriteString("    movl %esi, _novus_gc_freelist\n")
	w.WriteString(".gcc32_skip:\n")
	w.WriteString("    incl %ebx\n")
	w.WriteString("    jmp .gcc32_sweep_loop\n")
	// Marked — compact.
	w.WriteString(".gcc32_keep:\n")
	w.WriteString("    cmpl %edi, %ebx\n")
	w.WriteString("    je .gcc32_no_copy\n")
	w.WriteString("    imull $12, %ebx, %eax\n")
	w.WriteString("    addl $_novus_gc_table, %eax\n")
	w.WriteString("    imull $12, %edi, %edx\n")
	w.WriteString("    addl $_novus_gc_table, %edx\n")
	w.WriteString("    movl (%eax), %esi\n")
	w.WriteString("    movl %esi, (%edx)\n")
	w.WriteString("    movl 4(%eax), %esi\n")
	w.WriteString("    movl %esi, 4(%edx)\n")
	w.WriteString("    movl $0, 8(%edx)\n")
	w.WriteString(".gcc32_no_copy:\n")
	w.WriteString("    incl %edi\n")
	w.WriteString("    incl %ebx\n")
	w.WriteString("    jmp .gcc32_sweep_loop\n")
	w.WriteString(".gcc32_sweep_done:\n")
	w.WriteString("    movl %edi, _novus_gc_count\n")
	// Double threshold.
	w.WriteString("    movl _novus_gc_threshold, %eax\n")
	w.WriteString("    shll $1, %eax\n")
	w.WriteString("    movl %eax, _novus_gc_threshold\n")

	w.WriteString("    popl %edi\n")
	w.WriteString("    popl %esi\n")
	w.WriteString("    popl %ebx\n")
	w.WriteString("    popl %ebp\n")
	w.WriteString("    ret\n")

	// ---- _novus_gc_alloc(size) → eax ----
	w.WriteString("\n_novus_gc_alloc:\n")
	w.WriteString("    pushl %ebp\n")
	w.WriteString("    movl %esp, %ebp\n")
	w.WriteString("    pushl %ebx\n")
	w.WriteString("    pushl %esi\n")
	w.WriteString("    pushl %edi\n")
	// Align size to 4, min 8.
	w.WriteString("    movl 8(%ebp), %esi\n") // esi = requested size
	w.WriteString("    addl $3, %esi\n")
	w.WriteString("    andl $-4, %esi\n")
	w.WriteString("    cmpl $8, %esi\n")
	w.WriteString("    jge .gca32_size_ok\n")
	w.WriteString("    movl $8, %esi\n")
	w.WriteString(".gca32_size_ok:\n")

	// Bump allocate from heap.
	w.WriteString("    movl _novus_heap_ptr, %eax\n")
	w.WriteString("    testl %eax, %eax\n")
	w.WriteString("    jnz .gca32_bump_ok\n")
	w.WriteString("    movl $_novus_heap, %eax\n")
	w.WriteString(".gca32_bump_ok:\n")
	w.WriteString("    leal (%eax,%esi), %ebx\n") // new heap ptr
	w.WriteString("    movl %ebx, _novus_heap_ptr\n")
	// eax = allocated block.

	// Register with GC.
	w.WriteString("    pushl %eax\n")   // save result
	w.WriteString("    pushl %esi\n")   // size arg
	w.WriteString("    pushl %eax\n")   // ptr arg
	w.WriteString("    call _novus_gc_register\n")
	w.WriteString("    addl $8, %esp\n")
	w.WriteString("    popl %eax\n")    // restore result

	w.WriteString("    popl %edi\n")
	w.WriteString("    popl %esi\n")
	w.WriteString("    popl %ebx\n")
	w.WriteString("    popl %ebp\n")
	w.WriteString("    ret\n")
}
