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

	// BSS: string concatenation double-buffer.
	if e.usesStrConcat() {
		w.WriteString(".bss\n")
		w.WriteString("_novus_strcat_buf_a:\n")
		w.WriteString("    .space 4096\n")
		w.WriteString("_novus_strcat_buf_b:\n")
		w.WriteString("    .space 4096\n")
		w.WriteString("_novus_strcat_sel:\n")
		w.WriteString("    .space 4\n\n")
	}

	// Text section.
	w.WriteString(".text\n")
	w.WriteString(".globl _start\n")
	w.WriteString(".globl main\n\n")

	// _start entry point calls main and exits.
	w.WriteString("_start:\n")
	w.WriteString("    call main\n")
	w.WriteString("    movl %eax, %ebx\n")
	w.WriteString("    movl $1, %eax\n") // exit syscall
	w.WriteString("    int $0x80\n\n")

	for _, fn := range e.mod.Functions {
		e.emitFunction(fn)
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
		w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))

	case IRLea:
		src := e.leaOperand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    leal %s, %s\n", src, dst))

	case IRLoad:
		src := e.memOperand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))

	case IRStore:
		src := e.operand(instr.Src1)
		if instr.Src2.Kind != OpNone {
			addr := e.operand(instr.Src2)
			w.WriteString(fmt.Sprintf("    movl %s, (%s)\n", src, stripPercent(addr)))
		} else {
			dst := e.memOperand(instr.Dst)
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
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
	case IRMod:
		src1 := e.operand(instr.Src1)
		src2 := e.operand(instr.Src2)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n", src1))
		w.WriteString("    cdq\n")
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", src2))
		w.WriteString("    idivl %ecx\n")
		w.WriteString(fmt.Sprintf("    movl %%edx, %s\n", dst))

	case IRNeg:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
		w.WriteString(fmt.Sprintf("    negl %s\n", dst))

	case IRNot:
		src := e.operand(instr.Src1)
		dst := e.operand(instr.Dst)
		w.WriteString(fmt.Sprintf("    movl %s, %s\n", src, dst))
		w.WriteString(fmt.Sprintf("    xorl $1, %s\n", dst))

	case IRAnd:
		e.emitBinOp(instr, "andl")
	case IROr:
		e.emitBinOp(instr, "orl")
	case IRXor:
		e.emitBinOp(instr, "xorl")

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
		w.WriteString(fmt.Sprintf("    testl %s, %s\n", src, src))
		w.WriteString(fmt.Sprintf("    jnz %s\n", instr.Dst.Label))

	case IRJmpNot:
		src := e.operand(instr.Src1)
		w.WriteString(fmt.Sprintf("    testl %s, %s\n", src, src))
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
		w.WriteString("    ## flag manipulation (not yet implemented)\n")

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
	case IRGetTimeNs:
		e.emitGetTimeNs(instr)
	}
}

func (e *x86Emitter) emitBinOp(instr IRInstr, mnemonic string) {
	w := e.b
	src1 := e.operand(instr.Src1)
	src2 := e.operand(instr.Src2)
	dst := e.operand(instr.Dst)
	w.WriteString(fmt.Sprintf("    movl %s, %s\n", src1, dst))
	w.WriteString(fmt.Sprintf("    %s %s, %s\n", mnemonic, src2, dst))
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

	copy1Label := fmt.Sprintf(".Lsc32_1_%p", &instr)
	copy2Label := fmt.Sprintf(".Lsc32_2_%p", &instr)
	doneLabel := fmt.Sprintf(".Lsc32_d_%p", &instr)
	selALabel := fmt.Sprintf(".Lsc32_a_%p", &instr)

	// Load source pointers.
	w.WriteString(fmt.Sprintf("    movl %s, %%esi\n", src1))
	w.WriteString(fmt.Sprintf("    movl %s, %%edx\n", src2))

	// Toggle buffer selector and pick destination buffer.
	w.WriteString("    pushl %edi\n")
	w.WriteString("    xorb $1, _novus_strcat_sel\n")
	w.WriteString("    movzbl _novus_strcat_sel, %eax\n")
	w.WriteString("    movl $_novus_strcat_buf_a, %edi\n")
	w.WriteString("    testl %eax, %eax\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", selALabel))
	w.WriteString("    movl $_novus_strcat_buf_b, %edi\n")
	w.WriteString(fmt.Sprintf("%s:\n", selALabel))
	w.WriteString("    pushl %edi\n") // save buffer start

	// Copy left string (skip null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy1Label))
	w.WriteString("    movb (%esi), %al\n")
	w.WriteString("    testb %al, %al\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", copy2Label))
	w.WriteString("    movb %al, (%edi)\n")
	w.WriteString("    incl %esi\n")
	w.WriteString("    incl %edi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy1Label))

	// Copy right string (including null terminator).
	w.WriteString(fmt.Sprintf("%s:\n", copy2Label))
	w.WriteString("    movb (%edx), %al\n")
	w.WriteString("    movb %al, (%edi)\n")
	w.WriteString("    testb %al, %al\n")
	w.WriteString(fmt.Sprintf("    jz %s\n", doneLabel))
	w.WriteString("    incl %edx\n")
	w.WriteString("    incl %edi\n")
	w.WriteString(fmt.Sprintf("    jmp %s\n", copy2Label))

	// Done \u2014 recover buffer start.
	w.WriteString(fmt.Sprintf("%s:\n", doneLabel))
	w.WriteString("    popl %eax\n") // buffer start
	w.WriteString("    popl %edi\n") // restore
	w.WriteString(fmt.Sprintf("    movl %%eax, %s\n", dst))
}

// emitGetTimeNs emits inline x86 assembly to get time in nanoseconds.
// Uses gettimeofday syscall 78 on Linux (int 0x80 ABI).
func (e *x86Emitter) emitGetTimeNs(instr IRInstr) {
	w := e.b
	dst := e.operand(instr.Dst)
	w.WriteString("    // __time_ns: get current time in nanoseconds (32-bit stub)\n")
	// On 32-bit x86 we return 0 as a simplified stub. Timing is primarily
	// used on 64-bit targets. A full implementation would need to handle
	// 32-bit overflow carefully for the multiplication.
	w.WriteString(fmt.Sprintf("    movl $0, %s\n", dst))
}

func (e *x86Emitter) usesStrConcat() bool {
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
