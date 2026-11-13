package codegen

import "fmt"

// Novus values keep their full 64-bit representation on i386. Pointers and OS
// structures remain 32-bit; function arguments occupy eight-byte stack slots.
func (e *x86Emitter) widePart(op Operand, high bool) string {
	if !high {
		if op.Kind == OpImmediate {
			return fmt.Sprintf("$%d", int32(op.Imm))
		}
		return e.operand(op)
	}
	switch op.Kind {
	case OpImmediate:
		return fmt.Sprintf("$%d", int32(uint64(op.Imm)>>32))
	case OpVirtReg:
		return fmt.Sprintf("%d(%%ebp)", e.vregOffset(e.fn, op.Reg)+4)
	case OpMemory:
		if op.MemBase != "" {
			return fmt.Sprintf("%d(%%%s)", op.MemOffset+4, op.MemBase)
		}
		return fmt.Sprintf("%d", op.MemOffset+4)
	case OpLabel:
		return op.Label + "+4"
	}
	return "$0"
}

func (e *x86Emitter) wideLoad(op Operand, low, high string) {
	e.b.WriteString(fmt.Sprintf("    movl %s, %s\n    movl %s, %s\n", e.widePart(op, false), low, e.widePart(op, true), high))
}

func (e *x86Emitter) wideStore(op Operand, low, high string) {
	if op.Kind == OpNone {
		return
	}
	e.b.WriteString(fmt.Sprintf("    movl %s, %s\n", low, e.widePart(op, false)))
	if op.Kind != OpPhysReg {
		e.b.WriteString(fmt.Sprintf("    movl %s, %s\n", high, e.widePart(op, true)))
	}
}

func (e *x86Emitter) wideResult(op Operand) { e.wideStore(op, "%eax", "%edx") }

func (e *x86Emitter) emitWide(instr IRInstr) bool {
	w := e.b
	switch instr.Op {
	case IRMov, IRLoad, IRStore:
		if instr.Op == IRStore && instr.Src2.Kind != OpNone {
			w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", e.widePart(instr.Src2, false)))
			e.wideLoad(instr.Src1, "%eax", "%edx")
			w.WriteString("    movl %eax, (%ecx)\n")
			if instr.Dst.Kind != OpImmediate || instr.Dst.Imm != 4 {
				w.WriteString("    movl %edx, 4(%ecx)\n")
			}
		} else if instr.Dst.Kind == OpPhysReg {
			w.WriteString(fmt.Sprintf("    movl %s, %s\n", e.widePart(instr.Src1, false), e.operand(instr.Dst)))
		} else if instr.Src1.Kind == OpPhysReg {
			// Register reads and their following local stores must preserve flags
			// and all registers: more syscall results may still need capturing.
			e.wideStore(instr.Dst, e.operand(instr.Src1), "$0")
		} else if isMemOperand(instr.Src1) && isMemOperand(instr.Dst) {
			w.WriteString(fmt.Sprintf("    pushl %s\n    pushl %s\n    popl %s\n    popl %s\n", e.widePart(instr.Src1, true), e.widePart(instr.Src1, false), e.widePart(instr.Dst, false), e.widePart(instr.Dst, true)))
		} else {
			e.wideLoad(instr.Src1, "%eax", "%edx")
			e.wideResult(instr.Dst)
		}
	case IRLea:
		if instr.Dst.Kind == OpPhysReg {
			w.WriteString(fmt.Sprintf("    leal %s, %s\n", e.leaOperand(instr.Src1), e.operand(instr.Dst)))
		} else {
			w.WriteString(fmt.Sprintf("    leal %s, %%eax\n    movl $0, %%edx\n", e.leaOperand(instr.Src1)))
			e.wideResult(instr.Dst)
		}
	case IRAdd, IRSub, IRAnd, IROr, IRXor, IRMul:
		e.wideLoad(instr.Src1, "%eax", "%edx")
		e.wideLoad(instr.Src2, "%ebx", "%ecx")
		switch instr.Op {
		case IRAdd:
			w.WriteString("    addl %ebx, %eax\n    adcl %ecx, %edx\n")
		case IRSub:
			w.WriteString("    subl %ebx, %eax\n    sbbl %ecx, %edx\n")
		case IRAnd:
			w.WriteString("    andl %ebx, %eax\n    andl %ecx, %edx\n")
		case IROr:
			w.WriteString("    orl %ebx, %eax\n    orl %ecx, %edx\n")
		case IRXor:
			w.WriteString("    xorl %ebx, %eax\n    xorl %ecx, %edx\n")
		case IRMul:
			w.WriteString("    imull %eax, %ecx\n    imull %ebx, %edx\n    addl %edx, %ecx\n    mull %ebx\n    addl %ecx, %edx\n")
		}
		e.wideResult(instr.Dst)
	case IRNeg, IRBitNot, IRNot, IRFNeg:
		e.wideLoad(instr.Src1, "%eax", "%edx")
		switch instr.Op {
		case IRNeg:
			w.WriteString("    negl %eax\n    adcl $0, %edx\n    negl %edx\n")
		case IRBitNot:
			w.WriteString("    notl %eax\n    notl %edx\n")
		case IRNot:
			w.WriteString("    orl %edx, %eax\n    sete %al\n    movzbl %al, %eax\n    movl $0, %edx\n")
		case IRFNeg:
			w.WriteString("    xorl $-2147483648, %edx\n")
		}
		e.wideResult(instr.Dst)
	case IRDiv, IRMod, IRUDiv, IRUMod:
		e.emitWideDiv(instr)
	case IRShl, IRShr, IRUShr:
		e.emitWideShift(instr)
	case IRCmpEq, IRCmpNe, IRCmpLt, IRCmpLe, IRCmpGt, IRCmpGe, IRUCmpLt, IRUCmpLe, IRUCmpGt, IRUCmpGe:
		e.emitWideCmp(instr)
	case IRNarrow:
		e.wideLoad(instr.Src1, "%eax", "%edx")
		switch instr.Src2.Imm {
		case 8:
			w.WriteString("    movsbl %al, %eax\n    cltd\n")
		case 16:
			w.WriteString("    movswl %ax, %eax\n    cltd\n")
		case 32:
			w.WriteString("    cltd\n")
		case -8:
			w.WriteString("    movzbl %al, %eax\n    movl $0, %edx\n")
		case -16:
			w.WriteString("    movzwl %ax, %eax\n    movl $0, %edx\n")
		case -32:
			w.WriteString("    movl $0, %edx\n")
		}
		e.wideResult(instr.Dst)
	case IRLoad8, IRLoad32, IRLoad64:
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n", e.widePart(instr.Src1, false)))
		switch instr.Op {
		case IRLoad8:
			w.WriteString("    movzbl (%ecx), %eax\n    movl $0, %edx\n")
		case IRLoad32:
			w.WriteString("    movl (%ecx), %eax\n    cltd\n")
		case IRLoad64:
			w.WriteString("    movl (%ecx), %eax\n    movl 4(%ecx), %edx\n")
		}
		e.wideResult(instr.Dst)
	case IRStoreByte:
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n    movl %s, %%eax\n    movb %%al, (%%ecx)\n", e.widePart(instr.Dst, false), e.widePart(instr.Src1, false)))
	case IRLoadGlobal:
		label := e.target.Sym(instr.Src1.Label)
		w.WriteString(fmt.Sprintf("    movl %s, %%eax\n    movl %s+4, %%edx\n", label, label))
		e.wideResult(instr.Dst)
	case IRStoreGlobal:
		label := e.target.Sym(instr.Dst.Label)
		e.wideLoad(instr.Src1, "%eax", "%edx")
		w.WriteString(fmt.Sprintf("    movl %%eax, %s\n    movl %%edx, %s+4\n", label, label))
	case IRGetReg:
		e.wideStore(instr.Dst, e.operand(instr.Src1), "$0")
	case IRSetReg:
		w.WriteString(fmt.Sprintf("    movl %s, %s\n", e.widePart(instr.Src1, false), e.operand(instr.Dst)))
	case IRPop:
		w.WriteString(fmt.Sprintf("    popl %s\n", e.operand(instr.Dst)))
		if instr.Dst.Kind != OpPhysReg {
			w.WriteString(fmt.Sprintf("    movl $0, %s\n", e.widePart(instr.Dst, true)))
		}
	case IRJmpIf, IRJmpNot:
		e.wideLoad(instr.Src1, "%eax", "%edx")
		w.WriteString("    orl %edx, %eax\n")
		branch := "jne"
		if instr.Op == IRJmpNot {
			branch = "je"
		}
		w.WriteString(fmt.Sprintf("    %s %s\n", branch, instr.Dst.Label))
	case IRCall:
		e.emitWideCall(instr)
	case IRHeapAlloc:
		w.WriteString(fmt.Sprintf("    pushl %s\n    pushl %s\n    call _novus_gc_alloc\n    addl $8, %%esp\n", e.widePart(instr.Src1, true), e.widePart(instr.Src1, false)))
		e.wideResult(instr.Dst)
	case IRRet:
		if instr.Src1.Kind != OpNone {
			e.wideLoad(instr.Src1, "%eax", "%edx")
		}
		frame := e.computeFrameSize(e.fn)
		w.WriteString(fmt.Sprintf("    leal -%d(%%ebp), %%esp\n    popl %%edi\n    popl %%esi\n    popl %%ebx\n    movl %%ebp, %%esp\n    popl %%ebp\n    ret\n", frame+12))
	case IRArrayNew, IRArrayGet, IRArraySet, IRArrayAppend, IRArrayPop, IRArrayLen:
		e.emitWideArray(instr)
	case IRFAdd, IRFSub, IRFMul, IRFDiv, IRFMod, IRFCmpEq, IRFCmpNe, IRFCmpLt, IRFCmpLe, IRFCmpGt, IRFCmpGe, IRIntToFloat, IRFloatToInt:
		e.emitWideFloat(instr)
	default:
		return false
	}
	return true
}

func (e *x86Emitter) emitWideCall(instr IRInstr) {
	for i := len(instr.Args) - 1; i >= 0; i-- {
		e.b.WriteString(fmt.Sprintf("    pushl %s\n    pushl %s\n", e.widePart(instr.Args[i], true), e.widePart(instr.Args[i], false)))
	}
	e.b.WriteString(fmt.Sprintf("    call %s\n", instr.Src1.Label))
	if len(instr.Args) > 0 {
		e.b.WriteString(fmt.Sprintf("    addl $%d, %%esp\n", len(instr.Args)*8))
	}
	e.wideResult(instr.Dst)
}

func (e *x86Emitter) emitWideShift(instr IRInstr) {
	w := e.b
	id := e.uniqueID()
	e.wideLoad(instr.Src1, "%eax", "%edx")
	w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n    andl $63, %%ecx\n    cmpl $32, %%ecx\n    jae .Lwide_shift_large_%d\n", e.widePart(instr.Src2, false), id))
	switch instr.Op {
	case IRShl:
		w.WriteString("    shldl %cl, %eax, %edx\n    shll %cl, %eax\n")
	case IRShr:
		w.WriteString("    shrdl %cl, %edx, %eax\n    sarl %cl, %edx\n")
	case IRUShr:
		w.WriteString("    shrdl %cl, %edx, %eax\n    shrl %cl, %edx\n")
	}
	w.WriteString(fmt.Sprintf("    jmp .Lwide_shift_done_%d\n.Lwide_shift_large_%d:\n", id, id))
	switch instr.Op {
	case IRShl:
		w.WriteString("    movl %eax, %edx\n    shll %cl, %edx\n    movl $0, %eax\n")
	case IRShr:
		w.WriteString("    movl %edx, %eax\n    sarl %cl, %eax\n    sarl $31, %edx\n")
	case IRUShr:
		w.WriteString("    movl %edx, %eax\n    shrl %cl, %eax\n    movl $0, %edx\n")
	}
	w.WriteString(fmt.Sprintf(".Lwide_shift_done_%d:\n", id))
	e.wideResult(instr.Dst)
}

func (e *x86Emitter) emitWideCmp(instr IRInstr) {
	w := e.b
	id := e.uniqueID()
	e.wideLoad(instr.Src1, "%eax", "%edx")
	e.wideLoad(instr.Src2, "%ebx", "%ecx")
	high, low := "e", "e"
	switch instr.Op {
	case IRCmpNe:
		high, low = "ne", "ne"
	case IRCmpLt:
		high, low = "l", "b"
	case IRCmpLe:
		high, low = "le", "be"
	case IRCmpGt:
		high, low = "g", "a"
	case IRCmpGe:
		high, low = "ge", "ae"
	case IRUCmpLt:
		high, low = "b", "b"
	case IRUCmpLe:
		high, low = "be", "be"
	case IRUCmpGt:
		high, low = "a", "a"
	case IRUCmpGe:
		high, low = "ae", "ae"
	}
	w.WriteString(fmt.Sprintf("    cmpl %%ecx, %%edx\n    jne .Lwide_cmp_high_%d\n    cmpl %%ebx, %%eax\n    set%s %%al\n    jmp .Lwide_cmp_done_%d\n.Lwide_cmp_high_%d:\n    set%s %%al\n.Lwide_cmp_done_%d:\n    movzbl %%al, %%eax\n    movl $0, %%edx\n", id, low, id, id, high, id))
	e.wideResult(instr.Dst)
}

func (e *x86Emitter) emitWideDiv(instr IRInstr) {
	w := e.b
	id := e.uniqueID()
	e.wideLoad(instr.Src1, "%eax", "%edx")
	e.wideLoad(instr.Src2, "%ebx", "%ecx")
	signed := instr.Op == IRDiv || instr.Op == IRMod
	remainder := instr.Op == IRMod || instr.Op == IRUMod
	if signed {
		w.WriteString("    movl %edx, %esi\n    sarl $31, %esi\n    movl %ecx, %edi\n    sarl $31, %edi\n    xorl %esi, %edi\n    pushl %esi\n    pushl %edi\n")
		w.WriteString(fmt.Sprintf("    testl %%edx, %%edx\n    jns .Lwide_div_a_%d\n    negl %%eax\n    adcl $0, %%edx\n    negl %%edx\n.Lwide_div_a_%d:\n    testl %%ecx, %%ecx\n    jns .Lwide_div_b_%d\n    negl %%ebx\n    adcl $0, %%ecx\n    negl %%ecx\n.Lwide_div_b_%d:\n", id, id, id, id))
	}
	w.WriteString("    call _novus_udivmod64\n")
	if remainder {
		w.WriteString("    movl %ebx, %eax\n    movl %ecx, %edx\n")
	}
	if signed {
		w.WriteString("    popl %edi\n    popl %esi\n")
		sign := "%edi"
		if remainder {
			sign = "%esi"
		}
		w.WriteString(fmt.Sprintf("    testl %s, %s\n    jz .Lwide_div_done_%d\n    negl %%eax\n    adcl $0, %%edx\n    negl %%edx\n.Lwide_div_done_%d:\n", sign, sign, id, id))
	}
	e.wideResult(instr.Dst)
}

// Register-only integer arithmetic support. Fast-path division by a single
// machine word also covers the collector's indexing and ordinary loop counts.
func (e *x86Emitter) emitWideDivisionRuntime() {
	e.b.WriteString(`
_novus_udivmod64:
    pushl %esi
    pushl %edi
    testl %ecx, %ecx
    jnz .Ludiv64_long
    movl %eax, %esi
    movl %edx, %eax
    xorl %edx, %edx
    divl %ebx
    movl %eax, %edi
    movl %esi, %eax
    divl %ebx
    movl %edx, %ebx
    movl %edi, %edx
    movl $0, %ecx
    jmp .Ludiv64_done
.Ludiv64_long:
    xorl %esi, %esi
    xorl %edi, %edi
    pushl $64
.Ludiv64_loop:
    shll $1, %eax
    rcll $1, %edx
    rcll $1, %esi
    rcll $1, %edi
    jc .Ludiv64_sub
    cmpl %ecx, %edi
    jb .Ludiv64_next
    ja .Ludiv64_sub
    cmpl %ebx, %esi
    jb .Ludiv64_next
.Ludiv64_sub:
    subl %ebx, %esi
    sbbl %ecx, %edi
    orl $1, %eax
.Ludiv64_next:
    decl (%esp)
    jnz .Ludiv64_loop
    addl $4, %esp
    movl %esi, %ebx
    movl %edi, %ecx
.Ludiv64_done:
    popl %edi
    popl %esi
    ret
`)
}

func (e *x86Emitter) emitWideFloat(instr IRInstr) {
	w := e.b
	e.wideLoad(instr.Src1, "%eax", "%edx")
	w.WriteString("    subl $24, %esp\n    movl %eax, (%esp)\n    movl %edx, 4(%esp)\n")
	if instr.Op == IRIntToFloat {
		w.WriteString("    fildll (%esp)\n    fstpl (%esp)\n")
	} else if instr.Op == IRFloatToInt {
		w.WriteString("    fnstcw 16(%esp)\n    movzwl 16(%esp), %eax\n    orl $3072, %eax\n    movw %ax, 18(%esp)\n    fldcw 18(%esp)\n    fldl (%esp)\n    fistpll (%esp)\n    fldcw 16(%esp)\n")
	} else {
		e.wideLoad(instr.Src2, "%eax", "%edx")
		w.WriteString("    movl %eax, 8(%esp)\n    movl %edx, 12(%esp)\n")
		if instr.Op == IRFMod {
			id := e.uniqueID()
			w.WriteString(fmt.Sprintf("    fldl 8(%%esp)\n    fldl (%%esp)\n.Lfmod32_%d:\n    fprem\n    fnstsw %%ax\n    testw $1024, %%ax\n    jnz .Lfmod32_%d\n    fstpl (%%esp)\n    fstp %%st(0)\n", id, id))
		} else {
			w.WriteString("    movsd (%esp), %xmm0\n    movsd 8(%esp), %xmm1\n")
			mnemonic := ""
			switch instr.Op {
			case IRFAdd:
				mnemonic = "addsd"
			case IRFSub:
				mnemonic = "subsd"
			case IRFMul:
				mnemonic = "mulsd"
			case IRFDiv:
				mnemonic = "divsd"
			}
			if mnemonic != "" {
				w.WriteString(fmt.Sprintf("    %s %%xmm1, %%xmm0\n    movsd %%xmm0, (%%esp)\n", mnemonic))
			} else {
				w.WriteString("    ucomisd %xmm1, %xmm0\n")
				switch instr.Op {
				case IRFCmpEq:
					w.WriteString("    sete %al\n    setnp %cl\n    andb %cl, %al\n")
				case IRFCmpNe:
					w.WriteString("    setne %al\n    setp %cl\n    orb %cl, %al\n")
				case IRFCmpLt:
					w.WriteString("    setb %al\n    setnp %cl\n    andb %cl, %al\n")
				case IRFCmpLe:
					w.WriteString("    setbe %al\n    setnp %cl\n    andb %cl, %al\n")
				case IRFCmpGt:
					w.WriteString("    seta %al\n")
				case IRFCmpGe:
					w.WriteString("    setae %al\n")
				}
				w.WriteString("    movzbl %al, %eax\n    movl %eax, (%esp)\n    movl $0, 4(%esp)\n")
			}
		}
	}
	w.WriteString("    movl (%esp), %eax\n    movl 4(%esp), %edx\n    addl $24, %esp\n")
	e.wideResult(instr.Dst)
}

func (e *x86Emitter) emitWideArray(instr IRInstr) {
	w := e.b
	switch instr.Op {
	case IRArrayNew:
		capacity := instr.Src2.Imm
		if capacity < 4 {
			capacity = 4
		}
		w.WriteString(fmt.Sprintf("    pushl $0\n    pushl $%d\n    call _novus_gc_alloc\n    addl $8, %%esp\n    leal 12(%%eax), %%ecx\n    movl %%ecx, (%%eax)\n    movl $0, 4(%%eax)\n    movl $%d, 8(%%eax)\n    movl $0, %%edx\n", 12+capacity*8, capacity))
		e.wideResult(instr.Dst)
	case IRArrayGet:
		w.WriteString(fmt.Sprintf("    movl %s, %%ebx\n    movl %s, %%ecx\n    movl (%%ebx), %%ebx\n    movl (%%ebx,%%ecx,8), %%eax\n    movl 4(%%ebx,%%ecx,8), %%edx\n", e.widePart(instr.Src1, false), e.widePart(instr.Src2, false)))
		e.wideResult(instr.Dst)
	case IRArraySet:
		w.WriteString(fmt.Sprintf("    movl %s, %%ebx\n    movl %s, %%ecx\n    movl (%%ebx), %%ebx\n", e.widePart(instr.Dst, false), e.widePart(instr.Src1, false)))
		e.wideLoad(instr.Src2, "%eax", "%edx")
		w.WriteString("    movl %eax, (%ebx,%ecx,8)\n    movl %edx, 4(%ebx,%ecx,8)\n")
	case IRArrayLen:
		w.WriteString(fmt.Sprintf("    movl %s, %%ecx\n    movl 4(%%ecx), %%eax\n    movl $0, %%edx\n", e.widePart(instr.Src1, false)))
		e.wideResult(instr.Dst)
	case IRArrayPop:
		w.WriteString(fmt.Sprintf("    movl %s, %%ebx\n    movl 4(%%ebx), %%ecx\n    decl %%ecx\n    movl %%ecx, 4(%%ebx)\n    movl (%%ebx), %%ebx\n    movl (%%ebx,%%ecx,8), %%eax\n    movl 4(%%ebx,%%ecx,8), %%edx\n", e.widePart(instr.Src1, false)))
		e.wideResult(instr.Dst)
	case IRArrayAppend:
		id := e.uniqueID()
		w.WriteString(fmt.Sprintf("    movl %s, %%ebx\n    movl 4(%%ebx), %%ecx\n    cmpl 8(%%ebx), %%ecx\n    jb .Larray32_room_%d\n    movl 8(%%ebx), %%esi\n    shll $1, %%esi\n    leal (,%%esi,8), %%eax\n    pushl $0\n    pushl %%eax\n    call _novus_gc_alloc\n    addl $8, %%esp\n    movl %%esi, 8(%%ebx)\n    movl (%%ebx), %%esi\n    movl %%eax, (%%ebx)\n    movl %%eax, %%edi\n    movl 4(%%ebx), %%ecx\n    shll $1, %%ecx\n    cld\n    rep movsl\n.Larray32_room_%d:\n    movl 4(%%ebx), %%ecx\n    movl (%%ebx), %%ebx\n", e.widePart(instr.Dst, false), id, id))
		e.wideLoad(instr.Src1, "%eax", "%edx")
		w.WriteString("    movl %eax, (%ebx,%ecx,8)\n    movl %edx, 4(%ebx,%ecx,8)\n")
		w.WriteString(fmt.Sprintf("    movl %s, %%ebx\n    incl 4(%%ebx)\n", e.widePart(instr.Dst, false)))
	}
}
