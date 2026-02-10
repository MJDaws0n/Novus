package codegen

import (
	"fmt"
	"novus/internal/ast"
	"strconv"
	"strings"
)

// ---------------------------------------------------------------------------
// Lowerer — translates an AST Program into an IRModule
// ---------------------------------------------------------------------------

// Lowerer walks the AST and produces IR instructions.
type Lowerer struct {
	module *IRModule
	target *Target
	fn     *IRFunc // current function being lowered

	// Virtual register allocator — each expression result gets a fresh vreg.
	nextVReg int

	// Label counter for generating unique labels.
	nextLabel int

	// Variable table: maps variable names to stack slot offsets (from rbp/frame pointer).
	// Each function gets its own table.
	vars     map[string]int // name → slot index (0-based)
	nextSlot int            // next slot index

	// Type tracking for string-aware lowering.
	varTypes     map[string]string // variable name → declared type (e.g. "str", "i32")
	vregIsStr    map[int]bool      // vreg number → true if it holds a string pointer
	funcRetTypes map[string]string // function name → return type (pre-scanned)

	// Loop context for break/continue.
	loopBreakLabel    string
	loopContinueLabel string

	// Physical register lookup: which Novus identifiers are physical registers.
	physRegs map[string]bool
}

// Lower translates an AST Program into an IRModule for the given target.
func Lower(program *ast.Program, target *Target) *IRModule {
	// Pre-scan function signatures to know return types.
	funcRetTypes := map[string]string{}
	for _, fn := range program.Functions {
		if fn.ReturnType != nil {
			// Use MangledName if set (for overloaded functions), otherwise plain Name.
			key := fn.MangledName
			if key == "" {
				key = fn.Name
			}
			funcRetTypes[key] = fn.ReturnType.Name
		}
	}

	l := &Lowerer{
		module:       &IRModule{EntryFunc: "main"},
		target:       target,
		physRegs:     makePhysRegSet(),
		funcRetTypes: funcRetTypes,
	}

	for _, fn := range program.Functions {
		l.lowerFunction(fn)
	}

	return l.module
}

// makePhysRegSet builds the set of all physical register names recognized by
// Novus (same list as semantic.reservedRegisters).
func makePhysRegSet() map[string]bool {
	regs := map[string]bool{
		// x86 32-bit general purpose
		"eax": true, "ebx": true, "ecx": true, "edx": true,
		"esi": true, "edi": true, "ebp": true, "esp": true,
		// x86 64-bit general purpose
		"rax": true, "rbx": true, "rcx": true, "rdx": true,
		"rsi": true, "rdi": true, "rbp": true, "rsp": true,
		"r8": true, "r9": true, "r10": true, "r11": true,
		"r12": true, "r13": true, "r14": true, "r15": true,
		// x86 Instruction pointers
		"eip": true, "rip": true,
		// x86 Flags
		"eflags": true, "rflags": true,
		// x87 FPU
		"st0": true, "st1": true, "st2": true, "st3": true,
		"st4": true, "st5": true, "st6": true, "st7": true,
		// MMX
		"mm0": true, "mm1": true, "mm2": true, "mm3": true,
		"mm4": true, "mm5": true, "mm6": true, "mm7": true,
		// SSE
		"xmm0": true, "xmm1": true, "xmm2": true, "xmm3": true,
		"xmm4": true, "xmm5": true, "xmm6": true, "xmm7": true,
		"xmm8": true, "xmm9": true, "xmm10": true, "xmm11": true,
		"xmm12": true, "xmm13": true, "xmm14": true, "xmm15": true,
		// ARM64 64-bit general purpose
		"x0": true, "x1": true, "x2": true, "x3": true,
		"x4": true, "x5": true, "x6": true, "x7": true,
		"x8": true, "x9": true, "x10": true, "x11": true,
		"x12": true, "x13": true, "x14": true, "x15": true,
		"x16": true, "x17": true, "x18": true, "x19": true,
		"x20": true, "x21": true, "x22": true, "x23": true,
		"x24": true, "x25": true, "x26": true, "x27": true,
		"x28": true, "x29": true, "x30": true,
		// ARM64 32-bit general purpose
		"w0": true, "w1": true, "w2": true, "w3": true,
		"w4": true, "w5": true, "w6": true, "w7": true,
		"w8": true, "w9": true, "w10": true, "w11": true,
		"w12": true, "w13": true, "w14": true, "w15": true,
		"w16": true, "w17": true, "w18": true, "w19": true,
		"w20": true, "w21": true, "w22": true, "w23": true,
		"w24": true, "w25": true, "w26": true, "w27": true,
		"w28": true, "w29": true, "w30": true,
		// ARM64 special registers
		"sp": true, "xzr": true, "wzr": true, "lr": true,
	}
	return regs
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

func (l *Lowerer) freshVReg() int {
	r := l.nextVReg
	l.nextVReg++
	return r
}

func (l *Lowerer) freshLabel(prefix string) string {
	lbl := fmt.Sprintf(".L%s_%d", prefix, l.nextLabel)
	l.nextLabel++
	return lbl
}

func (l *Lowerer) emit(instr IRInstr) {
	l.fn.Emit(instr)
}

func (l *Lowerer) emitComment(text string) {
	l.emit(IRInstr{Op: IRComment, Src1: LabelOp(text)})
}

// allocVar allocates a stack slot for a local variable and returns the slot index.
func (l *Lowerer) allocVar(name string) int {
	slot := l.nextSlot
	l.vars[name] = slot
	l.nextSlot++
	if l.nextSlot > l.fn.Locals {
		l.fn.Locals = l.nextSlot
	}
	return slot
}

// varSlot returns the stack slot for a variable, or -1 if not found.
func (l *Lowerer) varSlot(name string) int {
	if slot, ok := l.vars[name]; ok {
		return slot
	}
	return -1
}

// slotMem returns a memory operand addressing the given stack slot.
// Slots are addressed as [rbp - (slot+1)*ptrSize].
func (l *Lowerer) slotMem(slot int) Operand {
	offset := -int64((slot + 1) * l.target.PtrSize)
	return Mem(l.target.BasePointer, offset)
}

// ---------------------------------------------------------------------------
// Function lowering
// ---------------------------------------------------------------------------

func (l *Lowerer) lowerFunction(fn *ast.FnDecl) {
	// Use the mangled name for the IR label (handles overloaded functions).
	irName := fn.MangledName
	if irName == "" {
		irName = fn.Name
	}

	irFn := &IRFunc{
		Name:       irName,
		ParamCount: len(fn.Params),
		ParamNames: make([]string, len(fn.Params)),
	}
	l.fn = irFn
	l.nextVReg = 0
	l.nextSlot = 0
	l.vars = make(map[string]int)
	l.loopBreakLabel = ""
	l.loopContinueLabel = ""
	l.varTypes = map[string]string{}
	l.vregIsStr = map[int]bool{}

	// Function prologue label.
	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(irName)})
	l.emitComment(fmt.Sprintf("function %s", fn.Name))

	// Allocate stack slots for parameters and store them.
	for i, param := range fn.Params {
		irFn.ParamNames[i] = param.Name
		if param.Type != nil {
			l.varTypes[param.Name] = param.Type.Name
		}
		slot := l.allocVar(param.Name)
		// Parameters are passed in registers (on x86-64 SysV) or stack.
		// We'll store them into their local slots.
		if i < len(l.target.ArgRegs) {
			// Parameter is in a register — store it to the stack slot.
			l.emit(IRInstr{
				Op:   IRStore,
				Dst:  l.slotMem(slot),
				Src1: PReg(l.target.ArgRegs[i]),
			})
		} else {
			// Parameter is on the stack — load from caller's frame.
			// On x86-64 SysV: first stack arg is at [rbp+16], then +8 each.
			callerOffset := int64(16 + (i-len(l.target.ArgRegs))*l.target.PtrSize)
			tmpReg := l.freshVReg()
			l.emit(IRInstr{
				Op:   IRLoad,
				Dst:  VReg(tmpReg),
				Src1: Mem(l.target.BasePointer, callerOffset),
			})
			l.emit(IRInstr{
				Op:   IRStore,
				Dst:  l.slotMem(slot),
				Src1: VReg(tmpReg),
			})
		}
	}

	// Lower the function body.
	l.lowerBlock(fn.Body)

	// If the last instruction is not a return, emit an implicit return.
	if len(irFn.Instrs) == 0 || irFn.Instrs[len(irFn.Instrs)-1].Op != IRRet {
		l.emit(IRInstr{Op: IRRet})
	}

	// Calculate frame size: locals * ptrSize, aligned to 16 bytes.
	frameSize := irFn.Locals * l.target.PtrSize
	if frameSize%16 != 0 {
		frameSize += 16 - (frameSize % 16)
	}
	irFn.FrameSize = frameSize

	l.module.Functions = append(l.module.Functions, irFn)
}

// ---------------------------------------------------------------------------
// Statement lowering
// ---------------------------------------------------------------------------

func (l *Lowerer) lowerBlock(block *ast.BlockStmt) {
	for _, stmt := range block.Stmts {
		l.lowerStmt(stmt)
	}
}

func (l *Lowerer) lowerStmt(stmt ast.Stmt) {
	switch s := stmt.(type) {
	case *ast.LetStmt:
		l.lowerLetStmt(s)
	case *ast.ReturnStmt:
		l.lowerReturnStmt(s)
	case *ast.IfStmt:
		l.lowerIfStmt(s)
	case *ast.WhileStmt:
		l.lowerWhileStmt(s)
	case *ast.ForStmt:
		l.lowerForStmt(s)
	case *ast.ExprStmt:
		l.lowerExpr(s.Expression)
	case *ast.AssignStmt:
		l.lowerAssignStmt(s)
	case *ast.BlockStmt:
		l.lowerBlock(s)
	case *ast.BreakStmt:
		if l.loopBreakLabel != "" {
			l.emit(IRInstr{Op: IRJmp, Dst: LabelOp(l.loopBreakLabel)})
		}
	case *ast.ContinueStmt:
		if l.loopContinueLabel != "" {
			l.emit(IRInstr{Op: IRJmp, Dst: LabelOp(l.loopContinueLabel)})
		}
	}
}

func (l *Lowerer) lowerLetStmt(s *ast.LetStmt) {
	slot := l.allocVar(s.Name)
	if s.Type != nil {
		l.varTypes[s.Name] = s.Type.Name
	}
	valOp := l.lowerExpr(s.Value)
	l.emit(IRInstr{
		Op:   IRStore,
		Dst:  l.slotMem(slot),
		Src1: valOp,
	})
	_ = slot
}

func (l *Lowerer) lowerReturnStmt(s *ast.ReturnStmt) {
	if s.Value != nil {
		valOp := l.lowerExpr(s.Value)
		l.emit(IRInstr{Op: IRRet, Src1: valOp})
	} else {
		l.emit(IRInstr{Op: IRRet})
	}
}

func (l *Lowerer) lowerIfStmt(s *ast.IfStmt) {
	condOp := l.lowerExpr(s.Condition)
	elseLabel := l.freshLabel("else")
	endLabel := l.freshLabel("endif")

	if s.Else != nil {
		// if cond == 0, jump to else
		l.emit(IRInstr{Op: IRJmpNot, Src1: condOp, Dst: LabelOp(elseLabel)})
		l.lowerBlock(s.Then)
		l.emit(IRInstr{Op: IRJmp, Dst: LabelOp(endLabel)})
		l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(elseLabel)})
		switch e := s.Else.(type) {
		case *ast.BlockStmt:
			l.lowerBlock(e)
		case *ast.IfStmt:
			l.lowerIfStmt(e)
		}
	} else {
		l.emit(IRInstr{Op: IRJmpNot, Src1: condOp, Dst: LabelOp(endLabel)})
		l.lowerBlock(s.Then)
	}
	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(endLabel)})
}

func (l *Lowerer) lowerWhileStmt(s *ast.WhileStmt) {
	condLabel := l.freshLabel("while_cond")
	bodyLabel := l.freshLabel("while_body")
	endLabel := l.freshLabel("while_end")

	// Save and set loop context.
	prevBreak := l.loopBreakLabel
	prevContinue := l.loopContinueLabel
	l.loopBreakLabel = endLabel
	l.loopContinueLabel = condLabel

	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(condLabel)})
	condOp := l.lowerExpr(s.Condition)
	l.emit(IRInstr{Op: IRJmpNot, Src1: condOp, Dst: LabelOp(endLabel)})
	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(bodyLabel)})
	l.lowerBlock(s.Body)
	l.emit(IRInstr{Op: IRJmp, Dst: LabelOp(condLabel)})
	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(endLabel)})

	l.loopBreakLabel = prevBreak
	l.loopContinueLabel = prevContinue
}

func (l *Lowerer) lowerForStmt(s *ast.ForStmt) {
	condLabel := l.freshLabel("for_cond")
	updateLabel := l.freshLabel("for_update")
	endLabel := l.freshLabel("for_end")

	prevBreak := l.loopBreakLabel
	prevContinue := l.loopContinueLabel
	l.loopBreakLabel = endLabel
	l.loopContinueLabel = updateLabel

	// Init
	if s.Init != nil {
		l.lowerStmt(s.Init)
	}

	// Condition
	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(condLabel)})
	condOp := l.lowerExpr(s.Condition)
	l.emit(IRInstr{Op: IRJmpNot, Src1: condOp, Dst: LabelOp(endLabel)})

	// Body
	l.lowerBlock(s.Body)

	// Update
	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(updateLabel)})
	if s.Update != nil {
		l.lowerStmt(s.Update)
	}
	l.emit(IRInstr{Op: IRJmp, Dst: LabelOp(condLabel)})

	l.emit(IRInstr{Op: IRLabel, Dst: LabelOp(endLabel)})

	l.loopBreakLabel = prevBreak
	l.loopContinueLabel = prevContinue
}

func (l *Lowerer) lowerAssignStmt(s *ast.AssignStmt) {
	valOp := l.lowerExpr(s.Value)

	switch target := s.Target.(type) {
	case *ast.IdentExpr:
		// Check if it's a physical register.
		if l.physRegs[target.Name] {
			l.emit(IRInstr{Op: IRMov, Dst: PReg(target.Name), Src1: valOp})
			return
		}
		// Otherwise it's a local variable.
		slot := l.varSlot(target.Name)
		if slot >= 0 {
			l.emit(IRInstr{Op: IRStore, Dst: l.slotMem(slot), Src1: valOp})
		}
	case *ast.IndexExpr:
		// Check if target is an array or string.
		if ident, ok := target.Object.(*ast.IdentExpr); ok && l.varIsArray(ident.Name) {
			// arr[idx] = val → IRArraySet
			baseOp := l.lowerExpr(target.Object)
			idxOp := l.lowerExpr(target.Index)
			l.emit(IRInstr{Op: IRArraySet, Dst: baseOp, Src1: idxOp, Src2: valOp})
		} else {
			// str[idx] = val → compute base+idx, store a single byte.
			baseOp := l.lowerExpr(target.Object)
			idxOp := l.lowerExpr(target.Index)
			addrReg := l.freshVReg()
			l.emit(IRInstr{Op: IRAdd, Dst: VReg(addrReg), Src1: baseOp, Src2: idxOp})
			l.emit(IRInstr{Op: IRStoreByte, Dst: VReg(addrReg), Src1: valOp})
		}
	}
}

// ---------------------------------------------------------------------------
// Expression lowering — returns an Operand holding the result
// ---------------------------------------------------------------------------

func (l *Lowerer) lowerExpr(expr ast.Expr) Operand {
	if expr == nil {
		return None()
	}

	switch e := expr.(type) {
	case *ast.IntLitExpr:
		return l.lowerIntLit(e)
	case *ast.FloatLitExpr:
		return l.lowerFloatLit(e)
	case *ast.StringLitExpr:
		return l.lowerStringLit(e)
	case *ast.BoolLitExpr:
		if e.Value {
			return Imm(1)
		}
		return Imm(0)
	case *ast.IdentExpr:
		return l.lowerIdentExpr(e)
	case *ast.UnaryExpr:
		return l.lowerUnaryExpr(e)
	case *ast.BinaryExpr:
		return l.lowerBinaryExpr(e)
	case *ast.CallExpr:
		return l.lowerCallExpr(e)
	case *ast.GroupExpr:
		return l.lowerExpr(e.Expression)
	case *ast.IndexExpr:
		return l.lowerIndexExpr(e)
	case *ast.AddressOfExpr:
		return l.lowerAddressOfExpr(e)
	case *ast.ArrayLitExpr:
		return l.lowerArrayLitExpr(e)
	case *ast.MemberExpr:
		// Member expressions (import aliases) — return a label if possible.
		if ident, ok := e.Object.(*ast.IdentExpr); ok {
			return LabelOp(ident.Name + "." + e.Field)
		}
		return None()
	}
	return None()
}

func (l *Lowerer) lowerIntLit(e *ast.IntLitExpr) Operand {
	val, err := strconv.ParseInt(e.Value, 0, 64)
	if err != nil {
		// Try unsigned
		uval, err2 := strconv.ParseUint(e.Value, 0, 64)
		if err2 != nil {
			return Imm(0)
		}
		return Imm(int64(uval))
	}
	return Imm(val)
}

func (l *Lowerer) lowerFloatLit(e *ast.FloatLitExpr) Operand {
	// For now, truncate to integer. Full float support requires FP registers.
	val, err := strconv.ParseFloat(e.Value, 64)
	if err != nil {
		return Imm(0)
	}
	return Imm(int64(val))
}

func (l *Lowerer) lowerStringLit(e *ast.StringLitExpr) Operand {
	// Strip quotes and handle escapes.
	lexeme := e.Value
	isSingleQuoted := len(lexeme) >= 2 && lexeme[0] == '\''

	raw := lexeme
	if len(raw) >= 2 && (raw[0] == '"' || raw[0] == '\'') {
		raw = raw[1 : len(raw)-1]
	}
	raw = unescapeString(raw)

	// Only single-quoted literals that decode to a single byte are treated as
	// immediate byte values (used for comparisons with str_index results).
	// Double-quoted strings (even length 1 like "\n") remain real strings.
	if isSingleQuoted && len(raw) == 1 {
		return Imm(int64(raw[0]))
	}

	idx := l.module.AddString(raw)
	return StrRef(idx)
}

func (l *Lowerer) lowerIdentExpr(e *ast.IdentExpr) Operand {
	// Physical register?
	if l.physRegs[e.Name] {
		return PReg(e.Name)
	}
	// Local variable?
	slot := l.varSlot(e.Name)
	if slot >= 0 {
		dst := l.freshVReg()
		l.emit(IRInstr{Op: IRLoad, Dst: VReg(dst), Src1: l.slotMem(slot)})
		if l.varTypes[e.Name] == "str" {
			l.vregIsStr[dst] = true
		}
		return VReg(dst)
	}
	// Could be a function name (for call targets) — return as label.
	return LabelOp(e.Name)
}

func (l *Lowerer) lowerUnaryExpr(e *ast.UnaryExpr) Operand {
	operand := l.lowerExpr(e.Operand)
	dst := l.freshVReg()
	switch e.Op {
	case "-":
		l.emit(IRInstr{Op: IRNeg, Dst: VReg(dst), Src1: operand})
	case "!":
		l.emit(IRInstr{Op: IRNot, Dst: VReg(dst), Src1: operand})
	default:
		return operand
	}
	return VReg(dst)
}

// operandIsStr reports whether an operand holds a string pointer.
func (l *Lowerer) operandIsStr(op Operand) bool {
	switch op.Kind {
	case OpStringRef:
		return true
	case OpVirtReg:
		return l.vregIsStr[op.Reg]
	}
	return false
}

// varIsArray reports whether a variable is an array type.
func (l *Lowerer) varIsArray(name string) bool {
	t := l.varTypes[name]
	return len(t) > 2 && t[:2] == "[]"
}

// charToStr converts a byte value (e.g. from str_index) into a 1-char
// null-terminated string by storing it on the stack and returning the address.
// On little-endian systems, storing a byte value as a 64-bit qword naturally
// places the byte at offset 0 and zeros (null terminators) at offsets 1-7.
func (l *Lowerer) charToStr(byteOp Operand) Operand {
	slot := l.allocVar(fmt.Sprintf("_c2s_%d", l.nextVReg))
	l.emit(IRInstr{Op: IRStore, Dst: l.slotMem(slot), Src1: byteOp})
	reg := l.freshVReg()
	l.emit(IRInstr{Op: IRLea, Dst: VReg(reg), Src1: l.slotMem(slot)})
	l.vregIsStr[reg] = true
	return VReg(reg)
}

func (l *Lowerer) lowerBinaryExpr(e *ast.BinaryExpr) Operand {
	left := l.lowerExpr(e.Left)
	right := l.lowerExpr(e.Right)

	// String concatenation: if at least one operand is a string and op is +.
	if e.Op == "+" {
		leftStr := l.operandIsStr(left)
		rightStr := l.operandIsStr(right)

		if leftStr || rightStr {
			// Compile-time constant folding when both are literal string refs.
			if left.Kind == OpStringRef && right.Kind == OpStringRef {
				li := int(left.Imm)
				ri := int(right.Imm)
				if li >= 0 && li < len(l.module.Strings) && ri >= 0 && ri < len(l.module.Strings) {
					combined := l.module.Strings[li].Value + l.module.Strings[ri].Value
					idx := l.module.AddString(combined)
					return StrRef(idx)
				}
			}
			// Convert non-string operand (byte) to a 1-char string if needed.
			if !leftStr {
				left = l.charToStr(left)
			}
			if !rightStr {
				right = l.charToStr(right)
			}
			// Runtime concatenation.
			dst := l.freshVReg()
			l.vregIsStr[dst] = true
			l.emit(IRInstr{Op: IRStrConcat, Dst: VReg(dst), Src1: left, Src2: right})
			return VReg(dst)
		}
	}
	dst := l.freshVReg()

	var op IROp
	switch e.Op {
	case "+":
		op = IRAdd
	case "-":
		op = IRSub
	case "*":
		op = IRMul
	case "/":
		op = IRDiv
	case "%":
		op = IRMod
	case "==":
		op = IRCmpEq
	case "!=":
		op = IRCmpNe
	case "<":
		op = IRCmpLt
	case "<=":
		op = IRCmpLe
	case ">":
		op = IRCmpGt
	case ">=":
		op = IRCmpGe
	case "&&":
		op = IRAnd
	case "||":
		op = IROr
	default:
		return left
	}

	l.emit(IRInstr{Op: op, Dst: VReg(dst), Src1: left, Src2: right})
	return VReg(dst)
}

func (l *Lowerer) lowerCallExpr(e *ast.CallExpr) Operand {
	// Determine callee name.
	calleeName := ""
	switch c := e.Callee.(type) {
	case *ast.IdentExpr:
		calleeName = c.Name
	case *ast.MemberExpr:
		// For import-aliased calls, use the resolved callee set by semantic analysis.
		if e.ResolvedCallee != "" {
			calleeName = e.ResolvedCallee
		} else {
			calleeName = ast.ExprString(c)
		}
	default:
		calleeName = ast.ExprString(e.Callee)
	}

	// Check if this is a built-in intrinsic.
	if handler, ok := l.builtinHandlers()[calleeName]; ok {
		return handler(e)
	}

	// Use the resolved callee name from semantic analysis (handles overloads).
	resolvedName := calleeName
	if e.ResolvedCallee != "" {
		resolvedName = e.ResolvedCallee
	}

	// Regular function call — lower arguments, emit call.
	var argOps []Operand
	for _, arg := range e.Args {
		argOps = append(argOps, l.lowerExpr(arg))
	}

	dst := l.freshVReg()
	l.emit(IRInstr{
		Op:   IRCall,
		Dst:  VReg(dst),
		Src1: LabelOp(resolvedName),
		Args: argOps,
	})
	if l.funcRetTypes[resolvedName] == "str" {
		l.vregIsStr[dst] = true
	}
	return VReg(dst)
}

func (l *Lowerer) lowerIndexExpr(e *ast.IndexExpr) Operand {
	// Determine if the object is an array or string.
	if ident, ok := e.Object.(*ast.IdentExpr); ok && l.varIsArray(ident.Name) {
		// Array indexing: arr[i] → IRArrayGet
		objOp := l.lowerExpr(e.Object)
		idxOp := l.lowerExpr(e.Index)
		dst := l.freshVReg()
		l.emit(IRInstr{Op: IRArrayGet, Dst: VReg(dst), Src1: objOp, Src2: idxOp})
		return VReg(dst)
	}
	// String indexing: str[i] → IRStrIndex
	objOp := l.lowerExpr(e.Object)
	idxOp := l.lowerExpr(e.Index)
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRStrIndex, Dst: VReg(dst), Src1: objOp, Src2: idxOp})
	return VReg(dst)
}

func (l *Lowerer) lowerAddressOfExpr(e *ast.AddressOfExpr) Operand {
	// &variable — load the address of the variable's stack slot.
	if ident, ok := e.Operand.(*ast.IdentExpr); ok {
		slot := l.varSlot(ident.Name)
		if slot >= 0 {
			dst := l.freshVReg()
			l.emit(IRInstr{Op: IRLea, Dst: VReg(dst), Src1: l.slotMem(slot)})
			return VReg(dst)
		}
	}
	return l.lowerExpr(e.Operand)
}

func (l *Lowerer) lowerArrayLitExpr(e *ast.ArrayLitExpr) Operand {
	// Create array with initial capacity = max(len(elems), 4).
	cap := len(e.Elems)
	if cap < 4 {
		cap = 4
	}
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRArrayNew, Dst: VReg(dst), Src1: Imm(8), Src2: Imm(int64(cap))})
	// Append each initial element.
	for _, elem := range e.Elems {
		elemOp := l.lowerExpr(elem)
		l.emit(IRInstr{Op: IRArrayAppend, Dst: VReg(dst), Src1: elemOp})
	}
	return VReg(dst)
}

// ---------------------------------------------------------------------------
// Built-in intrinsic handlers
// ---------------------------------------------------------------------------

type builtinHandler func(e *ast.CallExpr) Operand

func (l *Lowerer) builtinHandlers() map[string]builtinHandler {
	return map[string]builtinHandler{
		"mov":          l.builtinMov,
		"lea":          l.builtinLea,
		"push":         l.builtinPush,
		"pop":          l.builtinPop,
		"syscall":      l.builtinSyscall,
		"int":          l.builtinInt,
		"call":         l.builtinCall,
		"ret":          l.builtinRet,
		"nop":          l.builtinNop,
		"setreg":       l.builtinSetReg,
		"getreg":       l.builtinGetReg,
		"setflag":      l.builtinSetFlag,
		"getflag":      l.builtinGetFlag,
		"array_append": l.builtinAppend,
		"array_pop":    l.builtinArrayPop,
		"len":          l.builtinLen,
		"load8":        l.builtinLoad8,
		"load32":       l.builtinLoad32,
		"load64":       l.builtinLoad64,
		"win_call":     l.builtinWinCall,
	}
}

// mov(dst, src) — move data into a register or memory.
func (l *Lowerer) builtinMov(e *ast.CallExpr) Operand {
	if len(e.Args) != 2 {
		return None()
	}
	dstOp := l.lowerExpr(e.Args[0])
	srcOp := l.lowerExpr(e.Args[1])
	l.emit(IRInstr{Op: IRMov, Dst: dstOp, Src1: srcOp})
	return None()
}

// lea(dst, src) — load effective address.
func (l *Lowerer) builtinLea(e *ast.CallExpr) Operand {
	if len(e.Args) != 2 {
		return None()
	}
	dstOp := l.lowerExpr(e.Args[0])

	// For lea, we want the address of the data the source refers to.
	// If the source is a local variable (already lowered via IRLoad into a vreg),
	// the vreg already holds the pointer value we want.  Using IRMov is correct
	// because the value IS the address.  IRLea is only needed for raw memory
	// or string-literal references.
	srcArg := e.Args[1]
	if ident, ok := srcArg.(*ast.IdentExpr); ok {
		if !l.physRegs[ident.Name] {
			// Local variable: load its value (which is already a pointer for strings).
			srcOp := l.lowerExpr(srcArg)
			l.emit(IRInstr{Op: IRMov, Dst: dstOp, Src1: srcOp})
			return None()
		}
	}
	srcOp := l.lowerExpr(e.Args[1])
	l.emit(IRInstr{Op: IRLea, Dst: dstOp, Src1: srcOp})
	return None()
}

// push(value)
func (l *Lowerer) builtinPush(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	l.emit(IRInstr{Op: IRPush, Src1: srcOp})
	return None()
}

// pop() -> value
func (l *Lowerer) builtinPop(e *ast.CallExpr) Operand {
	// pop() — CPU stack pop.
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRPop, Dst: VReg(dst)})
	return VReg(dst)
}

// array_pop(arr) — pop the last element from an array.
func (l *Lowerer) builtinArrayPop(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	arrOp := l.lowerExpr(e.Args[0])
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRArrayPop, Dst: VReg(dst), Src1: arrOp})
	return VReg(dst)
}

// syscall()
func (l *Lowerer) builtinSyscall(e *ast.CallExpr) Operand {
	l.emit(IRInstr{Op: IRSyscall})
	return None()
}

// int(number)
func (l *Lowerer) builtinInt(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	l.emit(IRInstr{Op: IRInt, Src1: srcOp})
	return None()
}

// call(label)
func (l *Lowerer) builtinCall(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	targetOp := l.lowerExpr(e.Args[0])
	l.emit(IRInstr{Op: IRCall, Src1: targetOp})
	return None()
}

// ret()
func (l *Lowerer) builtinRet(e *ast.CallExpr) Operand {
	l.emit(IRInstr{Op: IRRet})
	return None()
}

// nop()
func (l *Lowerer) builtinNop(e *ast.CallExpr) Operand {
	l.emit(IRInstr{Op: IRNop})
	return None()
}

// setreg(regname, value)
func (l *Lowerer) builtinSetReg(e *ast.CallExpr) Operand {
	if len(e.Args) != 2 {
		return None()
	}
	dstOp := l.lowerExpr(e.Args[0])
	srcOp := l.lowerExpr(e.Args[1])
	l.emit(IRInstr{Op: IRSetReg, Dst: dstOp, Src1: srcOp})
	return None()
}

// getreg(regname) -> value
func (l *Lowerer) builtinGetReg(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRGetReg, Dst: VReg(dst), Src1: srcOp})
	return VReg(dst)
}

// setflag(flag, value)
func (l *Lowerer) builtinSetFlag(e *ast.CallExpr) Operand {
	if len(e.Args) != 2 {
		return None()
	}
	dstOp := l.lowerExpr(e.Args[0])
	srcOp := l.lowerExpr(e.Args[1])
	l.emit(IRInstr{Op: IRSetFlag, Dst: dstOp, Src1: srcOp})
	return None()
}

// getflag(flag) -> bool
func (l *Lowerer) builtinGetFlag(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRGetFlag, Dst: VReg(dst), Src1: srcOp})
	return VReg(dst)
}

// array_append(arr, val) — append a value to an array.
func (l *Lowerer) builtinAppend(e *ast.CallExpr) Operand {
	if len(e.Args) != 2 {
		return None()
	}
	arrOp := l.lowerExpr(e.Args[0])
	valOp := l.lowerExpr(e.Args[1])
	l.emit(IRInstr{Op: IRArrayAppend, Dst: arrOp, Src1: valOp})
	return None()
}

// len(s) — string length (inline, no function call, does NOT clobber regs).
func (l *Lowerer) builtinLen(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRStrLen, Dst: VReg(dst), Src1: srcOp})
	return VReg(dst)
}

// load8(addr) — load unsigned byte from memory address → i32.
func (l *Lowerer) builtinLoad8(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRLoad8, Dst: VReg(dst), Src1: srcOp})
	return VReg(dst)
}

// load32(addr) — load 32-bit integer from memory address → i32.
func (l *Lowerer) builtinLoad32(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRLoad32, Dst: VReg(dst), Src1: srcOp})
	return VReg(dst)
}

// load64(addr) — load 64-bit integer from memory address → i64.
func (l *Lowerer) builtinLoad64(e *ast.CallExpr) Operand {
	if len(e.Args) != 1 {
		return None()
	}
	srcOp := l.lowerExpr(e.Args[0])
	dst := l.freshVReg()
	l.emit(IRInstr{Op: IRLoad64, Dst: VReg(dst), Src1: srcOp})
	return VReg(dst)
}

// win_call("FuncName", arg1, arg2, ...) — call a Windows API function.
// The first argument must be a string literal naming the API function.
// Remaining arguments are passed via Windows x64 calling convention.
func (l *Lowerer) builtinWinCall(e *ast.CallExpr) Operand {
	if len(e.Args) < 1 {
		return None()
	}

	// The first argument is the API function name (string literal).
	// We store it as a label operand (the emitter will use it as an extern).
	nameArg := e.Args[0]
	apiName := ""
	if strLit, ok := nameArg.(*ast.StringLitExpr); ok {
		raw := strLit.Value
		if len(raw) >= 2 && (raw[0] == '"' || raw[0] == '\'') {
			raw = raw[1 : len(raw)-1]
		}
		apiName = raw
	}

	if apiName == "" {
		// Fallback: lower the expression and use it as a label.
		apiName = "unknown_win_api"
	}

	// Lower remaining arguments.
	var argOps []Operand
	for _, arg := range e.Args[1:] {
		argOps = append(argOps, l.lowerExpr(arg))
	}

	dst := l.freshVReg()
	l.emit(IRInstr{
		Op:   IRWinCall,
		Dst:  VReg(dst),
		Src1: LabelOp(apiName),
		Args: argOps,
	})
	return VReg(dst)
}

// ---------------------------------------------------------------------------
// String unescape utility
// ---------------------------------------------------------------------------

func unescapeString(s string) string {
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		if s[i] == '\\' && i+1 < len(s) {
			switch s[i+1] {
			case 'n':
				b.WriteByte('\n')
			case 'r':
				b.WriteByte('\r')
			case 't':
				b.WriteByte('\t')
			case '\\':
				b.WriteByte('\\')
			case '"':
				b.WriteByte('"')
			case '\'':
				b.WriteByte('\'')
			case '0':
				b.WriteByte(0)
			default:
				b.WriteByte(s[i+1])
			}
			i++
		} else {
			b.WriteByte(s[i])
		}
	}
	return b.String()
}
