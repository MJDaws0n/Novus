package codegen

import "fmt"

// ---------------------------------------------------------------------------
// IR — a low-level, architecture-neutral intermediate representation
//
// The IR is a simple, flat, three-address-code style instruction list
// organized into functions, each containing basic blocks.  Operands are
// virtual registers, physical registers, immediates, memory references,
// labels, and string references.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Operand kinds
// ---------------------------------------------------------------------------

// OpKind describes what an IR operand represents.
type OpKind int

const (
	OpNone      OpKind = iota // unused operand slot
	OpVirtReg                 // virtual register (SSA-ish, numbered)
	OpPhysReg                 // a named physical CPU register (e.g. "rax")
	OpImmediate               // integer literal
	OpStringRef               // reference to a string constant in .data (index into IRModule.Strings)
	OpLabel                   // label reference (branch target / function name)
	OpMemory                  // memory operand [base + offset] or [base + index*scale + disp]
)

// Operand is a single value in an IR instruction.
type Operand struct {
	Kind    OpKind
	Reg     int    // virtual register number (OpVirtReg)
	PhysReg string // physical register name (OpPhysReg)
	Imm     int64  // integer value (OpImmediate) or string index (OpStringRef)
	Label   string // label name (OpLabel)

	// Memory operand fields
	MemBase   string // base register name
	MemOffset int64  // constant displacement
}

func (o Operand) String() string {
	switch o.Kind {
	case OpNone:
		return "<none>"
	case OpVirtReg:
		return fmt.Sprintf("v%d", o.Reg)
	case OpPhysReg:
		return fmt.Sprintf("%%%s", o.PhysReg)
	case OpImmediate:
		return fmt.Sprintf("$%d", o.Imm)
	case OpStringRef:
		return fmt.Sprintf("str_%d", o.Imm)
	case OpLabel:
		return o.Label
	case OpMemory:
		if o.MemOffset != 0 {
			return fmt.Sprintf("[%s + %d]", o.MemBase, o.MemOffset)
		}
		return fmt.Sprintf("[%s]", o.MemBase)
	default:
		return "?"
	}
}

// Convenience constructors for operands.
func VReg(n int) Operand          { return Operand{Kind: OpVirtReg, Reg: n} }
func PReg(name string) Operand    { return Operand{Kind: OpPhysReg, PhysReg: name} }
func Imm(val int64) Operand       { return Operand{Kind: OpImmediate, Imm: val} }
func StrRef(idx int) Operand      { return Operand{Kind: OpStringRef, Imm: int64(idx)} }
func LabelOp(name string) Operand { return Operand{Kind: OpLabel, Label: name} }
func Mem(base string, off int64) Operand {
	return Operand{Kind: OpMemory, MemBase: base, MemOffset: off}
}
func None() Operand { return Operand{Kind: OpNone} }

// ---------------------------------------------------------------------------
// IR opcodes
// ---------------------------------------------------------------------------

// IROp is an IR instruction opcode.
type IROp int

const (
	// Data movement
	IRMov   IROp = iota // dst = src
	IRLea               // dst = address-of src
	IRLoad              // dst = *src (load from memory)
	IRStore             // *dst = src (store to memory)
	IRPush              // push src onto stack
	IRPop               // dst = pop from stack

	// Arithmetic
	IRAdd // dst = src1 + src2
	IRSub // dst = src1 - src2
	IRMul // dst = src1 * src2
	IRDiv // dst = src1 / src2 (signed)
	IRMod // dst = src1 % src2 (signed)
	IRNeg // dst = -src1

	// Logic / bitwise
	IRAnd // dst = src1 & src2
	IROr  // dst = src1 | src2
	IRXor // dst = src1 ^ src2
	IRNot // dst = !src1 (logical not)

	// Comparison — sets dst to 0 or 1
	IRCmpEq // dst = (src1 == src2)
	IRCmpNe // dst = (src1 != src2)
	IRCmpLt // dst = (src1 < src2)
	IRCmpLe // dst = (src1 <= src2)
	IRCmpGt // dst = (src1 > src2)
	IRCmpGe // dst = (src1 >= src2)

	// Control flow
	IRJmp    // unconditional jump to label
	IRJmpIf  // conditional jump: if src1 != 0 goto label
	IRJmpNot // conditional jump: if src1 == 0 goto label
	IRLabel  // label definition (not a real instruction; marks a position)
	IRCall   // call function: dst = call label(args...)
	IRRet    // return from function (optional src1 = return value)

	// System / low-level
	IRSyscall // invoke system call (regs already set up)
	IRInt     // software interrupt: int src1
	IRNop     // no operation

	// Register transfer (for Novus builtins setreg/getreg)
	IRSetReg // move src1 into physical register named by dst
	IRGetReg // move physical register named by src1 into dst

	// Flag manipulation (for Novus builtins setflag/getflag)
	IRSetFlag // set flag named by dst to src1
	IRGetFlag // dst = read flag named by src1

	// String operations
	IRStrConcat // dst = src1 + src2 (string concatenation, helper call)
	IRStrLen    // dst = len(src1)
	IRStrIndex  // dst = src1[src2]
	IRStoreByte // store byte src1 at address [dst] (for string index assignment)

	// Memory load (raw address read)
	IRLoad8  // dst = *(uint8*)src1  (load byte from address, zero-extend)
	IRLoad32 // dst = *(int32*)src1  (load 32-bit word from address)
	IRLoad64 // dst = *(int64*)src1  (load 64-bit qword from address)

	// Array operations
	IRArrayNew    // dst = new array (Src1=elem size imm, Src2=initial cap imm)
	IRArrayGet    // dst = arr[index] (Src1=arrPtr, Src2=index)
	IRArraySet    // arr[index] = val (Dst=arrPtr, Src1=index, Src2=val)
	IRArrayAppend // append val to arr (Dst=arrPtr, Src1=val)
	IRArrayPop    // dst = pop(arr) (Src1=arrPtr)
	IRArrayLen    // dst = len(arr) (Src1=arrPtr)

	// Windows API call
	IRWinCall // call Windows API function (Src1=string ref with function name, Args=arguments)

	// Misc
	IRComment // emit a comment in the output (src1 = label with comment text)
	IRData    // emit raw data (used for string constants in data section)
)

var irOpNames = map[IROp]string{
	IRMov: "mov", IRLea: "lea", IRLoad: "load", IRStore: "store",
	IRPush: "push", IRPop: "pop",
	IRAdd: "add", IRSub: "sub", IRMul: "mul", IRDiv: "div", IRMod: "mod", IRNeg: "neg",
	IRAnd: "and", IROr: "or", IRXor: "xor", IRNot: "not",
	IRCmpEq: "cmp_eq", IRCmpNe: "cmp_ne", IRCmpLt: "cmp_lt", IRCmpLe: "cmp_le",
	IRCmpGt: "cmp_gt", IRCmpGe: "cmp_ge",
	IRJmp: "jmp", IRJmpIf: "jmp_if", IRJmpNot: "jmp_not",
	IRLabel: "label", IRCall: "call", IRRet: "ret",
	IRSyscall: "syscall", IRInt: "int", IRNop: "nop",
	IRSetReg: "setreg", IRGetReg: "getreg", IRSetFlag: "setflag", IRGetFlag: "getflag",
	IRStrConcat: "str_concat", IRStrLen: "str_len", IRStrIndex: "str_index", IRStoreByte: "store_byte",
	IRLoad8: "load8", IRLoad32: "load32", IRLoad64: "load64",
	IRArrayNew: "array_new", IRArrayGet: "array_get", IRArraySet: "array_set",
	IRArrayAppend: "array_append", IRArrayPop: "array_pop", IRArrayLen: "array_len",
	IRWinCall: "win_call",
	IRComment: "comment", IRData: "data",
}

func (op IROp) String() string {
	if s, ok := irOpNames[op]; ok {
		return s
	}
	return fmt.Sprintf("irop_%d", int(op))
}

// ---------------------------------------------------------------------------
// IR Instruction
// ---------------------------------------------------------------------------

// IRInstr is a single IR instruction.
type IRInstr struct {
	Op   IROp
	Dst  Operand   // destination (or label for IRLabel/IRJmp)
	Src1 Operand   // first source
	Src2 Operand   // second source
	Args []Operand // extra args (used by IRCall)
}

func (i IRInstr) String() string {
	s := i.Op.String()
	if i.Dst.Kind != OpNone {
		s += " " + i.Dst.String()
	}
	if i.Src1.Kind != OpNone {
		s += ", " + i.Src1.String()
	}
	if i.Src2.Kind != OpNone {
		s += ", " + i.Src2.String()
	}
	for _, a := range i.Args {
		s += ", " + a.String()
	}
	return s
}

// ---------------------------------------------------------------------------
// IR Function / Module
// ---------------------------------------------------------------------------

// IRFunc represents a single function in IR form.
type IRFunc struct {
	Name       string
	ParamNames []string // parameter names (in order)
	ParamCount int
	Instrs     []IRInstr
	Locals     int // number of local variable stack slots used
	FrameSize  int // total frame size in bytes (aligned)
}

// Emit appends an instruction to the function.
func (f *IRFunc) Emit(instr IRInstr) {
	f.Instrs = append(f.Instrs, instr)
}

// IRStringConst holds a string literal that will be placed in the data section.
type IRStringConst struct {
	Label string // assembly label (e.g. "str_0")
	Value string // the raw string content (unescaped)
}

// IRModule is the top-level IR container for an entire compilation unit.
type IRModule struct {
	Functions []*IRFunc
	Strings   []IRStringConst
	EntryFunc string // name of the entry-point function (usually "main")
}

// AddString registers a string constant and returns its index.
func (m *IRModule) AddString(value string) int {
	// Deduplicate: if we already have this string, return existing index.
	for i, s := range m.Strings {
		if s.Value == value {
			return i
		}
	}
	idx := len(m.Strings)
	m.Strings = append(m.Strings, IRStringConst{
		Label: fmt.Sprintf("_str_%d", idx),
		Value: value,
	})
	return idx
}

// DebugDump returns a human-readable representation of the entire IR module.
func (m *IRModule) DebugDump() string {
	var s string
	s += fmt.Sprintf("=== IR Module (entry: %s, %d strings, %d functions) ===\n",
		m.EntryFunc, len(m.Strings), len(m.Functions))
	for i, str := range m.Strings {
		s += fmt.Sprintf("  .str[%d] %s = %q\n", i, str.Label, str.Value)
	}
	for _, fn := range m.Functions {
		s += fmt.Sprintf("\nfunc %s (params=%d, locals=%d, frame=%d):\n",
			fn.Name, fn.ParamCount, fn.Locals, fn.FrameSize)
		for _, instr := range fn.Instrs {
			s += fmt.Sprintf("  %s\n", instr.String())
		}
	}
	return s
}
