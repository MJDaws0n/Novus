package codegen

import (
	"novus/internal/ast"
	"strings"
)

func (l *Lowerer) lowerArgs(operands []Operand, args []ast.Expr, types []string) []Operand {
	for i, arg := range args {
		typ := ""
		if i < len(types) {
			typ = types[i]
		}
		operands = append(operands, l.lowerExprForType(arg, typ))
	}
	return operands
}

func (l *Lowerer) lowerNewExpr(e *ast.NewExpr) Operand {
	args := l.lowerArgs(nil, e.Args, l.funcParamTypes[e.Constructor][1:])
	instance := VReg(l.freshVReg())
	size := int64(max(16/l.target.PtrSize, e.Slots) * l.target.PtrSize)
	l.emit(IRInstr{Op: IRHeapAlloc, Dst: instance, Src1: Imm(size)})
	// Recycled blocks may contain old values and GC pointers.
	for slot := 0; slot < int(size)/l.target.PtrSize; slot++ {
		address := l.slotAddress(instance, slot)
		l.emit(IRInstr{Op: IRStore, Src1: Imm(0), Src2: address})
	}
	l.emit(IRInstr{Op: IRCall, Src1: LabelOp(e.Constructor), Args: append([]Operand{instance}, args...)})
	return instance
}

func (l *Lowerer) slotAddress(base Operand, slot int) Operand {
	if slot == 0 {
		return base
	}
	addr := VReg(l.freshVReg())
	l.emit(IRInstr{Op: IRAdd, Dst: addr, Src1: base, Src2: Imm(int64(slot * l.target.PtrSize))})
	return addr
}

func (l *Lowerer) fieldAddress(member *ast.MemberExpr) Operand {
	if member.ResolvedField.Global != "" {
		return None()
	}
	return l.slotAddress(l.lowerExpr(member.Object), member.ResolvedField.Slot)
}

func (l *Lowerer) storeField(field *ast.ResolvedField, address, value Operand) {
	value = l.coerceValueToType(field.Type, value)
	if field.Global != "" {
		l.emit(IRInstr{Op: IRStoreGlobal, Dst: LabelOp(l.globals[field.Global]), Src1: value})
	} else {
		l.emit(IRInstr{Op: IRStore, Src1: value, Src2: address})
	}
}

func (l *Lowerer) loadField(field *ast.ResolvedField, address Operand) Operand {
	dst := VReg(l.freshVReg())
	if field.Global != "" {
		l.emit(IRInstr{Op: IRLoadGlobal, Dst: dst, Src1: LabelOp(l.globals[field.Global])})
	} else {
		l.emit(IRInstr{Op: IRLoad64, Dst: dst, Src1: address})
	}
	l.markOperandType(dst, field.Type)
	return dst
}

func (l *Lowerer) markOperandType(operand Operand, typ string) {
	switch typ {
	case "str":
		l.vregIsStr[operand.Reg] = true
	case "f32", "f64":
		l.vregIsFloat[operand.Reg] = true
	case "u8", "u16", "u32", "u64":
		l.vregIsUnsigned[operand.Reg] = true
	}
}

func (l *Lowerer) indexElementType(index *ast.IndexExpr) string {
	if index.ArrayType != "" {
		return index.ArrayType
	}
	objectType := l.exprType(index.Object)
	if strings.HasPrefix(objectType, "[]") {
		return strings.TrimPrefix(objectType, "[]")
	}
	return ""
}

func (l *Lowerer) arrayElement(array, index Operand, elemType string) Operand {
	dst := VReg(l.freshVReg())
	l.emit(IRInstr{Op: IRArrayGet, Dst: dst, Src1: array, Src2: index})
	l.markOperandType(dst, elemType)
	return dst
}
