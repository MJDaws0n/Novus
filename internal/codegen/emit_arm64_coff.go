package codegen

import "fmt"

func (e *arm64Emitter) emitWindowsHeapAllocator() {
	w := e.b
	w.WriteString("\n.p2align 2\n_novus_heap_alloc:\n")
	e.emitLoadGlobal("x9", "_novus_heap")
	e.emitLoadGlobal("x10", "_novus_heap_ptr")
	w.WriteString("    ldr x11, [x10]\n")
	w.WriteString("    add x12, x11, x0\n")
	e.loadImm("x13", e.target.HeapSize)
	w.WriteString("    cmp x12, x13\n")
	w.WriteString("    b.hi .Lwin_heap_exhausted\n")
	w.WriteString("    str x12, [x10]\n")
	w.WriteString("    add x0, x9, x11\n")
	w.WriteString("    ret\n")
	w.WriteString(".Lwin_heap_exhausted:\n")
	w.WriteString("    brk #0\n")
}

func (e *arm64Emitter) emitWinMainArgs() {
	// Reserve five zeroed slots for the UCRT outputs and startup information.
	w := e.b
	w.WriteString("    sub sp, sp, #48\n")
	w.WriteString("    stp xzr, xzr, [sp]\n")
	w.WriteString("    stp xzr, xzr, [sp, #16]\n")
	w.WriteString("    str xzr, [sp, #32]\n")
	w.WriteString("    mov x0, sp\n")
	w.WriteString("    add x1, sp, #8\n")
	w.WriteString("    add x2, sp, #16\n")
	w.WriteString("    mov x3, xzr\n")
	w.WriteString("    add x4, sp, #32\n")
	w.WriteString(fmt.Sprintf("    bl %s\n", e.target.Sym("__getmainargs")))
	w.WriteString("    ldr w0, [sp]\n")
	w.WriteString("    ldr x1, [sp, #8]\n")
	w.WriteString("    add sp, sp, #48\n")
}
