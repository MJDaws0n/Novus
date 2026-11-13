package codegen

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
	e.b.WriteString("    bl _novus_windows_args\n    ldr x1, [x0]\n    ldr x0, [x0, #8]\n    sub x0, x0, #1\n")
}
