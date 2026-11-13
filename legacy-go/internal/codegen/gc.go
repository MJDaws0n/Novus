package codegen

import (
	"fmt"
	"novus/internal/lexer"
	"novus/internal/parser"
	"strings"
)

// The collector is ordinary Novus code. It allocates nothing and uses the same
// backend as the program. Binary search and an explicit mark queue avoid the
// quadratic pointer searches of the original assembly collectors.
const collectorSource = `fn _novus_gc_round(bytes: WORD) -> WORD {
    let size: WORD = MARKOFFSET;
    while (size < bytes) { size *= 2; }
    return size;
}
fn _novus_gc_bin(bytes: WORD) -> WORD {
    let index: WORD = 0;
    while (bytes > 1) { bytes = bytes >> 1; index++; }
    return index;
}
fn _novus_gc_take(free_list: WORD, bytes: WORD) -> WORD {
    let slot: WORD = free_list + _novus_gc_bin(bytes) * WORDSIZE;
    let address: WORD = LOAD(slot);
    if (address != 0) { store_word(slot, LOAD(address)); }
    return address;
}
fn _novus_gc_swap(table: WORD, first: WORD, second: WORD) -> void {
    let left: WORD = table + first * ENTRYSIZE;
    let right: WORD = table + second * ENTRYSIZE;
    for (let offset: WORD = 0; offset < ENTRYSIZE; offset += WORDSIZE) {
        let value: WORD = LOAD(left + offset);
        store_word(left + offset, LOAD(right + offset));
        store_word(right + offset, value);
    }
}
fn _novus_gc_sift(table: WORD, root_index: WORD, size: WORD) -> void {
    while (root_index * 2 + 1 < size) {
        let child: WORD = root_index * 2 + 1;
        if (child + 1 < size && LOAD(table + child * ENTRYSIZE) < LOAD(table + (child + 1) * ENTRYSIZE)) { child++; }
        if (LOAD(table + root_index * ENTRYSIZE) >= LOAD(table + child * ENTRYSIZE)) { return; }
        _novus_gc_swap(table, root_index, child);
        root_index = child;
    }
}
fn _novus_gc_index(table: WORD, size: WORD, pages: WORD) -> void {
    store_word(pages, 0);
    for (let i: WORD = 0; i < 262144; i++) { store_word(pages + WORDSIZE + i * ENTRYSIZE, 0); }
    let used: WORD = 0;
    for (let i: WORD = 0; i < size; i++) {
        let entry: WORD = table + i * ENTRYSIZE;
        let first: WORD = LOAD(entry) / 4096;
        let last: WORD = (LOAD(entry) + LOAD(entry + WORDSIZE) - 1) / 4096;
        for (let page: WORD = first; page <= last; page++) {
            let key: WORD = page + 1;
            let bucket: WORD = ((page ^ (page >> 18)) * 2654435761) & 262143;
            let probes: WORD = 0;
            let slot: WORD = pages + WORDSIZE + bucket * ENTRYSIZE;
            while (LOAD(slot) != 0 && LOAD(slot) != key) {
                bucket = (bucket + 1) & 262143;
                slot = pages + WORDSIZE + bucket * ENTRYSIZE;
                probes++;
                if (probes == 262144) { store_word(pages, 1); return; }
            }
            if (LOAD(slot) == 0) {
                used++;
                if (used >= 196608) { store_word(pages, 1); return; }
                store_word(slot, key);
                store_word(slot + WORDSIZE, i);
            }
            store_word(slot + MARKOFFSET, i + 1);
        }
    }
}
fn _novus_gc_mark(table: WORD, size: WORD, queue: WORD, candidate: WORD) -> void {
    if (size == 0 || candidate < LOAD(table)) { return; }
    let last: WORD = table + (size - 1) * ENTRYSIZE;
    if (candidate >= LOAD(last) + LOAD(last + WORDSIZE)) { return; }
    let low: WORD = 0;
    let high: WORD = size;
    let pages: WORD = queue + PAGEOFFSET;
    if (LOAD(pages) == 0) {
        let page: WORD = candidate / 4096;
        let bucket: WORD = ((page ^ (page >> 18)) * 2654435761) & 262143;
        let slot: WORD = pages + WORDSIZE + bucket * ENTRYSIZE;
        while (LOAD(slot) != page + 1) {
            if (LOAD(slot) == 0) { return; }
            bucket = (bucket + 1) & 262143;
            slot = pages + WORDSIZE + bucket * ENTRYSIZE;
        }
        low = LOAD(slot + WORDSIZE);
        high = LOAD(slot + MARKOFFSET);
    }
    while (low < high) {
        let middle: WORD = low + (high - low) / 2;
        if (LOAD(table + middle * ENTRYSIZE) <= candidate) { low = middle + 1; }
        else { high = middle; }
    }
    if (low == 0) { return; }
    let entry: WORD = table + (low - 1) * ENTRYSIZE;
    if (candidate >= LOAD(entry) + LOAD(entry + WORDSIZE) || LOAD(entry + MARKOFFSET) != 0) { return; }
    store_word(entry + MARKOFFSET, 1);
    let queued: WORD = LOAD(queue);
    store_word(queue + (queued + 1) * WORDSIZE, low - 1);
    store_word(queue, queued + 1);
}
fn _novus_gc_trace(table: WORD, size: WORD, roots: WORD, root_count: WORD, stack_low: WORD, stack_high: WORD, free_list: WORD, queue: WORD) -> WORD {
    if (size == 0) { return 0; }
    for (let i: WORD = 0; i < size; i++) { store_word(table + i * ENTRYSIZE + MARKOFFSET, 0); }
    let heap_root: WORD = size / 2;
    while (heap_root > 0) { heap_root--; _novus_gc_sift(table, heap_root, size); }
    let end: WORD = size;
    while (end > 1) { end--; _novus_gc_swap(table, 0, end); _novus_gc_sift(table, 0, end); }
    _novus_gc_index(table, size, queue + PAGEOFFSET);
    store_word(queue, 0);
    if (stack_high != 0) {
        for (let cursor: WORD = stack_low; cursor < stack_high; cursor += WORDSIZE) { _novus_gc_mark(table, size, queue, LOAD(cursor)); }
    }
    for (let i: WORD = 0; i < root_count; i++) { _novus_gc_mark(table, size, queue, LOAD(LOAD(roots + i * WORDSIZE))); }
    let cursor: WORD = 0;
    while (cursor < LOAD(queue)) {
        let entry: WORD = table + LOAD(queue + (cursor + 1) * WORDSIZE) * ENTRYSIZE;
        let address: WORD = LOAD(entry);
        let bytes: WORD = LOAD(entry + WORDSIZE);
        for (let offset: WORD = 0; offset + WORDSIZE <= bytes; offset += WORDSIZE) { _novus_gc_mark(table, size, queue, LOAD(address + offset)); }
        cursor++;
    }
    let kept: WORD = 0;
    for (let i: WORD = 0; i < size; i++) {
        let entry: WORD = table + i * ENTRYSIZE;
        let address: WORD = LOAD(entry);
        let bytes: WORD = LOAD(entry + WORDSIZE);
        if (LOAD(entry + MARKOFFSET) != 0) {
            let destination: WORD = table + kept * ENTRYSIZE;
            store_word(destination, address);
            store_word(destination + WORDSIZE, bytes);
            store_word(destination + MARKOFFSET, 0);
            kept++;
        } else if (bytes >= MARKOFFSET) {
            let slot: WORD = free_list + _novus_gc_bin(bytes) * WORDSIZE;
            store_word(address, LOAD(slot));
            store_word(address + WORDSIZE, bytes);
            store_word(slot, address);
        }
    }
    return kept;
}
`

func withCollector(mod *IRModule, target *Target) *IRModule {
	needed := false
	for _, fn := range mod.Functions {
		if fn.Name == "_novus_gc_trace" {
			return mod
		}
		for _, instruction := range fn.Instrs {
			switch instruction.Op {
			case IRHeapAlloc, IRStrConcat, IRArrayNew, IRArrayAppend, IRGCCollect:
				needed = true
			}
		}
	}
	if !needed {
		return mod
	}
	word, load := "i64", "load64"
	if target.PtrSize == 4 {
		word, load = "i32", "load32"
	}
	source := strings.ReplaceAll(collectorSource, "WORDSIZE", fmt.Sprintf("%d", target.PtrSize))
	source = strings.ReplaceAll(source, "ENTRYSIZE", fmt.Sprintf("%d", target.PtrSize*3))
	source = strings.ReplaceAll(source, "MARKOFFSET", fmt.Sprintf("%d", target.PtrSize*2))
	source = strings.ReplaceAll(source, "PAGEOFFSET", fmt.Sprintf("%d", (target.GCEntries+1)*target.PtrSize))
	source = strings.ReplaceAll(source, "WORD", word)
	source = strings.ReplaceAll(source, "LOAD", load)
	tokens, _ := lexer.Lex(source)
	program, _ := parser.Parse(tokens)
	runtime := Lower(program, target)
	result := *mod
	result.Functions = append([]*IRFunc{}, mod.Functions...)
	for _, fn := range runtime.Functions {
		for i := range fn.Instrs {
			instruction := &fn.Instrs[i]
			instruction.Dst = collectorOperand(instruction.Dst)
			instruction.Src1 = collectorOperand(instruction.Src1)
			instruction.Src2 = collectorOperand(instruction.Src2)
		}
		result.Functions = append(result.Functions, fn)
	}
	return &result
}

func collectorOperand(operand Operand) Operand {
	if operand.Kind == OpLabel && strings.HasPrefix(operand.Label, ".L") {
		return LabelOp(".Lgc_runtime" + operand.Label[2:])
	}
	return operand
}

func collectorData(mod *IRModule, target *Target) string {
	var out strings.Builder
	roots := target.Sym("_novus_gc_roots")
	queue := target.Sym("_novus_gc_queue")
	if target.Flavor == NASM {
		out.WriteString("section .data\n" + roots + ":\n")
		for _, global := range mod.Globals {
			out.WriteString("    dq " + target.Sym(global.Name) + "\n")
		}
		out.WriteString(fmt.Sprintf("section .bss\n%s: resb %d\nsection .text\n", queue, (target.GCEntries+1+262144*3+1)*target.PtrSize))
	} else {
		section := ".data\n"
		text := ".text\n"
		if target.OS == OS_Darwin {
			section = ".section __DATA,__data\n"
			text = ".section __TEXT,__text\n"
		}
		out.WriteString(section + roots + ":\n")
		directive := ".quad"
		if target.PtrSize == 4 {
			directive = ".long"
		}
		for _, global := range mod.Globals {
			out.WriteString("    " + directive + " " + target.Sym(global.Name) + "\n")
		}
		out.WriteString(fmt.Sprintf(".lcomm %s, %d\n%s", queue, (target.GCEntries+1+262144*3+1)*target.PtrSize, text))
	}
	return out.String()
}

func (e *arm64Emitter) emitFastCollector() {
	w := e.b
	w.WriteString(collectorData(e.mod, e.target))
	w.WriteString(e.target.Sym("_novus_gc_collect") + ":\n")
	for r := 0; r < 30; r += 2 {
		w.WriteString(fmt.Sprintf("    stp x%d, x%d, [sp, #-16]!\n", r, r+1))
	}
	w.WriteString("    stp x30, xzr, [sp, #-16]!\n")
	e.emitLoadGlobal("x0", e.target.Sym("_novus_gc_table"))
	e.emitLoadGlobal("x9", e.target.Sym("_novus_gc_count"))
	w.WriteString("    ldr x1, [x9]\n")
	e.emitLoadGlobal("x2", e.target.Sym("_novus_gc_roots"))
	e.loadImm("x3", int64(len(e.mod.Globals)))
	w.WriteString("    mov x4, sp\n")
	e.emitLoadGlobal("x9", e.target.Sym("_novus_gc_stack_bottom"))
	w.WriteString("    ldr x5, [x9]\n")
	e.emitLoadGlobal("x6", e.target.Sym("_novus_gc_freelist"))
	e.emitLoadGlobal("x7", e.target.Sym("_novus_gc_queue"))
	w.WriteString("    bl " + e.target.Sym("_novus_gc_trace") + "\n")
	e.emitLoadGlobal("x9", e.target.Sym("_novus_gc_count"))
	w.WriteString("    str x0, [x9]\n")
	e.emitLoadGlobal("x9", e.target.Sym("_novus_gc_threshold"))
	w.WriteString("    lsl x0, x0, #1\n    add x0, x0, #256\n    str x0, [x9]\n")
	w.WriteString("    ldp x30, xzr, [sp], #16\n")
	for r := 28; r >= 0; r -= 2 {
		w.WriteString(fmt.Sprintf("    ldp x%d, x%d, [sp], #16\n", r, r+1))
	}
	w.WriteString("    ret\n")
}

func (e *x86_64Emitter) emitFastCollector(nasm bool) {
	w := e.b
	w.WriteString(collectorData(e.mod, e.target))
	w.WriteString(e.target.Sym("_novus_gc_collect") + ":\n")
	registers := []string{"rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"}
	for _, reg := range registers {
		if nasm {
			w.WriteString("    push " + reg + "\n")
		} else {
			w.WriteString("    pushq %" + reg + "\n")
		}
	}
	if nasm {
		w.WriteString("    mov r10, rsp\n    sub rsp, 64\n")
		w.WriteString("    lea rcx, [rel _novus_gc_table]\n    mov rdx, [rel _novus_gc_count]\n    lea r8, [rel _novus_gc_roots]\n")
		w.WriteString(fmt.Sprintf("    mov r9, %d\n", len(e.mod.Globals)))
		w.WriteString("    mov [rsp+32], r10\n    mov rax, [rel _novus_gc_stack_bottom]\n    mov [rsp+40], rax\n")
		w.WriteString("    lea rax, [rel _novus_gc_freelist]\n    mov [rsp+48], rax\n    lea rax, [rel _novus_gc_queue]\n    mov [rsp+56], rax\n")
		w.WriteString("    call _novus_gc_trace\n    add rsp, 64\n    mov [rel _novus_gc_count], rax\n")
		w.WriteString("    lea rax, [rax*2+256]\n    mov [rel _novus_gc_threshold], rax\n")
	} else {
		w.WriteString("    movq %rsp, %r8\n")
		w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rdi\n", e.target.Sym("_novus_gc_table")))
		w.WriteString(fmt.Sprintf("    movq %s(%%rip), %%rsi\n", e.target.Sym("_novus_gc_count")))
		w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rdx\n", e.target.Sym("_novus_gc_roots")))
		w.WriteString(fmt.Sprintf("    movq $%d, %%rcx\n", len(e.mod.Globals)))
		w.WriteString(fmt.Sprintf("    movq %s(%%rip), %%r9\n", e.target.Sym("_novus_gc_stack_bottom")))
		w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n    pushq %%rax\n", e.target.Sym("_novus_gc_queue")))
		w.WriteString(fmt.Sprintf("    leaq %s(%%rip), %%rax\n    pushq %%rax\n", e.target.Sym("_novus_gc_freelist")))
		w.WriteString("    call " + e.target.Sym("_novus_gc_trace") + "\n    addq $16, %rsp\n")
		w.WriteString(fmt.Sprintf("    movq %%rax, %s(%%rip)\n", e.target.Sym("_novus_gc_count")))
		w.WriteString("    leaq 256(,%rax,2), %rax\n")
		w.WriteString(fmt.Sprintf("    movq %%rax, %s(%%rip)\n", e.target.Sym("_novus_gc_threshold")))
	}
	for i := len(registers) - 1; i >= 0; i-- {
		if nasm {
			w.WriteString("    pop " + registers[i] + "\n")
		} else {
			w.WriteString("    popq %" + registers[i] + "\n")
		}
	}
	w.WriteString("    ret\n")
}

func (e *x86Emitter) emitFastCollector() {
	w := e.b
	w.WriteString(collectorData(e.mod, e.target))
	w.WriteString("_novus_gc_collect:\n    pushal\n    movl %esp, %eax\n")
	w.WriteString("    pushl $_novus_gc_queue\n    pushl $_novus_gc_freelist\n    pushl _novus_gc_stack_bottom\n    pushl %eax\n")
	w.WriteString(fmt.Sprintf("    pushl $%d\n", len(e.mod.Globals)))
	w.WriteString("    pushl $_novus_gc_roots\n    pushl _novus_gc_count\n    pushl $_novus_gc_table\n    call _novus_gc_trace\n    addl $32, %esp\n")
	w.WriteString("    movl %eax, _novus_gc_count\n    leal 256(,%eax,2), %eax\n    movl %eax, _novus_gc_threshold\n    popal\n    ret\n")
}

// Windows startup uses the documented Unicode command-line APIs, independent
// of the particular C runtime import library installed with the Windows SDK.
const windowsArgsSource = `fn _novus_windows_args() -> []u64 {
    let count: i32 = 0;
    win_call("GetCommandLineW");
    let command: u64 = getreg(rax);
    win_call("CommandLineToArgvW", command, &count);
    let wide: u64 = getreg(rax);
    if (wide == 0) { win_call("ExitProcess", 71); }
    let arguments: []u64 = [];
    for (let i: i32 = 0; i < count; i++) {
        let value: u64 = load64(wide + i * 8);
        win_call("WideCharToMultiByte", 65001, 0, value, -1, 0, 0, 0, 0);
        let size: i32 = getreg(rax);
        if (size <= 0) { win_call("ExitProcess", 71); }
        let buffer: u64 = _novus_gc_alloc(size);
        win_call("WideCharToMultiByte", 65001, 0, value, -1, buffer, size, 0, 0);
        let copied: i32 = getreg(rax);
        if (copied <= 0) { win_call("ExitProcess", 71); }
        array_append(arguments, buffer);
    }
    array_append(arguments, 0);
    win_call("LocalFree", wide);
    return arguments;
}
`

func withWindowsArgs(mod *IRModule, target *Target) *IRModule {
	if target.OS != OS_Windows {
		return mod
	}
	needed := false
	for _, fn := range mod.Functions {
		if fn.Name == "_novus_windows_args" {
			return mod
		}
		if fn.Name == "main" && fn.ParamCount >= 2 {
			needed = true
		}
	}
	if !needed {
		return mod
	}
	tokens, _ := lexer.Lex(windowsArgsSource)
	program, _ := parser.Parse(tokens)
	runtime := Lower(program, target)
	result := *mod
	result.Functions = append([]*IRFunc{}, mod.Functions...)
	for _, fn := range runtime.Functions {
		for i := range fn.Instrs {
			instruction := &fn.Instrs[i]
			operands := []*Operand{&instruction.Dst, &instruction.Src1, &instruction.Src2}
			for _, operand := range operands {
				if operand.Kind == OpLabel && strings.HasPrefix(operand.Label, ".L") {
					operand.Label = ".Lwin_args" + operand.Label[2:]
				}
			}
		}
		result.Functions = append(result.Functions, fn)
	}
	return &result
}
