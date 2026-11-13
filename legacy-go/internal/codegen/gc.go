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
const collectorSource = `fn _novus_gc_word_store(address: WORD, value: WORD) -> void {
    for (let byte_index: WORD = 0; byte_index < WORDSIZE; byte_index++) {
        store_byte(address + byte_index, (value >> (byte_index * 8)) & 255);
    }
}
fn _novus_gc_round(bytes: WORD) -> WORD {
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
    if (address != 0) { _novus_gc_word_store(slot, LOAD(address)); }
    return address;
}
fn _novus_gc_swap(table: WORD, first: WORD, second: WORD) -> void {
    let left: WORD = table + first * ENTRYSIZE;
    let right: WORD = table + second * ENTRYSIZE;
    for (let offset: WORD = 0; offset < ENTRYSIZE; offset += WORDSIZE) {
        let value: WORD = LOAD(left + offset);
        _novus_gc_word_store(left + offset, LOAD(right + offset));
        _novus_gc_word_store(right + offset, value);
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
fn _novus_gc_mark(table: WORD, size: WORD, queue: WORD, candidate: WORD) -> void {
    if (size == 0 || candidate < LOAD(table)) { return; }
    let last: WORD = table + (size - 1) * ENTRYSIZE;
    if (candidate >= LOAD(last) + LOAD(last + WORDSIZE)) { return; }
    let low: WORD = 0;
    let high: WORD = size;
    while (low < high) {
        let middle: WORD = low + (high - low) / 2;
        if (LOAD(table + middle * ENTRYSIZE) <= candidate) { low = middle + 1; }
        else { high = middle; }
    }
    if (low == 0) { return; }
    let entry: WORD = table + (low - 1) * ENTRYSIZE;
    if (candidate >= LOAD(entry) + LOAD(entry + WORDSIZE) || LOAD(entry + MARKOFFSET) != 0) { return; }
    _novus_gc_word_store(entry + MARKOFFSET, 1);
    let queued: WORD = LOAD(queue);
    _novus_gc_word_store(queue + (queued + 1) * WORDSIZE, low - 1);
    _novus_gc_word_store(queue, queued + 1);
}
fn _novus_gc_trace(table: WORD, size: WORD, roots: WORD, root_count: WORD, stack_low: WORD, stack_high: WORD, free_list: WORD, queue: WORD) -> WORD {
    if (size == 0) { return 0; }
    for (let i: WORD = 0; i < size; i++) { _novus_gc_word_store(table + i * ENTRYSIZE + MARKOFFSET, 0); }
    let heap_root: WORD = size / 2;
    while (heap_root > 0) { heap_root--; _novus_gc_sift(table, heap_root, size); }
    let end: WORD = size;
    while (end > 1) { end--; _novus_gc_swap(table, 0, end); _novus_gc_sift(table, 0, end); }
    _novus_gc_word_store(queue, 0);
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
            _novus_gc_word_store(destination, address);
            _novus_gc_word_store(destination + WORDSIZE, bytes);
            _novus_gc_word_store(destination + MARKOFFSET, 0);
            kept++;
        } else if (bytes >= MARKOFFSET) {
            let slot: WORD = free_list + _novus_gc_bin(bytes) * WORDSIZE;
            _novus_gc_word_store(address, LOAD(slot));
            _novus_gc_word_store(address + WORDSIZE, bytes);
            _novus_gc_word_store(slot, address);
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
		out.WriteString(fmt.Sprintf("section .bss\n%s: resb %d\nsection .text\n", queue, (target.GCEntries+1)*target.PtrSize))
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
		out.WriteString(fmt.Sprintf(".lcomm %s, %d\n%s", queue, (target.GCEntries+1)*target.PtrSize, text))
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
