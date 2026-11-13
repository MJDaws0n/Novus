"""Mechanical representation adjustments after the typed source migration."""
from pathlib import Path
import re
root=Path('/Users/max/Documents/ChatGPT/Novus/compiler')
src=root/'src'

def replace_function(text,name,replacement):
    start=text.index('fn '+name+'(')
    end=text.find('\nfn ',start+4)
    if end<0:end=len(text)
    return text[:start]+replacement+'\n\n'+text[end:]

p=src/'codegen_lower.nov';s=p.read_text()
handlers=dict(re.findall(r'"([a-z0-9_]+)":\s+l\.(builtin\w+)',(root/'legacy-go/internal/codegen/lower.go').read_text()))
a=s.index('    {\n        let handler:');b=s.index('    let resolvedName:',a)
s=s[:a]+''.join('    if (calleeName == "'+key+'") { return codegen_Lowerer_'+value+'(l, e); }\n' for key,value in handlers.items())+s[b:]
# A structured equivalent of the Go floating-point fallback label.
s=s.replace('// MIGRATION ERROR Labelled branch intOps','op = -1;')
a=s.index('    if (isFloat) {',s.index('fn codegen_Lowerer_lowerBinaryOperands'))
b=s.index('    let op: i32 = 0;',a+30)
c=s.index('        if (!(isCmp)) {',b)
d=s.index('\n    }',c)
block=s[c:d]
s=s[:c]+'        if (op >= 0) {\n'+''.join('    '+line+'\n' for line in block.splitlines())+'        }'+s[d:]
s=s.replace('let raw: str = codegen_stringLiteralValue(', 'let raw: ByteBuffer = codegen_stringLiteralValue(')
s=s.replace('isSingleQuoted && (len(raw) == 1)', 'isSingleQuoted && (raw.length() == 1)').replace('runtime_to_i64(byte_at(raw, 0))','runtime_to_i64(raw.get(0))')
s=s.replace('fn codegen_stringLiteralValue(lexeme: str) -> str', 'fn codegen_stringLiteralValue(lexeme: str) -> ByteBuffer')
s=s.replace('let combined: str = (l.module_value.Strings[li].Value + l.module_value.Strings[ri].Value);','let combined: ByteBuffer = new ByteBuffer();\n                    combined.append_buffer(l.module_value.Strings[li].Value);\n                    combined.append_buffer(l.module_value.Strings[ri].Value);')
start=s.index('fn codegen_unescapeString(');end=s.index('\nfn ',start+4)
piece=s[start:end].replace('-> str {','-> ByteBuffer {',1).replace('let b: StringBuilder = new StringBuilder();','let b: ByteBuffer = new ByteBuffer();').replace('b.append_byte(', 'b.append(').replace('return b.text();','return b;')
s=s[:start]+piece+s[end:];p.write_text(s)

p=src/'model.nov';s=p.read_text();a=s.index('class CodegenIRStringConst {');b=s.index('\nclass ',a+6)
piece=s[a:b].replace('Value: str = ""','Value: ByteBuffer = new ByteBuffer()');s=s[:a]+piece+s[b:]
s=re.sub(r'(fn make_CodegenIRStringConst_\d+\(Label: str, Value:) str',r'\1 ByteBuffer',s)
# Go struct keys compare by fields, rather than object identity.
for name in re.findall(r'class (Map\w+)',s):
    if name.startswith('MapImportsfnkey'):
        a=s.index('fn '+name+'_index(');b=s.index('\nfn ',a+4)
        piece=s[a:b].replace('map.keys[i] == key','map.keys[i].alias == key.alias && map.keys[i].sig == key.sig')
        s=s[:a]+piece+s[b:]
p.write_text(s)

p=src/'codegen_ir.nov';s=p.read_text().replace('codegen_IRModule_AddString(m: CodegenIRModule, value: str)','codegen_IRModule_AddString(m: CodegenIRModule, value: ByteBuffer)').replace('(s.Value == value)','s.Value.equals(value)').replace('runtime_quote(str_value.Value)','codegen_gasQuoteString(str_value.Value)');p.write_text(s)

for p in src.glob('codegen_emit*.nov'):
    s=p.read_text().replace('len(s.Value)', 's.Value.length()')
    if 'fn codegen_gasQuoteString(' in s:
        s=replace_function(s,'codegen_gasQuoteString','''fn codegen_gasQuoteString(value: ByteBuffer) -> str {
    let result: StringBuilder = new StringBuilder();
    result.append("\\\"");
    for (let i: i32 = 0; i < value.length(); i++) {
        let ch: i32 = value.get(i);
        if (ch == 34) { result.append("\\\\\\\""); }
        else if (ch == 92) { result.append("\\\\\\\\"); }
        else if (ch < 32 || ch > 126) { result.append("\\\\" + format_unsigned(i32_to_u64(ch), 8, 3)); }
        else { result.append_byte(ch); }
    }
    result.append("\\\"");
    return result.text();
}

fn codegen_nasmQuoteString(value: ByteBuffer) -> str {
    let parts: []str = [];
    for (let i: i32 = 0; i < value.length(); i++) { array_append(parts, i32_to_str(value.get(i))); }
    if (len(parts) == 0) { return "\\\"\\\""; }
    return string_join(parts, ", ");
}''')
    # The Go break here leaves the enclosing instruction switch.
    s=s.replace('// End of this switch case.', 'return;')
    p.write_text(s)

p=root/'main.nov';s=p.read_text();s=s.replace('import src/runtime;', 'import src/runtime;\nimport src/tool_install;');s+='''
fn main(argc: i32, argv: u64) -> i32 {
    initialize_compiler();
    for (let i: i32 = 0; i < argc; i++) { array_append(CompilerState.arguments, argv_get(argv, i)); }
    let started: i64 = get_time_ns();
    let status: i32 = driver_run();
    if (status == 0) { print("Compile time: " + format_signed((get_time_ns() - started) / 1000000) + "ms"); }
    return status;
}
''';p.write_text(s)
