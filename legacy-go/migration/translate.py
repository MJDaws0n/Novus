"""One-time, type-directed migration of compiler algorithms to Novus source."""
import collections
import hashlib
import json
from pathlib import Path

ROOT = Path("/Users/max/Documents/ChatGPT/Novus/compiler")
SOURCE = json.loads(Path("/tmp/novus-port/source.json").read_text())
TYPES = SOURCE["types"]
RESERVED = {"fn", "class", "new", "pub", "priv", "static", "let", "return",
            "self", "module", "import", "if", "else", "for", "while", "break",
            "continue", "true", "false", "null", "void", "bool", "str", "int",
            "init", "ret", "f32", "f64", "i32", "i64", "u32", "u64", "u8", "i8", "i16", "u16"}
IMMUTABLE = {"CodegenOperand", "AstPosition", "LexerToken"}
NODE_TYPES = {name for name in TYPES
              if name.startswith("novus/internal/ast.") and
              name.rsplit(".", 1)[1] not in {"Position", "ClassBinding", "Param",
                 "TypeExpr", "ClassField", "ClassMethod", "ResolvedField"}}

def ident(name):
    if name in RESERVED:
        return name + "_value"
    if name == "_":
        return "ignored"
    return name

def short_package(package):
    return "driver" if package == "novus/cmd/novus" else package.rsplit("/", 1)[-1]

def named_class(name):
    d = TYPES[name]
    return short_package(d.get("package", "")).title() + d["name"][0].upper() + d["name"][1:]

def pointer_base(name):
    while TYPES.get(name, {}).get("kind") == "pointer":
        name = TYPES[name]["elem"]
    return name

def underlying(name):
    while TYPES.get(name, {}).get("kind") in {"named", "alias"}:
        name = TYPES[name]["underlying"]
    return TYPES.get(name, {})

def is_value_struct(name):
    return TYPES.get(name, {}).get("kind") in {"named", "struct"} and underlying(name).get("kind") == "struct"

def literal(value):
    return json.dumps(value, ensure_ascii=False)

class Translator:
    def __init__(self):
        self.maps = {}
        self.arrays = {}
        self.records = {}
        self.tuples = {}
        self.node_fields = {}
        self.global_fields = {}
        self.global_initializers = []
        self.errors = []
        self.sequence = 0
        self.package = ""
        self.function = ""
        self.lines = []
        self.level = 0
        self.prefix = []
        self.locals = set()
        self.results = []
        self.loop_context = []
        self.function_names = {}
        self.original_functions = {}
        self.runtime_calls = collections.Counter()
        self.literal_factories = {}
        self.method_functions = {}
        for file in SOURCE["files"]:
            for decl in file["ast"]["Decls"] or []:
                if decl["node"] == "FuncDecl":
                    obj = decl["Name"]["object"]
                    name = self.function_name(obj)
                    self.original_functions[name] = (file, decl)
                    sig = TYPES[obj["type"]]
                    if sig.get("receiver"):
                        self.method_functions[(pointer_base(sig["receiver"]), obj["name"])] = name
        self.skip_functions = {
            "driver_main", "codegen_Lowerer_builtinHandlers", "codegen_nasmQuoteString",
            "codegen_getToolsDir", "codegen_EnsureWindowsToolchain", "codegen_findNASM",
            "codegen_findGoLink", "codegen_downloadNASM", "codegen_downloadGoLink",
            "codegen_downloadFile", "codegen_extractZip", "codegen_unzip", "codegen_downloadAndInstallNASM",
        }

    def host_dependent(self, node):
        if isinstance(node, dict):
            obj = node.get("object") or {}
            if obj.get("package") == "runtime" and obj.get("name") in {"GOOS", "GOARCH"}:
                return True
            return any(self.host_dependent(v) for v in node.values())
        if isinstance(node, list):
            return any(self.host_dependent(v) for v in node)
        return False

    def fresh(self, description="temporary"):
        self.sequence += 1
        return description + "_" + str(self.sequence)

    def type(self, name):
        if not name:
            return "void"
        base = pointer_base(name)
        if base in NODE_TYPES:
            return "AstNode"
        if base in {"strings.Builder", "bytes.Buffer"}:
            return "StringBuilder"
        if base == "error":
            return "str"
        d = TYPES.get(name, {})
        kind = d.get("kind")
        if kind == "alias":
            return self.type(d["underlying"])
        if kind == "pointer":
            return self.type(d["elem"])
        if kind == "named":
            if underlying(name).get("kind") in {"basic", "signature"}:
                return self.type(d["underlying"])
            if d.get("package", "").startswith("novus/"):
                return named_class(name)
            if base in {"io/fs.FileInfo", "io/fs.DirEntry"}:
                return "FileInfo"
            if base == "os/exec.Cmd":
                return "ProcessCommand"
            if base == "time.Time":
                return "i64"
            return named_class(name)
        if kind == "basic":
            n = d["name"]
            if n in {"string", "untyped string"}:
                return "str"
            if n in {"bool", "untyped bool"}:
                return "bool"
            if n in {"int64", "untyped int"}:
                return "i64" if n == "int64" else "i32"
            if n == "uint64":
                return "u64"
            if n in {"float32", "float64", "untyped float"}:
                return "f64"
            if n == "untyped nil":
                return "null"
            return "i32"
        if kind in {"slice", "array"}:
            elem = self.type(d["elem"])
            self.arrays.setdefault(elem, d["elem"])
            return "[]" + elem
        if kind == "map":
            key, val = self.type(d["key"]), self.type(d["elem"])
            signature = (key, val)
            if signature not in self.maps:
                self.maps[signature] = ("Map" + self.safe_type(key) + self.safe_type(val), name)
            return self.maps[signature][0]
        if kind == "struct":
            result = "Record" + hashlib.sha1(name.encode()).hexdigest()[:10]
            self.records[result] = name
            return result
        if kind == "tuple":
            return self.tuple(d.get("elems") or [])
        if kind == "signature":
            return "i32"
        if kind == "interface":
            return "str" if name == "error" else "AstNode"
        raise ValueError("Unknown type " + name)

    def safe_type(self, name):
        return name.replace("[]", "Array").replace("_", "").title()

    def tuple(self, elems):
        key = tuple(self.type(e) for e in elems)
        if len(key) == 0:
            return "void"
        if len(key) == 1:
            return key[0]
        result = "Result" + "".join(self.safe_type(x) for x in key)
        self.tuples[result] = list(elems)
        return result

    def zero(self, type_name):
        t = self.type(type_name)
        if t.startswith("[]"):
            return "[]"
        if t == "str":
            return '""'
        if t == "bool":
            return "false"
        if t == "f64":
            return "0.0"
        if t in {"i32", "i64", "u64"}:
            return "0"
        if is_value_struct(type_name):
            return "new " + t + "()"
        return "null"

    def copy(self, expr, go_type):
        if self.type(go_type) in IMMUTABLE:
            return expr
        if is_value_struct(go_type) and not expr.startswith(("make_", "copy_", "new ")):
            return "copy_" + self.type(go_type) + "(" + expr + ")"
        return expr

    def field(self, receiver_type, name, field_type):
        if pointer_base(receiver_type) in NODE_TYPES and name == "Value":
            actual = self.type(field_type)
            if actual == "str":
                return "TextValue"
            if actual == "bool":
                return "BoolValue"
        return ident(name)

    def function_name(self, obj):
        sig = TYPES.get(obj.get("type", ""), {})
        prefix = short_package(obj.get("package", self.package))
        if sig.get("receiver"):
            recv = pointer_base(sig["receiver"])
            receiver_name = TYPES[recv].get("name", recv.rsplit(".", 1)[-1])
            return prefix + "_" + receiver_name + "_" + obj["name"]
        return prefix + "_" + obj["name"]

    def variable(self, node):
        obj = node.get("object") or {}
        name = node["Name"]
        if name == "nil":
            return "null"
        if obj.get("global") and obj.get("package", "").startswith("novus/"):
            return "CompilerState." + short_package(obj["package"]) + "_" + ident(name)
        return ident(name)

    def runtime(self, name, args):
        self.runtime_calls[name] += 1
        return "runtime_" + name + "(" + ", ".join(args) + ")"

    def cast(self, expression, source, target):
        if source == "untyped nil":
            return self.zero(target)
        a, b = self.type(source), self.type(target)
        if a == b or source.startswith("untyped"):
            return expression
        if b == "str":
            if a.startswith("[]"):
                return self.runtime("bytes_to_string", [expression])
            return self.runtime("character", [expression])
        if a == "str" and b.startswith("[]"):
            return self.runtime("string_to_bytes", [expression])
        return self.runtime("to_" + b, [expression])

    def expr(self, n, expected=None):
        if n is None:
            return self.zero(expected) if expected else "null"
        kind = n["node"]
        gt = n.get("goType", "")
        if "constant" in n and kind not in {"Ident", "SelectorExpr"} and not self.host_dependent(n):
            value = n["constant"]
            t = self.type(gt)
            if t == "str":
                return literal(value)
            if t == "f64":
                return repr(float(value)) if "/" not in value else "(" + value.replace("/", ".0 / ") + ".0)"
            return value
        if kind == "Ident":
            obj = n.get("object") or {}
            if "constant" in obj:
                return literal(obj["constant"]) if self.type(gt) == "str" else obj["constant"]
            if n["Name"] == "nil":
                return self.zero(expected or gt)
            if obj.get("kind") == "*types.Func":
                return self.function_name(obj)
            return self.variable(n)
        if kind == "BasicLit":
            if n["Kind"] == "STRING":
                return literal(n.get("constant", n["Value"].strip('"`')))
            return n["Value"]
        if kind == "ParenExpr":
            return "(" + self.expr(n["X"], expected) + ")"
        if kind == "UnaryExpr":
            if n["Op"] in {"&", "*"}:
                return self.expr(n["X"], expected)
            return ("~" if n["Op"] == "^" else n["Op"]) + "(" + self.expr(n["X"]) + ")"
        if kind == "StarExpr":
            return self.expr(n["X"])
        if kind == "BinaryExpr":
            left, right = n["X"], n["Y"]
            ltype, rtype = left.get("goType", ""), right.get("goType", "")
            if right.get("Name") == "nil" and self.type(ltype).startswith("[]"):
                op = "==" if n["Op"] == "==" else "!="
                return "(len(" + self.expr(left) + ") " + op + " 0)"
            if left.get("Name") == "nil" and self.type(rtype).startswith("[]"):
                op = "==" if n["Op"] == "==" else "!="
                return "(len(" + self.expr(right) + ") " + op + " 0)"
            if n["Op"] == "&^":
                return "(" + self.expr(left, rtype) + " & ~(" + self.expr(right, ltype) + "))"
            return "(" + self.expr(left, rtype) + " " + n["Op"] + " " + self.expr(right, ltype) + ")"
        if kind == "SelectorExpr" and self.host_dependent(n):
            name = n["Sel"]["Name"]
            return self.runtime("host_" + ("os" if name == "GOOS" else "arch"), [])
        if kind == "SelectorExpr":
            selection = n.get("selection")
            obj = n["Sel"].get("object") or {}
            if "constant" in obj:
                return literal(obj["constant"]) if self.type(gt) == "str" else obj["constant"]
            if not selection:
                pkg = obj.get("package", "")
                if pkg.startswith("novus/"):
                    if obj.get("kind") == "*types.Func":
                        return self.function_name(obj)
                    return "CompilerState." + short_package(pkg) + "_" + ident(obj["name"])
                if pkg == "runtime" and obj["name"] in {"GOOS", "GOARCH"}:
                    return self.runtime("host_" + ("os" if obj["name"] == "GOOS" else "arch"), [])
                if pkg == "os" and obj["name"] == "Args":
                    return "CompilerState.arguments"
                if pkg == "os" and obj["name"] in {"Stdout", "Stderr"}:
                    return "null"
                return self.runtime("constant_" + pkg.replace("/", "_") + "_" + obj["name"], [])
            if selection["kind"] == 0:
                field = self.field(n["X"].get("goType", ""), n["Sel"]["Name"], gt)
                return self.expr(n["X"]) + "." + field
            return self.function_name(selection["object"])
        if kind == "IndexExpr":
            typ = underlying(n["X"].get("goType", ""))
            obj, index = self.expr(n["X"]), self.expr(n["Index"])
            if typ.get("kind") == "map":
                name = self.type(n["X"]["goType"])
                return name + "_get(" + obj + ", " + index + ")"
            if self.type(n["X"]["goType"]) == "str":
                return "byte_at(" + obj + ", " + index + ")"
            return obj + "[" + index + "]"
        if kind == "SliceExpr":
            obj = self.expr(n["X"])
            low = self.expr(n["Low"]) if n.get("Low") else "0"
            high = self.expr(n["High"]) if n.get("High") else "len(" + obj + ")"
            if self.type(n["X"]["goType"]) == "str":
                return self.runtime("substring", [obj, low, high])
            elem = self.type(n["X"]["goType"])[2:]
            return "slice_" + self.safe_type(elem) + "(" + obj + ", " + low + ", " + high + ")"
        if kind == "TypeAssertExpr":
            return self.expr(n["X"])
        if kind == "CompositeLit":
            typ = underlying(gt)
            if typ.get("kind") in {"slice", "array"}:
                return "[" + ", ".join(self.copy(self.expr(e, typ["elem"]), typ["elem"]) for e in n.get("Elts") or []) + "]"
            if typ.get("kind") == "map":
                name = self.type(gt)
                temp = self.fresh("map")
                self.prefix.append("let " + temp + ": " + name + " = new " + name + "();")
                for e in n.get("Elts") or []:
                    self.prefix.append(name + "_put(" + temp + ", " + self.expr(e["Key"], typ["key"]) + ", " + self.expr(e["Value"], typ["elem"]) + ");")
                return temp
            if typ.get("kind") == "struct":
                fields = typ.get("fields") or []
                values = {}
                for i, e in enumerate(n.get("Elts") or []):
                    if e["node"] == "KeyValueExpr":
                        values[e["Key"]["Name"]] = e["Value"]
                    else:
                        values[fields[i]["name"]] = e
                data = []
                for f in fields:
                    if f["name"] in values:
                        value = self.expr(values[f["name"]], f["type"])
                        data.append((f, self.copy(value, f["type"])))
                key = (gt, tuple(f["name"] for f, _ in data))
                if key not in self.literal_factories:
                    self.literal_factories[key] = "make_" + self.type(gt) + "_" + str(len(self.literal_factories) + 1)
                return self.literal_factories[key] + "(" + ", ".join(x[1] for x in data) + ")"
            raise ValueError("Composite " + gt)
        if kind == "CallExpr":
            return self.call(n)
        raise ValueError("Expression " + kind)

    def formatted(self, fmt_node, args):
        fmt = fmt_node.get("constant")
        if fmt is None:
            return self.runtime("format", [self.expr(fmt_node), "[" + ", ".join(self.stringify(a) for a in args) + "]"])
        import re
        pattern = re.compile(r"%(?:\[(\d+)\])?[-+# 0]*\d*(?:\.\d+)?[a-zA-Z%]")
        chunks = []
        cursor = 0
        index = 0
        for m in pattern.finditer(fmt):
            if m.start() > cursor:
                chunks.append(literal(fmt[cursor:m.start()]))
            spec = m.group()
            if spec == "%%":
                chunks.append(literal("%"))
            else:
                if m.group(1):
                    index = int(m.group(1)) - 1
                if index >= len(args):
                    raise ValueError("Format arguments " + fmt)
                a = args[index]
                index += 1
                rendered = self.stringify(a)
                if spec[-1] == "q":
                    rendered = self.runtime("quote", [rendered])
                elif spec[-1] in {"x", "X", "o", "b", "c"}:
                    rendered = self.runtime("format_integer", [self.expr(a), literal(spec)])
                elif spec not in {"%s", "%d", "%v", "%w", "%t"}:
                    rendered = self.runtime("format_value", [rendered, literal(spec)])
                chunks.append(rendered)
            cursor = m.end()
        if cursor < len(fmt):
            chunks.append(literal(fmt[cursor:]))
        return "(" + " + ".join(chunks or ['""']) + ")"

    def stringify(self, n):
        gt = n.get("goType", "")
        t = self.type(gt)
        value = self.expr(n)
        if t == "str":
            return value
        base = pointer_base(gt)
        if (base, "String") in self.method_functions:
            return self.method_functions[(base, "String")] + "(" + value + ")"
        if (base, "Error") in self.method_functions:
            return self.method_functions[(base, "Error")] + "(" + value + ")"
        return self.runtime("string_" + t, [value])

    def call(self, n):
        f = n["Fun"]
        args = n.get("Args") or []
        obj = ((f.get("Sel") or {}).get("object") if f["node"] == "SelectorExpr" else f.get("object")) or {}
        if f["node"] in {"Ident", "ArrayType"} and (obj.get("kind") == "*types.TypeName" or f["node"] == "ArrayType"):
            return self.cast(self.expr(args[0]), args[0]["goType"], n["goType"])
        if f["node"] == "Ident" and f["Name"] in {"len", "cap", "append", "make", "new", "delete", "max", "min"}:
            name = f["Name"]
            if name in {"len", "cap"}:
                typ = underlying(args[0]["goType"])
                if typ.get("kind") == "map":
                    return self.type(args[0]["goType"]) + "_length(" + self.expr(args[0]) + ")"
                return "len(" + self.expr(args[0]) + ")"
            if name == "make":
                typ = underlying(n["goType"])
                t = self.type(n["goType"])
                if typ["kind"] == "map":
                    return "new " + t + "()"
                return "array_" + self.safe_type(t[2:]) + "(" + self.expr(args[1]) + ")"
            if name == "append":
                t = self.type(n["goType"])
                array = self.expr(args[0], n["goType"])
                if n.get("Ellipsis"):
                    return "extend_" + self.safe_type(t[2:]) + "(" + array + ", " + self.expr(args[1]) + ")"
                elem = underlying(n["goType"])["elem"]
                result = array
                for a in args[1:]:
                    result = "append_" + self.safe_type(t[2:]) + "(" + result + ", " + self.copy(self.expr(a, elem), elem) + ")"
                return result
            if name == "delete":
                return self.type(args[0]["goType"]) + "_delete(" + self.expr(args[0]) + ", " + self.expr(args[1]) + ")"
            return self.runtime(name, [self.expr(a) for a in args])
        selection = f.get("selection") or {}
        if selection:
            obj = selection["object"]
        package, name = obj.get("package", ""), obj.get("name", "")
        if package.startswith("novus/"):
            if name in {"GetPos"} and pointer_base(selection.get("receiver", "")) in NODE_TYPES:
                return self.expr(f["X"]) + ".Pos"
            sig = TYPES[obj["type"]]
            values = []
            if selection:
                values.append(self.expr(f["X"]))
            params = sig.get("params") or []
            if sig.get("variadic"):
                fixed = len(params) - 1
                values.extend(self.expr(a, params[i]["type"]) for i, a in enumerate(args[:fixed]))
                if n.get("Ellipsis"):
                    values.append(self.expr(args[-1]))
                else:
                    values.append("[" + ", ".join(self.expr(a) for a in args[fixed:]) + "]")
            else:
                for i, a in enumerate(args):
                    expected = params[i]["type"] if i < len(params) else a.get("goType", "")
                    values.append(self.copy(self.expr(a, expected), expected))
            return self.function_name(obj) + "(" + ", ".join(values) + ")"
        if package == "fmt":
            if name in {"Sprintf", "Errorf"}:
                return self.formatted(args[0], args[1:])
            if name in {"Printf", "Fprintf"}:
                text_args = args if name == "Printf" else args[1:]
                text = self.formatted(text_args[0], text_args[1:])
                if name == "Fprintf":
                    return self.expr(args[0]) + ".append(" + text + ")"
                return self.runtime("print_raw", [text])
            if name == "Println":
                return self.runtime("print", ["(" + ' + " " + '.join(self.stringify(a) for a in args) + ")" if args else '""'])
        if package == "strings" and selection:
            receiver = self.expr(f["X"])
            names = {"WriteString":"append", "WriteByte":"append_byte", "String":"text", "Reset":"clear", "Len":"length"}
            if name in names:
                return receiver + "." + names[name] + "(" + ", ".join(self.expr(a) for a in args) + ")"
        if package == "strings" and name == "Map":
            return self.runtime("sanitize_name", [self.expr(args[1])])
        if package == "slices":
            t = self.type(args[0]["goType"])
            suffix = self.safe_type(t[2:])
            if name == "Concat":
                result = "[]"
                for arg in args:
                    result = "extend_" + suffix + "(" + result + ", " + self.expr(arg) + ")"
                return result
            return "contains_" + suffix + "(" + ", ".join(self.expr(a) for a in args) + ")"
        if name == "Error" and self.type(f["X"]["goType"]) == "str":
            return self.expr(f["X"])
        if package == "os/exec" and name == "Command":
            tail = self.expr(args[1]) if n.get("Ellipsis") else "[" + ", ".join(self.expr(a) for a in args[1:]) + "]"
            return self.runtime("os_exec_Command", [self.expr(args[0]), tail])
        if package == "path/filepath" and name == "Join":
            tail = self.expr(args[0]) if n.get("Ellipsis") else "[" + ", ".join(self.expr(a) for a in args) + "]"
            return self.runtime("path_filepath_Join", [tail])
        if package:
            values = ([self.expr(f["X"])] if selection else []) + [self.expr(a) for a in args]
            return self.runtime(package.replace("/", "_").replace(".", "_") + "_" + name, values)
        if f["node"] == "Ident" and f["Name"] == "panic":
            return self.runtime("panic", [self.expr(a) for a in args])
        return self.expr(f) + "(" + ", ".join(self.expr(a) for a in args) + ")"

    def emit(self, line=""):
        self.lines.append("    " * self.level + line)

    def flush(self):
        for p in self.prefix:
            self.emit(p)
        self.prefix = []

    def block(self, body):
        self.level += 1
        for s in body or []:
            self.statement(s)
        self.level -= 1

    def assign_one(self, target, value, declare=False):
        if target.get("Name") == "_":
            return
        if target["node"] == "IndexExpr" and underlying(target["X"]["goType"]).get("kind") == "map":
            self.emit(self.type(target["X"]["goType"]) + "_put(" + self.expr(target["X"]) + ", " + self.expr(target["Index"]) + ", " + value + ");")
            return
        lhs = self.expr(target)
        if declare and target.get("defined", True):
            self.locals.add(lhs)
            self.emit("let " + lhs + ": " + self.type(target["goType"]) + " = " + value + ";")
        else:
            self.emit(lhs + " = " + value + ";")

    def statement(self, n):
        try:
            self.statement_inner(n)
        except (ValueError, KeyError, TypeError, IndexError) as error:
            self.errors.append((self.function, n.get("line"), str(error)))
            self.emit("// MIGRATION ERROR " + str(error).replace("\n", " "))

    def statement_inner(self, n):
        kind = n["node"]
        if kind == "BlockStmt":
            self.emit("{")
            self.block(n.get("List"))
            self.emit("}")
        elif kind == "AssignStmt":
            lhs, rhs, op = n["Lhs"], n["Rhs"], n["Tok"]
            declare = op == ":="
            if len(lhs) == 2 and len(rhs) == 1 and rhs[0]["node"] in {"IndexExpr", "TypeAssertExpr"}:
                r = rhs[0]
                if r["node"] == "TypeAssertExpr":
                    source = self.expr(r["X"])
                    type_name = pointer_base(r["Type"]["goType"]).rsplit(".", 1)[-1]
                    value = source
                    found = "(" + source + ' != null && ' + source + ".Kind == " + literal(type_name) + ")"
                else:
                    name = self.type(r["X"]["goType"])
                    source, key = self.expr(r["X"]), self.expr(r["Index"])
                    value = name + "_get(" + source + ", " + key + ")"
                    found = name + "_has(" + source + ", " + key + ")"
                self.flush()
                self.assign_one(lhs[0], value, declare)
                self.assign_one(lhs[1], found, declare)
                return
            if len(lhs) > 1 and len(rhs) == 1:
                value = self.expr(rhs[0])
                t = self.type(rhs[0]["goType"])
                name = self.fresh("result")
                self.flush()
                self.emit("let " + name + ": " + t + " = " + value + ";")
                for i, target in enumerate(lhs):
                    self.assign_one(target, name + ".value" + str(i), declare)
                return
            rendered = []
            for target, expression in zip(lhs, rhs):
                value = self.copy(self.expr(expression, target["goType"]), target["goType"])
                if op not in {"=", ":="}:
                    value = "(" + self.expr(target) + " " + op[:-1] + " " + value + ")"
                rendered.append(value)
            self.flush()
            if len(rendered) > 1:
                for i, value in enumerate(rendered):
                    temp = self.fresh("assigned")
                    self.emit("let " + temp + ": " + self.type(lhs[i]["goType"]) + " = " + value + ";")
                    rendered[i] = temp
            for target, value in zip(lhs, rendered):
                self.assign_one(target, value, declare)
        elif kind == "ExprStmt":
            value = self.expr(n["X"])
            self.flush()
            self.emit(value + ";")
        elif kind == "ReturnStmt":
            args = n.get("Results") or []
            values = [self.copy(self.expr(e, self.results[i]), self.results[i]) for i,e in enumerate(args)]
            self.flush()
            if len(values) > 1:
                self.emit("return make_" + self.tuple(self.results) + "(" + ", ".join(values) + ");")
            else:
                self.emit("return" + (" " + values[0] if values else "") + ";")
        elif kind == "IfStmt":
            if n.get("Init"):
                self.emit("{")
                self.level += 1
                self.statement(n["Init"])
            cond = self.expr(n["Cond"])
            self.flush()
            self.emit("if (" + cond + ") {")
            self.block(n["Body"].get("List"))
            if n.get("Else"):
                self.emit("} else {")
                self.level += 1
                other = n["Else"]
                if other["node"] == "BlockStmt":
                    for s in other.get("List") or []: self.statement(s)
                else:
                    self.statement(other)
                self.level -= 1
            self.emit("}")
            if n.get("Init"):
                self.level -= 1
                self.emit("}")
        elif kind == "IncDecStmt":
            expression = self.expr(n["X"])
            self.flush()
            if n["X"]["node"] == "IndexExpr" and underlying(n["X"]["X"]["goType"]).get("kind") == "map":
                self.assign_one(n["X"], "(" + expression + (" + 1)" if n["Tok"] == "++" else " - 1)"))
            else:
                self.emit(expression + n["Tok"] + ";")
        elif kind == "DeclStmt":
            for spec in n["Decl"].get("Specs") or []:
                if spec["node"] == "TypeSpec":
                    continue
                for i, name in enumerate(spec.get("Names") or []):
                    values = spec.get("Values") or []
                    value = self.expr(values[i], name["goType"]) if i < len(values) else self.zero(name["goType"])
                    self.flush()
                    self.assign_one(name, value, True)
        elif kind == "ForStmt":
            self.emit("{")
            self.level += 1
            if n.get("Init"): self.statement(n["Init"])
            cond = self.expr(n["Cond"]) if n.get("Cond") else "true"
            self.flush()
            post_lines = []
            if n.get("Post"):
                saved = self.lines
                self.lines = []
                self.statement(n["Post"])
                post_lines = [x.strip().rstrip(";") for x in self.lines]
                self.lines = saved
            if len(post_lines) > 1: raise ValueError("Complex loop update")
            post = post_lines[0] if post_lines else ""
            self.emit("for (; " + cond + "; " + post + ") {")
            self.loop_context.append("loop")
            self.block(n["Body"].get("List"))
            self.loop_context.pop()
            self.emit("}")
            self.level -= 1
            self.emit("}")
        elif kind == "RangeStmt":
            expression = self.expr(n["X"])
            go_type = n["X"]["goType"]
            typ = underlying(go_type)
            self.flush()
            self.emit("{")
            self.level += 1
            collection = self.fresh("items")
            self.emit("let " + collection + ": " + self.type(go_type) + " = " + expression + ";")
            index = self.fresh("index")
            limit = self.type(go_type) + "_length(" + collection + ")" if typ.get("kind") == "map" else "len(" + collection + ")"
            count = self.fresh("count")
            self.emit("let " + count + ": i32 = " + limit + ";")
            self.emit("for (let " + index + ": i32 = 0; " + index + " < " + count + "; " + index + "++) {")
            self.level += 1
            key, value = n.get("Key"), n.get("Value")
            if typ.get("kind") == "map":
                self.assign_one(key, collection + ".keys[" + index + "]", n["Tok"] == ":=")
                if value: self.assign_one(value, collection + ".values[" + index + "]", n["Tok"] == ":=")
            else:
                if key: self.assign_one(key, index, n["Tok"] == ":=")
                if value:
                    item = "byte_at(" + collection + ", " + index + ")" if self.type(go_type) == "str" else collection + "[" + index + "]"
                    self.assign_one(value, self.copy(item, value["goType"]), n["Tok"] == ":=")
            self.loop_context.append("loop")
            for s in n["Body"].get("List") or []: self.statement(s)
            self.loop_context.pop()
            self.level -= 1
            self.emit("}")
            self.level -= 1
            self.emit("}")
        elif kind in {"SwitchStmt", "TypeSwitchStmt"}:
            self.emit("{")
            self.level += 1
            if n.get("Init"): self.statement(n["Init"])
            switch_value = self.fresh("choice")
            if kind == "TypeSwitchStmt":
                assign = n["Assign"]
                assertion = assign["Rhs"][0] if assign["node"] == "AssignStmt" else assign["X"]
                expression = self.expr(assertion["X"])
                switched = assign["Lhs"][0] if assign["node"] == "AssignStmt" else None
                self.emit("let " + switch_value + ": AstNode = " + expression + ";")
            else:
                tag = n.get("Tag")
                if tag:
                    expression = self.expr(tag)
                    self.flush()
                    self.emit("let " + switch_value + ": " + self.type(tag["goType"]) + " = " + expression + ";")
            default = None
            first = True
            self.loop_context.append("switch")
            for case in n["Body"].get("List") or []:
                matches = case.get("List") or []
                if not matches:
                    default = case
                    continue
                if kind == "TypeSwitchStmt":
                    conditions = [switch_value + " != null && " + switch_value + ".Kind == " + literal(pointer_base(x["goType"]).rsplit(".", 1)[-1]) for x in matches]
                elif n.get("Tag"):
                    conditions = [switch_value + " == " + self.expr(x) for x in matches]
                else:
                    conditions = [self.expr(x) for x in matches]
                self.flush()
                self.emit(("if" if first else "} else if") + " (" + " || ".join("(" + c + ")" for c in conditions) + ") {")
                first = False
                self.level += 1
                if kind == "TypeSwitchStmt" and switched:
                    self.emit("let " + ident(switched["Name"]) + ": AstNode = " + switch_value + ";")
                for s in case.get("Body") or []: self.statement(s)
                self.level -= 1
            if default:
                self.emit("{" if first else "} else {")
                self.block(default.get("Body"))
            if not first or default: self.emit("}")
            self.loop_context.pop()
            self.level -= 1
            self.emit("}")
        elif kind == "BranchStmt":
            if n.get("Label"):
                raise ValueError("Labelled branch " + n["Label"]["Name"])
            if n["Tok"] == "break" and self.loop_context and self.loop_context[-1] == "switch":
                self.emit("// End of this switch case.")
            else:
                self.emit(n["Tok"] + ";")
        elif kind == "LabeledStmt":
            self.statement(n["Stmt"])
        elif kind == "EmptyStmt":
            pass
        else:
            raise ValueError("Statement " + kind)

    def translate_function(self, file, decl):
        self.package = file["package"]
        obj = decl["Name"]["object"]
        self.function = self.function_name(obj)
        if self.function in self.skip_functions or self.function.endswith(("_stmtNode", "_exprNode")):
            return []
        self.prefix, self.lines, self.level = [], [], 0
        sig = TYPES[obj["type"]]
        params = []
        if decl.get("Recv"):
            recv = decl["Recv"]["List"][0]
            params.append(ident(recv["Names"][0]["Name"]) + ": " + self.type(sig["receiver"]))
        for param in sig.get("params") or []:
            params.append(ident(param["name"]) + ": " + self.type(param["type"]))
        self.results = sig.get("results") or []
        result = self.tuple(self.results)
        doc = decl.get("Doc")
        if doc:
            for comment in doc.get("List") or []:
                self.emit(comment["Text"])
        self.emit("fn " + self.function + "(" + ", ".join(params) + ") -> " + result + " {")
        self.level += 1
        for result_field in ((decl.get("Type") or {}).get("Results") or {}).get("List") or []:
            for result_name in result_field.get("Names") or []:
                self.emit("let " + ident(result_name["Name"]) + ": " + self.type(result_name["goType"]) + " = " + self.zero(result_name["goType"]) + ";")
        self.level -= 1
        self.block(decl["Body"].get("List"))
        self.emit("}")
        return self.lines + [""]

    def build(self):
        out = ROOT / "src"
        out.mkdir(exist_ok=True)
        files = {}
        for file in sorted(SOURCE["files"], key=lambda x: x["path"]):
            name = short_package(file["package"]) + "_" + Path(file["path"]).stem
            self.package = file["package"]
            lines = ["module " + name + ";", "", "import model;", "import runtime;", ""]
            for decl in file["ast"]["Decls"] or []:
                if decl["node"] == "FuncDecl":
                    lines.extend(self.translate_function(file, decl))
                elif decl["node"] == "GenDecl" and decl["Tok"] == "var":
                    for spec in decl.get("Specs") or []:
                        for i, n in enumerate(spec.get("Names") or []):
                            field = short_package(file["package"]) + "_" + ident(n["Name"])
                            self.global_fields[field] = n["goType"]
                            if spec.get("Values"):
                                self.prefix = []
                                value = self.expr(spec["Values"][i], n["goType"])
                                self.global_initializers.extend(self.prefix)
                                self.global_initializers.append("CompilerState." + field + " = " + value + ";")
            files[name] = lines
        for name, lines in files.items():
            (out / (name + ".nov")).write_text("\n".join(lines) + "\n")
        self.write_model(out)
        (ROOT / "main.nov").write_text("module novus;\n\n" + "\n".join("import src/" + f + ";" for f in files) + "\nimport src/model;\nimport src/runtime;\n")
        Path("/tmp/novus-port/errors.json").write_text(json.dumps(self.errors, indent=2))
        Path("/tmp/novus-port/runtime-needed.json").write_text(json.dumps(self.runtime_calls, indent=2))

    def write_model(self, out):
        lines = ["module model;", "", "import ../lib/std;", "import ../lib/file_io;", "import ../lib/process;", ""]
        structs = {}
        for name, d in list(TYPES.items()):
            if d["kind"] == "named" and d.get("package", "").startswith("novus/") and underlying(name).get("kind") == "struct":
                fields = underlying(name).get("fields") or []
                if name in NODE_TYPES:
                    for f in fields:
                        fn = self.field(name, f["name"], f["type"])
                        old = self.node_fields.get(fn)
                        if old and self.type(old) != self.type(f["type"]):
                            raise ValueError("Node field conflict " + fn)
                        self.node_fields[fn] = f["type"]
                else:
                    structs[self.type(name)] = (name, fields)
        structs["AstNode"] = ("*novus/internal/ast.Expr", [{"name":n,"type":t} for n,t in self.node_fields.items()])
        for name, gt in list(self.records.items()):
            structs[name] = (gt, underlying(gt).get("fields") or [])
        for name, (gt, fields) in structs.items():
            lines.append("class " + name + " {")
            if name in IMMUTABLE:
                lines.append("    pub static let zero: " + name + " = new " + name + "();")
            if name == "AstNode":
                lines.append("    pub let Kind: str;")
            for f in fields:
                field = f["name"] if name == "AstNode" else ident(f["name"])
                default = self.zero(f["type"])
                if is_value_struct(f["type"]) and self.type(f["type"]) in IMMUTABLE:
                    default = self.type(f["type"]) + ".zero"
                lines.append("    pub let " + field + ": " + self.type(f["type"]) + " = " + default + ";")
            lines.extend(["}", ""])
            if True:
                lines.append("fn copy_" + name + "(value: " + name + ") -> " + name + " {")
                if name in IMMUTABLE:
                    lines.extend(["    return value;", "}", ""])
                    continue
                lines.append("    let result: " + name + " = new " + name + "();")
                if name == "AstNode": lines.append("    result.Kind = value.Kind;")
                for f in fields:
                    field = ident(f["name"])
                    lines.append("    result." + field + " = " + self.copy("value." + field, f["type"]) + ";")
                lines.extend(["    return result;", "}", ""])
        lines.append("class CompilerState {")
        lines.append("    pub static let arguments: []str = [];")
        for name, t in self.global_fields.items():
            lines.append("    pub static let " + name + ": " + self.type(t) + " = " + self.zero(t) + ";")
        lines.extend(["}", "", "fn initialize_compiler() -> void {"])
        lines.extend("    " + x for x in self.global_initializers)
        lines.extend(["}", ""])
        for (gt, selected), factory in self.literal_factories.items():
            t = self.type(gt)
            fields = {x["name"]:x for x in underlying(gt).get("fields") or []}
            params = [ident(n) + ": " + self.type(fields[n]["type"]) for n in selected]
            lines.append("fn " + factory + "(" + ", ".join(params) + ") -> " + t + " {")
            lines.append("    let result: " + t + " = new " + t + "();")
            if pointer_base(gt) in NODE_TYPES:
                lines.append("    result.Kind = " + literal(pointer_base(gt).rsplit(".",1)[-1]) + ";")
            for n in selected:
                lines.append("    result." + self.field(gt,n,fields[n]["type"]) + " = " + ident(n) + ";")
            lines.extend(["    return result;", "}", ""])
        for name, elems in self.tuples.items():
            lines.append("class " + name + " {")
            for i,e in enumerate(elems):
                lines.append("    pub let value" + str(i) + ": " + self.type(e) + " = " + self.zero(e) + ";")
            lines.extend(["}", ""])
            params = ["value" + str(i) + ": " + self.type(e) for i,e in enumerate(elems)]
            lines.append("fn make_" + name + "(" + ", ".join(params) + ") -> " + name + " {")
            lines.append("    let result: " + name + " = new " + name + "();")
            for i in range(len(elems)):
                lines.append("    result.value" + str(i) + " = value" + str(i) + ";")
            lines.extend(["    return result;", "}", ""])
        for (key,val),(name,gt) in list(self.maps.items()):
            zero = self.zero(underlying(gt)["elem"])
            lines.extend(self.map_source(name,key,val,zero))
        for elem,gt in list(self.arrays.items()):
            lines.extend(self.array_source(elem,gt))
        (out / "model.nov").write_text("\n".join(lines) + "\n")

    def map_source(self,name,key,value,zero):
        return [
            "class " + name + " {",
            "    pub let keys: []" + key + " = [];",
            "    pub let values: []" + value + " = [];",
            "}", "",
            "fn " + name + "_index(map: " + name + ", key: " + key + ") -> i32 {",
            "    if (map == null) { return -1; }",
            "    for (let i: i32 = 0; i < len(map.keys); i++) {",
            "        if (map.keys[i] == key) { return i; }",
            "    }",
            "    return -1;", "}", "",
            "fn " + name + "_has(map: " + name + ", key: " + key + ") -> bool {",
            "    return " + name + "_index(map, key) >= 0;", "}", "",
            "fn " + name + "_get(map: " + name + ", key: " + key + ") -> " + value + " {",
            "    let index: i32 = " + name + "_index(map, key);",
            "    if (index < 0) { return " + zero + "; }",
            "    return map.values[index];", "}", "",
            "fn " + name + "_put(map: " + name + ", key: " + key + ", value: " + value + ") -> void {",
            "    let index: i32 = " + name + "_index(map, key);",
            "    if (index >= 0) { map.values[index] = value; return; }",
            "    array_append(map.keys, key);",
            "    array_append(map.values, value);", "}", "",
            "fn " + name + "_length(map: " + name + ") -> i32 {",
            "    if (map == null) { return 0; }",
            "    return len(map.keys);", "}", "",
            "fn " + name + "_delete(map: " + name + ", key: " + key + ") -> void {",
            "    let index: i32 = " + name + "_index(map, key);",
            "    if (index < 0) { return; }",
            "    for (let i: i32 = index; i + 1 < len(map.keys); i++) {",
            "        map.keys[i] = map.keys[i + 1];",
            "        map.values[i] = map.values[i + 1];",
            "    }",
            "    array_pop(map.keys);",
            "    array_pop(map.values);", "}", ""]

    def array_source(self,elem,gt):
        suffix=self.safe_type(elem)
        return [
            "fn array_" + suffix + "(count: i32) -> []" + elem + " {",
            "    let result: []" + elem + " = [];",
            "    for (let i: i32 = 0; i < count; i++) { array_append(result, " + self.zero(gt) + "); }",
            "    return result;", "}", "",
            "fn append_" + suffix + "(items: []" + elem + ", value: " + elem + ") -> []" + elem + " {",
            "    array_append(items, value);",
            "    return items;", "}", "",
            "fn extend_" + suffix + "(items: []" + elem + ", more: []" + elem + ") -> []" + elem + " {",
            "    let count: i32 = len(more);",
            "    for (let i: i32 = 0; i < count; i++) { array_append(items, more[i]); }",
            "    return items;", "}", "",
            "fn slice_" + suffix + "(items: []" + elem + ", start: i32, end: i32) -> []" + elem + " {",
            "    let result: []" + elem + " = [];",
            "    for (let i: i32 = start; i < end; i++) { array_append(result, items[i]); }",
            "    return result;", "}", "",
            "fn contains_" + suffix + "(items: []" + elem + ", value: " + elem + ") -> bool {",
            "    for (let i: i32 = 0; i < len(items); i++) {",
            "        if (items[i] == value) { return true; }",
            "    }",
            "    return false;", "}", ""]

if __name__ == "__main__":
    t=Translator()
    t.build()
    print("Migration diagnostics",len(t.errors))
    for error in t.errors[:40]:
        print(error)
    print("Runtime adapters",len(t.runtime_calls))
