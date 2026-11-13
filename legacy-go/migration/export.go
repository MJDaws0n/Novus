package main

import (
    "encoding/json"
    "fmt"
    "go/ast"
    "go/constant"
    "go/importer"
    "go/parser"
    "go/token"
    "go/types"
    "os"
    "path/filepath"
    "reflect"
    "strings"
)

type loader struct {
    root string
    set *token.FileSet
    fallback types.Importer
    packages map[string]*types.Package
    files map[string][]*ast.File
    infos map[string]*types.Info
    descriptions map[string]any
}

func (l *loader) Import(path string) (*types.Package, error) {
    if !strings.HasPrefix(path, "novus/") {
        return l.fallback.Import(path)
    }
    if p := l.packages[path]; p != nil {
        return p, nil
    }
    dir := filepath.Join(l.root, strings.TrimPrefix(path, "novus/"))
    entries, err := os.ReadDir(dir)
    if err != nil { return nil, err }
    var files []*ast.File
    for _, entry := range entries {
        if !strings.HasSuffix(entry.Name(), ".go") || strings.HasSuffix(entry.Name(), "_test.go") {
            continue
        }
        f, err := parser.ParseFile(l.set, filepath.Join(dir,entry.Name()), nil, parser.ParseComments)
        if err != nil { return nil, err }
        files = append(files, f)
    }
    info := &types.Info{
        Types: map[ast.Expr]types.TypeAndValue{},
        Defs: map[*ast.Ident]types.Object{},
        Uses: map[*ast.Ident]types.Object{},
        Selections: map[*ast.SelectorExpr]*types.Selection{},
        Implicits: map[ast.Node]types.Object{},
    }
    cfg := types.Config{Importer:l}
    pkg, err := cfg.Check(path,l.set,files,info)
    if err != nil { return nil, err }
    l.packages[path], l.files[path], l.infos[path] = pkg, files, info
    return pkg,nil
}

func typeName(t types.Type) string {
    if t == nil { return "" }
    name := types.TypeString(t, func(p *types.Package) string { return p.Path() })
    if sig, ok := t.(*types.Signature); ok && sig.Recv() != nil {
        return typeName(sig.Recv().Type()) + "." + name
    }
    return name
}

func (l *loader) describe(t types.Type) string {
    name := typeName(t)
    if t == nil || l.descriptions[name] != nil { return name }
    d := map[string]any{}
    l.descriptions[name] = d
    switch t := t.(type) {
    case *types.Alias:
        d["kind"], d["underlying"] = "alias", l.describe(types.Unalias(t))
    case *types.Named:
        d["kind"], d["name"] = "named",t.Obj().Name()
        if t.Obj().Pkg()!=nil { d["package"] = t.Obj().Pkg().Path() }
        d["underlying"] = l.describe(t.Underlying())
    case *types.Pointer:
        d["kind"],d["elem"] = "pointer",l.describe(t.Elem())
    case *types.Basic:
        d["kind"],d["name"] = "basic",t.Name()
    case *types.Slice:
        d["kind"],d["elem"] = "slice",l.describe(t.Elem())
    case *types.Array:
        d["kind"],d["elem"],d["length"] = "array",l.describe(t.Elem()),t.Len()
    case *types.Map:
        d["kind"],d["key"],d["elem"] = "map",l.describe(t.Key()),l.describe(t.Elem())
    case *types.Struct:
        d["kind"] = "struct"
        var fields []any
        for i:=0;i<t.NumFields();i++ {
            f:=t.Field(i)
            fields=append(fields,map[string]any{"name":f.Name(),"type":l.describe(f.Type())})
        }
        d["fields"]=fields
    case *types.Signature:
        d["kind"], d["variadic"] = "signature", t.Variadic()
        var params, results []any
        for i:=0;i<t.Params().Len();i++ {
            v:=t.Params().At(i)
            params=append(params,map[string]any{"name":v.Name(),"type":l.describe(v.Type())})
        }
        for i:=0;i<t.Results().Len();i++ {
            results=append(results,l.describe(t.Results().At(i).Type()))
        }
        d["params"],d["results"]=params,results
        if t.Recv()!=nil { d["receiver"]=l.describe(t.Recv().Type()) }
    case *types.Tuple:
        d["kind"]="tuple"
        var elems []any
        for i:=0;i<t.Len();i++ {elems=append(elems,l.describe(t.At(i).Type()))}
        d["elems"]=elems
    case *types.Interface:
        d["kind"]="interface"
    default:
        d["kind"]=fmt.Sprintf("%T",t)
    }
    return name
}

func (l *loader) object(obj types.Object) any {
    if obj==nil {return nil}
    d:=map[string]any{"name":obj.Name(),"type":l.describe(obj.Type()),"kind":fmt.Sprintf("%T",obj)}
    if obj.Pkg()!=nil {
        d["package"]=obj.Pkg().Path()
        d["global"]=obj.Parent()==obj.Pkg().Scope()
    }
    if c,ok:=obj.(*types.Const);ok {
        d["constant"]=c.Val().ExactString()
        if c.Val().Kind()==constant.String { d["constant"]=constant.StringVal(c.Val()) }
    }
    return d
}

func (l *loader) node(n ast.Node, info *types.Info) any {
    if n==nil || reflect.ValueOf(n).IsNil() {return nil}
    v:=reflect.ValueOf(n).Elem()
    d:=map[string]any{"node":v.Type().Name(),"line":l.set.Position(n.Pos()).Line}
    if expr,ok:=n.(ast.Expr);ok {
        d["goType"]=l.describe(info.TypeOf(expr))
        if tv,ok:=info.Types[expr];ok && tv.Value!=nil {
            d["constant"]=tv.Value.ExactString()
            if tv.Value.Kind()==constant.String {d["constant"]=constant.StringVal(tv.Value)}
        }
    }
    switch n:=n.(type) {
    case *ast.Ident:
        obj:=info.Uses[n]
        if obj==nil {obj=info.Defs[n]}
        d["object"]=l.object(obj)
        d["defined"]=info.Defs[n]!=nil
    case *ast.SelectorExpr:
        if s:=info.Selections[n];s!=nil {
            d["selection"]=map[string]any{"object":l.object(s.Obj()),"receiver":l.describe(s.Recv()),"kind":int(s.Kind())}
        }
    }
    for i:=0;i<v.NumField();i++ {
        name:=v.Type().Field(i).Name
        if name=="Obj" || name=="Scope" || name=="Unresolved" || name=="Comments" {continue}
        field:=v.Field(i)
        if field.Type()==reflect.TypeOf(token.Pos(0)) { if name=="Ellipsis" { d[name]=field.Interface().(token.Pos).IsValid() }; continue }
        if field.Type()==reflect.TypeOf(token.Token(0)) {d[name]=field.Interface().(token.Token).String();continue}
        if field.Kind()==reflect.String || field.Kind()==reflect.Bool {d[name]=field.Interface();continue}
        if child,ok:=field.Interface().(ast.Node);ok {d[name]=l.node(child,info);continue}
        if field.Kind()==reflect.Slice {
            var values []any
            for j:=0;j<field.Len();j++ {
                if child,ok:=field.Index(j).Interface().(ast.Node);ok {values=append(values,l.node(child,info))}
            }
            d[name]=values
        }
    }
    return d
}

func main() {
    l:=&loader{root:os.Args[1],set:token.NewFileSet(),fallback:importer.Default(),packages:map[string]*types.Package{},files:map[string][]*ast.File{},infos:map[string]*types.Info{},descriptions:map[string]any{}}
    if _,err:=l.Import("novus/cmd/novus");err!=nil {panic(err)}
    var files []any
    for path, fs:=range l.files {
        for _,f:=range fs {
            files=append(files,map[string]any{"package":path,"path":l.set.Position(f.Pos()).Filename,"ast":l.node(f,l.infos[path])})
        }
    }
    out,err:=os.Create(os.Args[2])
    if err!=nil {panic(err)}
    defer out.Close()
    if err:=json.NewEncoder(out).Encode(map[string]any{"files":files,"types":l.descriptions});err!=nil {panic(err)}
}
