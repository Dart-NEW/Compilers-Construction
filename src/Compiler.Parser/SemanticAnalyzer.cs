using System;
using System.Collections.Generic;
using System.Linq;

namespace Compiler.Parser;

public sealed class SemanticDiagnostic
{
    public string Message { get; }
    public SemanticDiagnostic(string message) => Message = message;
    public override string ToString() => Message;
}

public sealed class SemanticAnalyzer
{
    private readonly List<SemanticDiagnostic> _diagnostics = new();

    public IReadOnlyList<SemanticDiagnostic> Diagnostics => _diagnostics;

    private sealed class ClassSymbol
    {
        public string Name { get; }
        public ClassSymbol? BaseClass { get; set; }
        public Dictionary<string, List<MethodNode>> Methods { get; } = new(StringComparer.Ordinal);
        public Dictionary<string, VariableNode> Fields { get; } = new(StringComparer.Ordinal);
        public ClassSymbol(string name) => Name = name;
    }

    private sealed class Scope
    {
        private readonly Scope? _parent;
        private readonly Dictionary<string, string> _variables = new(StringComparer.Ordinal);

        public Scope(Scope? parent) => _parent = parent;
        public bool Declare(string name, string type)
        {
            if (_variables.ContainsKey(name)) return false;
            _variables[name] = type;
            return true;
        }
        public string? Lookup(string name)
        {
            if (_variables.TryGetValue(name, out var t)) return t;
            return _parent?.Lookup(name);
        }
    }

    private readonly Dictionary<string, ClassSymbol> _classes = new(StringComparer.Ordinal);

    public void Analyze(ProgramNode program)
    {
        _diagnostics.Clear();
        _classes.Clear();

        // Collect classes
        foreach (var cls in program.Classes)
        {
            if (_classes.ContainsKey(cls.Name))
            {
                Report($"Duplicate class '{cls.Name}'.");
                continue;
            }
            _classes[cls.Name] = new ClassSymbol(cls.Name);
        }

        // Resolve inheritance
        foreach (var cls in program.Classes)
        {
            if (cls.Parent is not null)
            {
                if (!_classes.TryGetValue(cls.Parent.Name, out var baseSym))
                {
                    Report($"Unknown base class '{cls.Parent.Name}' for '{cls.Name}'.");
                }
                else
                {
                    _classes[cls.Name].BaseClass = baseSym;
                }
            }
        }

        // Detect inheritance cycles (simple tortoise-hare per class)
        foreach (var entry in _classes.Values)
        {
            var tortoise = entry;
            var hare = entry.BaseClass;
            while (hare is not null && hare.BaseClass is not null)
            {
                if (ReferenceEquals(tortoise, hare))
                {
                    Report($"Inheritance cycle detected involving '{entry.Name}'.");
                    break;
                }
                tortoise = tortoise.BaseClass ?? tortoise;
                hare = hare.BaseClass?.BaseClass;
            }
        }

        // Collect members and check duplicates
        foreach (var cls in program.Classes)
        {
            var classSym = _classes[cls.Name];
            foreach (var member in cls.Members)
            {
                switch (member)
                {
                    case VariableNode field:
                        if (classSym.Fields.ContainsKey(field.Name))
                            Report($"Duplicate field '{field.Name}' in class '{cls.Name}'.")
                            ;
                        else
                            classSym.Fields[field.Name] = field;
                        break;
                    case MethodNode method:
                        if (!classSym.Methods.TryGetValue(method.Name, out var overloads))
                        {
                            overloads = new List<MethodNode>();
                            classSym.Methods[method.Name] = overloads;
                        }
                        overloads.Add(method);
                        break;
                    case ConstructorNode:
                        // Allow single constructor for now
                        break;
                }
            }
        }

        // Analyze bodies
        foreach (var cls in program.Classes)
        {
            AnalyzeClass(cls);
        }
    }

    private void AnalyzeClass(ClassNode cls)
    {
        var scope = new Scope(parent: null);
        // Fields available as names in methods via 'this'
        foreach (var member in cls.Members)
        {
            if (member is VariableNode f)
            {
                scope.Declare(f.Name, f.Type);
                if (f.Initializer is not null)
                {
                    AnalyzeExpression(f.Initializer, scope, cls);
                }
            }
        }

        foreach (var member in cls.Members)
        {
            switch (member)
            {
                case ConstructorNode ctor:
                    AnalyzeConstructor(ctor, cls);
                    break;
                case MethodNode method:
                    AnalyzeMethod(method, cls);
                    break;
            }
        }
    }

    private void AnalyzeConstructor(ConstructorNode ctor, ClassNode cls)
    {
        var scope = new Scope(parent: null);
        foreach (var p in ctor.Parameters)
        {
            if (!scope.Declare(p.Name, p.Type))
                Report($"Duplicate constructor parameter '{p.Name}' in class '{cls.Name}'.");
            EnsureTypeExists(p.Type);
        }
        foreach (var stmt in ctor.Body)
            AnalyzeStatement(stmt, scope, cls, expectedReturnType: null);
    }

    private void AnalyzeMethod(MethodNode method, ClassNode cls)
    {
        var scope = new Scope(parent: null);
        foreach (var p in method.Parameters)
        {
            if (!scope.Declare(p.Name, p.Type))
                Report($"Duplicate parameter '{p.Name}' in method '{cls.Name}.{method.Name}'.");
            EnsureTypeExists(p.Type);
        }
        if (!string.IsNullOrEmpty(method.ReturnType))
            EnsureTypeExists(method.ReturnType);

        foreach (var stmt in method.Body)
            AnalyzeStatement(stmt, scope, cls, string.IsNullOrEmpty(method.ReturnType) ? null : method.ReturnType);
    }

    private void AnalyzeStatement(StatementNode stmt, Scope scope, ClassNode cls, string? expectedReturnType)
    {
        switch (stmt)
        {
            case VariableStatementNode v:
                var varType = !string.IsNullOrWhiteSpace(v.Type) 
                    ? v.Type 
                    : (v.Initializer is ConstructorCallNode cc ? cc.ClassName : "");
                if (!scope.Declare(v.Name, varType))
                    Report($"Variable '{v.Name}' already defined in this scope.");
                if (v.Initializer is not null)
                    AnalyzeExpression(v.Initializer, scope, cls);
                break;
            case IfNode ifn:
                AnalyzeExpression(ifn.Condition, scope, cls);
                var thenScope = new Scope(scope);
                foreach (var s in ifn.ThenBody) AnalyzeStatement(s, thenScope, cls, expectedReturnType);
                if (ifn.ElseBody is not null)
                {
                    var elseScope = new Scope(scope);
                    foreach (var s in ifn.ElseBody) AnalyzeStatement(s, elseScope, cls, expectedReturnType);
                }
                break;
            case WhileNode wh:
                AnalyzeExpression(wh.Condition, scope, cls);
                var loopScope = new Scope(scope);
                foreach (var s in wh.Body) AnalyzeStatement(s, loopScope, cls, expectedReturnType);
                break;
            case ReturnNode ret:
                var retType = AnalyzeExpression(ret.Expression, scope, cls);
                if (expectedReturnType is not null)
                {
                    // Simple compatibility check: exact match if both are known
                    if (!string.IsNullOrEmpty(retType) && !TypeCompatible(retType, expectedReturnType))
                        Report($"Return type mismatch: expected '{expectedReturnType}', got '{retType}'.");
                }
                break;
            case ExpressionStatementNode es:
                AnalyzeExpression(es.Expression, scope, cls);
                break;
            default:
                break;
        }
    }

    private string AnalyzeExpression(ExpressionNode expr, Scope scope, ClassNode cls)
    {
        switch (expr)
        {
            case IdentifierNode id:
                // Could be variable or field name
                var t = scope.Lookup(id.Name);
                if (t is null)
                {
                    if (_classes.TryGetValue(cls.Name, out var clsSym))
                    {
                        var fieldType = FindFieldType(clsSym, id.Name);
                        if (fieldType is not null)
                            return fieldType; // may be empty string, but identifier is valid
                    }
                    // Allow class names in constructor position; otherwise undefined identifier
                    if (!_classes.ContainsKey(id.Name))
                        Report($"Undefined identifier '{id.Name}'.");
                }
                return t ?? string.Empty;

            case AssignmentNode a:
                var lt = AnalyzeExpression(a.Target, scope, cls);
                var rt = AnalyzeExpression(a.Value, scope, cls);
                if (!string.IsNullOrEmpty(lt) && !string.IsNullOrEmpty(rt) && !TypeCompatible(rt, lt))
                    Report($"Cannot assign '{rt}' to '{lt}'.");
                return lt;

            case CallNode c:
                // Resolve call target type, but with this simple AST we can't infer much; still check args
                foreach (var arg in c.Arguments)
                    AnalyzeExpression(arg, scope, cls);
                return string.Empty;

            case ConstructorCallNode cc:
                // Treat as method call on 'this' if not a known type
                if (!_classes.ContainsKey(cc.ClassName) && !IsPrimitive(cc.ClassName) && !IsContainerType(cc.ClassName))
                {
                    if (_classes.TryGetValue(cls.Name, out var recvClass))
                    {
                        var method = FindMethod(recvClass, cc.ClassName, cc.Arguments.Count);
                        if (method is not null)
                        {
                            foreach (var arg in cc.Arguments) AnalyzeExpression(arg, scope, cls);
                            return method.ReturnType;
                        }
                    }
                    Report($"Unknown type '{cc.ClassName}' in construction.");
                }
                foreach (var arg in cc.Arguments)
                    AnalyzeExpression(arg, scope, cls);
                return cc.ClassName;

            case MemberAccessNode ma:
                var receiverType = AnalyzeExpression(ma.Target, scope, cls);
                if (ma.Member is CallNode mc && mc.Target is IdentifierNode mname)
                {
                    // Analyze arguments first
                    foreach (var a in mc.Arguments) AnalyzeExpression(a, scope, cls);
                    // Skip strict checking for primitives
                    if (IsPrimitive(receiverType) || string.IsNullOrEmpty(receiverType)) return string.Empty;
                    if (!_classes.TryGetValue(receiverType, out var recvClass)) return string.Empty;
                    var method = FindMethod(recvClass, mname.Name, mc.Arguments.Count);
                    if (method is null)
                    {
                        Report($"Unknown method '{mname.Name}'({mc.Arguments.Count}) on '{receiverType}'.");
                        return string.Empty;
                    }
                    return method.ReturnType;
                }
                else if (ma.Member is IdentifierNode fieldName)
                {
                    if (IsPrimitive(receiverType) || string.IsNullOrEmpty(receiverType)) return string.Empty;
                    if (!_classes.TryGetValue(receiverType, out var recvClass)) return string.Empty;
                    var fieldType = FindFieldType(recvClass, fieldName.Name);
                    if (fieldType is null)
                        Report($"Unknown member '{fieldName.Name}' on '{receiverType}'.");
                    return fieldType ?? string.Empty;
                }
                else
                {
                    AnalyzeExpression(ma.Member, scope, cls);
                    return string.Empty;
                }

            case IndexAccessNode ia:
                AnalyzeExpression(ia.Target, scope, cls);
                AnalyzeExpression(ia.Index, scope, cls);
                return string.Empty;

            case StringLiteralNode:
                return "String";
            case IntegerLiteralNode:
                return "Integer";
            case RealLiteralNode:
                return "Real";
            case BooleanLiteralNode:
                return "Boolean";
            case ThisNode:
                return cls.Name;
            case GenericTypeNode gt:
                // Treat generic types as their base for now
                return gt.BaseType;
            default:
                return string.Empty;
        }
    }

    private void EnsureTypeExists(string type)
    {
        if (string.IsNullOrEmpty(type)) return;
        // Accept primitive aliases
        if (IsPrimitive(type) || IsContainerType(type)) return;
        if (!_classes.ContainsKey(type))
            Report($"Unknown type '{type}'.");
    }

    private static bool IsPrimitive(string type)
        => type is "String" or "Integer" or "Real" or "Boolean";

    private static bool IsContainerType(string type)
        => type.Contains('[') && type.EndsWith("]", StringComparison.Ordinal);

    private bool TypeCompatible(string from, string to)
    {
        if (from == to) return true;
        // Simple upcast along inheritance
        if (_classes.TryGetValue(from, out var fromCls) && _classes.TryGetValue(to, out var toCls))
        {
            var c = fromCls;
            while (c is not null)
            {
                if (ReferenceEquals(c, toCls)) return true;
                c = c.BaseClass;
            }
        }
        return false;
    }

    private void Report(string message) => _diagnostics.Add(new SemanticDiagnostic(message));

    private static MethodNode? FindMethod(ClassSymbol cls, string name, int arity)
    {
        
        for (var c = cls; c is not null; c = c.BaseClass)
        {
            if (c.Methods.TryGetValue(name, out var overloads))
            {
                var match = overloads.FirstOrDefault(m => m.Parameters.Count == arity);
                if (match is not null) return match;
            }
        }
        return null;
    }


    private static string? FindFieldType(ClassSymbol cls, string name)
    {
        for (var c = cls; c is not null; c = c.BaseClass)
        {
            if (c.Fields.TryGetValue(name, out var field))
                return field.Type;
        }
        return null;
    }
}


