using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Compiler.Lexer;

namespace Compiler.Parser;

class Test
{
    static void Main(string[] args)
    {
        string code;
        
        if (args.Length > 0 && File.Exists(args[0]))
        {
            code = File.ReadAllText(args[0]);
            Console.WriteLine($"Parsing file: {args[0]}");
        }
        else
        {
            code = @"class Point is
    var x : Integer(0)
    var y : Integer(0)
    
    this() is
        x := Integer(0)
        y := Integer(0)
    end
    
    this(px: Integer, py: Integer) is
        x := px
        y := py
    end
    
    method getX : Integer is
        return x
    end
    
    method getY : Integer is
        return y
    end
    
    method setX(newX: Integer) is
        x := newX
    end
    
    method setY(newY: Integer) is
        y := newY
    end
end

class TestPoint is
    this() is
        var p1 : Point()
        var p2 : Point(Integer(10), Integer(20))
        
        var x1 : p1.getX()
        var y1 : p1.getY()
        var x2 : p2.getX()
        var y2 : p2.getY()
        
        p1.setX(Integer(5))
        p1.setY(Integer(15))
    end
end";
        }

        var lexer = new Compiler.Lexer.Lexer(code);
        var tokens = lexer.Lex().ToList();
        var parser = new Parser(tokens);
        
        try
        {
            var ast = parser.Parse();
            Console.WriteLine($"SUCCESS: Parsed {ast.Classes.Count} classes");
            PrintAST(ast, 0);
        }
        catch (ParseException ex)
        {
            Console.WriteLine($"ERROR: {ex.Message}");
        }
    }

    static void PrintAST(Node node, int indent)
    {
        var prefix = new string(' ', indent * 2);
        Console.WriteLine($"{prefix}{node.GetType().Name}: {GetNodeInfo(node)}");
        
        foreach (var child in GetChildren(node))
            PrintAST(child, indent + 1);
    }

    static string GetNodeInfo(Node node) => node switch
    {
        ClassNode c => c.Parent != null ? $"{c.Name} extends {c.Parent.Name}" : c.Name,
        MethodNode m => $"{m.Name}({string.Join(", ", m.Parameters.Select(p => $"{p.Name}: {p.Type}"))}): {m.ReturnType}",
        VariableNode v => $"{v.Name}: {v.Type}",
        VariableStatementNode vs => $"{vs.Name}: {vs.Type}",
        ConstructorNode cn => $"Constructor({string.Join(", ", cn.Parameters.Select(p => $"{p.Name}: {p.Type}"))})",
        IdentifierNode i => i.Name,
        GenericTypeNode gt => $"{gt.BaseType}[{gt.TypeParameter}]",
        StringLiteralNode s => $"\"{s.Value}\"",
        ConstructorCallNode cc => $"{cc.ClassName}(...)",
        CallNode c => c.Target is IdentifierNode id ? $"{id.Name}(...)" : "call(...)",
        WhileNode w => "while",
        IfNode i => "if",
        _ => ""
    };

    static IEnumerable<Node> GetChildren(Node node) => node switch
    {
        ProgramNode p => p.Classes.Cast<Node>(),
        ClassNode c => c.Members.Cast<Node>(),
        MethodNode m => m.Body.Cast<Node>(),
        ConstructorNode cn => cn.Body.Cast<Node>(),
        VariableNode v => v.Initializer != null ? new[] { v.Initializer } : Array.Empty<Node>(),
        VariableStatementNode vs => vs.Initializer != null ? new[] { vs.Initializer } : Array.Empty<Node>(),
        ReturnNode r => new[] { r.Expression },
        ExpressionStatementNode e => new[] { e.Expression },
        ConstructorCallNode cc => cc.Arguments.Cast<Node>(),
        CallNode c => c.Arguments.Cast<Node>(),
        AssignmentNode a => new[] { a.Target, a.Value },
        MemberAccessNode ma => new[] { ma.Target, ma.Member },
        WhileNode w => new[] { w.Condition }.Cast<Node>().Concat(w.Body.Cast<Node>()),
        IfNode i => new[] { i.Condition }.Cast<Node>().Concat(i.ThenBody.Cast<Node>()).Concat((i.ElseBody ?? new List<StatementNode>()).Cast<Node>()),
        _ => Array.Empty<Node>()
    };
}