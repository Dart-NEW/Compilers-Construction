using System.Linq;
using Compiler.Lexer;
using Compiler.Parser;
using Xunit;

namespace Compiler.Tests;

public sealed class OptimizerTests
{
    private static ProgramNode Parse(string src)
    {
        var tokens = new Compiler.Lexer.Lexer(src).Lex().ToList();
        return new Compiler.Parser.Parser(tokens).Parse();
    }

    [Fact]
    public void IfTrue_Removes_Else_And_Keeps_Then()
    {
        var src = @"class A is
    method m is
        if true then
            var x : Integer(1)
        else
            var y : Integer(2)
        end
    end
end";
        var opt = new AstOptimizer();
        var optimized = opt.Optimize(Parse(src));
        var method = ((ClassNode)optimized.Classes[0]).Members.OfType<MethodNode>().First();
        // Expect only one statement (var x) in body
        Assert.Single(method.Body);
        Assert.IsType<VariableStatementNode>(method.Body[0]);
    }

    [Fact]
    public void IfFalse_Removes_Then_And_Keeps_Else()
    {
        var src = @"class A is
    method m is
        if false then
            var x : Integer(1)
        else
            var y : Integer(2)
        end
    end
end";
        var opt = new AstOptimizer();
        var optimized = opt.Optimize(Parse(src));
        var method = ((ClassNode)optimized.Classes[0]).Members.OfType<MethodNode>().First();
        Assert.Single(method.Body);
        var vs = Assert.IsType<VariableStatementNode>(method.Body[0]);
        Assert.Equal("y", vs.Name);
    }

    [Fact]
    public void WhileFalse_Is_Removed()
    {
        var src = @"class A is
    method m is
        while false loop
            var x : Integer(1)
        end
    end
end";
        var opt = new AstOptimizer();
        var optimized = opt.Optimize(Parse(src));
        var method = ((ClassNode)optimized.Classes[0]).Members.OfType<MethodNode>().First();
        Assert.Empty(method.Body);
    }
}


