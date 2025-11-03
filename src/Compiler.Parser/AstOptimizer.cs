using System;
using System.Collections.Generic;
using System.Linq;

namespace Compiler.Parser;

public sealed class AstOptimizer
{
    public ProgramNode Optimize(ProgramNode program)
    {
        var newClasses = new List<ClassNode>(program.Classes.Count);
        foreach (var cls in program.Classes)
        {
            newClasses.Add(OptimizeClass(cls));
        }
        return new ProgramNode(newClasses);
    }

    private ClassNode OptimizeClass(ClassNode cls)
    {
        var newMembers = new List<MemberNode>(cls.Members.Count);
        foreach (var m in cls.Members)
        {
            switch (m)
            {
                case VariableNode v:
                    newMembers.Add(new VariableNode(v.Name, v.Type, v.Initializer is null ? null : OptimizeExpression(v.Initializer)));
                    break;
                case MethodNode meth:
                {
                    var newBody = OptimizeStatements(meth.Body);
                    newMembers.Add(new MethodNode(meth.Name, meth.Parameters, meth.ReturnType, newBody));
                    break;
                }
                case ConstructorNode ctor:
                {
                    var newBody = OptimizeStatements(ctor.Body);
                    newMembers.Add(new ConstructorNode(ctor.Parameters, newBody));
                    break;
                }
                default:
                    newMembers.Add(m);
                    break;
            }
        }
        return new ClassNode(cls.Name, cls.Parent, newMembers);
    }

    private List<StatementNode> OptimizeStatements(List<StatementNode> statements)
    {
        var result = new List<StatementNode>(statements.Count);
        foreach (var s in statements)
        {
            foreach (var os in OptimizeStatement(s))
                result.Add(os);
        }
        return result;
    }

    private IEnumerable<StatementNode> OptimizeStatement(StatementNode stmt)
    {
        switch (stmt)
        {
            case VariableStatementNode v:
                yield return new VariableStatementNode(v.Name, v.Type, v.Initializer is null ? null : OptimizeExpression(v.Initializer));
                yield break;
            case IfNode i:
            {
                var cond = OptimizeExpression(i.Condition);
                var thenOpt = OptimizeStatements(i.ThenBody);
                var elseOpt = i.ElseBody is null ? null : OptimizeStatements(i.ElseBody);

                if (cond is BooleanLiteralNode b)
                {
                    if (string.Equals(b.Value, "true", StringComparison.Ordinal))
                    {
                        foreach (var s in thenOpt) yield return s;
                        yield break;
                    }
                    if (string.Equals(b.Value, "false", StringComparison.Ordinal))
                    {
                        if (elseOpt is not null)
                            foreach (var s in elseOpt) yield return s;
                        yield break;
                    }
                }
                yield return new IfNode(cond, thenOpt, elseOpt);
                yield break;
            }
            case WhileNode w:
            {
                var cond = OptimizeExpression(w.Condition);
                var body = OptimizeStatements(w.Body);
                if (cond is BooleanLiteralNode b && string.Equals(b.Value, "false", StringComparison.Ordinal))
                {
                    // Remove while(false)
                    yield break;
                }
                yield return new WhileNode(cond, body);
                yield break;
            }
            case ReturnNode r:
                yield return new ReturnNode(OptimizeExpression(r.Expression));
                yield break;
            case ExpressionStatementNode e:
                yield return new ExpressionStatementNode(OptimizeExpression(e.Expression));
                yield break;
            default:
                yield return stmt;
                yield break;
        }
    }

    private ExpressionNode OptimizeExpression(ExpressionNode expr)
    {
        switch (expr)
        {
            case AssignmentNode a:
                return new AssignmentNode(OptimizeExpression(a.Target), OptimizeExpression(a.Value));
            case CallNode c:
                return new CallNode(OptimizeExpression(c.Target), c.Arguments.Select(OptimizeExpression).ToList());
            case MemberAccessNode m:
                return new MemberAccessNode(OptimizeExpression(m.Target), OptimizeExpression(m.Member));
            case ConstructorCallNode cc:
                return new ConstructorCallNode(cc.ClassName, cc.Arguments.Select(OptimizeExpression).ToList());
            case IndexAccessNode ia:
                return new IndexAccessNode(OptimizeExpression(ia.Target), OptimizeExpression(ia.Index));
            default:
                return expr;
        }
    }
}


