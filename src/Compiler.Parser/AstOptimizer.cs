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
            {
                result.Add(os);
                if (os is ReturnNode)
                {
                    // Remove code after return within the same block
                    return RemoveUnusedVariables(result);
                }
            }
        }
        return RemoveUnusedVariables(result);
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

    private List<StatementNode> RemoveUnusedVariables(List<StatementNode> statements)
    {
        var used = new HashSet<string>(StringComparer.Ordinal);
        var kept = new List<StatementNode>(statements.Count);

        // Backward pass for simple liveness on variable declarations
        for (int i = statements.Count - 1; i >= 0; i--)
        {
            var s = statements[i];
            switch (s)
            {
                case VariableStatementNode v:
                {
                    // If variable is never used later and has no initializer, drop it
                    if (!used.Contains(v.Name) && v.Initializer is null)
                    {
                        // skip
                        continue;
                    }
                    // Keep declaration; collect uses from initializer (if any)
                    if (v.Initializer is not null)
                        CollectReadIdentifiers(v.Initializer, used);
                    kept.Add(v);
                    // Definition could kill previous uses; no need to modify 'used' set here
                    break;
                }
                case IfNode iNode:
                {
                    // Collect uses from condition and bodies
                    CollectReadIdentifiers(iNode.Condition, used);
                    foreach (var st in iNode.ThenBody) CollectReadsFromStatement(st, used);
                    if (iNode.ElseBody is not null)
                        foreach (var st in iNode.ElseBody) CollectReadsFromStatement(st, used);
                    kept.Add(iNode);
                    break;
                }
                case WhileNode wNode:
                {
                    CollectReadIdentifiers(wNode.Condition, used);
                    foreach (var st in wNode.Body) CollectReadsFromStatement(st, used);
                    kept.Add(wNode);
                    break;
                }
                case ReturnNode r:
                {
                    CollectReadIdentifiers(r.Expression, used);
                    kept.Add(r);
                    break;
                }
                case ExpressionStatementNode e:
                {
                    CollectReadIdentifiers(e.Expression, used);
                    kept.Add(e);
                    break;
                }
                default:
                    kept.Add(s);
                    break;
            }
        }

        kept.Reverse();
        return kept;
    }

    private static void CollectReadsFromStatement(StatementNode stmt, HashSet<string> used)
    {
        switch (stmt)
        {
            case VariableStatementNode v:
                if (v.Initializer is not null) CollectReadIdentifiers(v.Initializer, used);
                break;
            case IfNode i:
                CollectReadIdentifiers(i.Condition, used);
                foreach (var st in i.ThenBody) CollectReadsFromStatement(st, used);
                if (i.ElseBody is not null)
                    foreach (var st in i.ElseBody) CollectReadsFromStatement(st, used);
                break;
            case WhileNode w:
                CollectReadIdentifiers(w.Condition, used);
                foreach (var st in w.Body) CollectReadsFromStatement(st, used);
                break;
            case ReturnNode r:
                CollectReadIdentifiers(r.Expression, used);
                break;
            case ExpressionStatementNode e:
                CollectReadIdentifiers(e.Expression, used);
                break;
        }
    }

    private static void CollectReadIdentifiers(ExpressionNode expr, HashSet<string> used, bool isAssignTarget = false)
    {
        switch (expr)
        {
            case IdentifierNode id:
                if (!isAssignTarget)
                    used.Add(id.Name);
                break;
            case AssignmentNode a:
                CollectReadIdentifiers(a.Target, used, isAssignTarget: true);
                CollectReadIdentifiers(a.Value, used, isAssignTarget: false);
                break;
            case CallNode c:
                CollectReadIdentifiers(c.Target, used);
                foreach (var a in c.Arguments) CollectReadIdentifiers(a, used);
                break;
            case MemberAccessNode m:
                CollectReadIdentifiers(m.Target, used);
                if (m.Member is CallNode mc)
                {
                    CollectReadIdentifiers(mc, used);
                }
                // If Member is IdentifierNode, it's a member name, not a variable read
                break;
            case IndexAccessNode ia:
                CollectReadIdentifiers(ia.Target, used);
                CollectReadIdentifiers(ia.Index, used);
                break;
            case ConstructorCallNode cc:
                foreach (var a in cc.Arguments) CollectReadIdentifiers(a, used);
                break;
            // Literals and ThisNode have no variable reads
        }
    }
}


