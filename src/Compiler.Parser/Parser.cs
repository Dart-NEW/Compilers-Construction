using System;
using System.Collections.Generic;
using System.Linq;
using Compiler.Lexer;

namespace Compiler.Parser;

public sealed class Parser
{
    private readonly List<Token> _tokens;
    private int _position;

    public Parser(IEnumerable<Token> tokens)
    {
        _tokens = tokens.ToList();
        _position = 0;
    }

    public ProgramNode Parse()
    {
        var classes = new List<ClassNode>();
        while (!IsAtEnd() && Current().Kind != TokenKind.EndOfFile)
        {
            classes.Add(ParseClass());
        }
        return new ProgramNode(classes);
    }

    private ClassNode ParseClass()
    {
        Consume(TokenKind.Class);
        var name = Consume(TokenKind.Identifier).Lexeme;
        
        ClassNode? parent = null;
        if (Match(TokenKind.Extends))
        {
            parent = new ClassNode(Consume(TokenKind.Identifier).Lexeme, null, new List<MemberNode>());
        }

        Consume(TokenKind.Is);
        var members = new List<MemberNode>();
        
        while (!Check(TokenKind.End) && !IsAtEnd())
        {
            if (Check(TokenKind.Var))
                members.Add(ParseVariable());
            else if (Check(TokenKind.Method))
                members.Add(ParseMethod());
            else if (Check(TokenKind.This))
                members.Add(ParseConstructor());
            else if (Check(TokenKind.Class))
                throw new ParseException($"Unexpected 'class' keyword - missing 'end' for class '{name}' or previous method");
            else
                throw new ParseException($"Unexpected token in class body: {Current().Kind} at position {_position} ('{Current().Lexeme}')");
        }
        
        if (IsAtEnd())
            throw new ParseException($"Expected 'end' to close class '{name}', but reached end of file");
        
        Consume(TokenKind.End);
        return new ClassNode(name, parent, members);
    }

    private VariableNode ParseVariable()
    {
        Consume(TokenKind.Var);
        var name = Consume(TokenKind.Identifier).Lexeme;
        Consume(TokenKind.Colon);
        // Support either a type annotation or an initializer expression
        string type = "";
        ExpressionNode? initializer = null;

        if (Check(TokenKind.Identifier))
        {
            // Lookahead without consuming to decide: type vs expression
            var nextNextKind = (_position + 1) < _tokens.Count ? _tokens[_position + 1].Kind : TokenKind.EndOfFile;
            bool parseAsExpression = false;
            if (nextNextKind == TokenKind.LParen || nextNextKind == TokenKind.Dot)
            {
                parseAsExpression = true;
            }
            else if (nextNextKind == TokenKind.LBracket)
            {
                // Find token after matching ']'
                int i = _position + 2;
                int depth = 1;
                while (i < _tokens.Count && depth > 0)
                {
                    if (_tokens[i].Kind == TokenKind.LBracket) depth++;
                    else if (_tokens[i].Kind == TokenKind.RBracket) depth--;
                    i++;
                }
                var afterGeneric = i < _tokens.Count ? _tokens[i].Kind : TokenKind.EndOfFile;
                if (afterGeneric == TokenKind.LParen) parseAsExpression = true;
            }

            if (parseAsExpression)
            {
                initializer = ParseExpression();
            }
            else
            {
                type = Consume(TokenKind.Identifier).Lexeme;
                if (Match(TokenKind.LBracket))
                {
                    type += "[";
                    while (!Check(TokenKind.RBracket) && !IsAtEnd())
                        type += Advance().Lexeme;
                    if (Check(TokenKind.RBracket)) type += Advance().Lexeme;
                }
            }
        }
        else
        {
            // Not an identifier, must be an expression initializer
            initializer = ParseExpression();
        }

        return new VariableNode(name, type, initializer);
    }

    private ConstructorNode ParseConstructor()
    {
        Consume(TokenKind.This);
        
        var parameters = new List<ParameterNode>();
        if (Match(TokenKind.LParen))
        {
            if (!Check(TokenKind.RParen))
            {
                do
                {
                    var paramName = Consume(TokenKind.Identifier).Lexeme;
                    Consume(TokenKind.Colon);
                    var paramType = Consume(TokenKind.Identifier).Lexeme;
                    parameters.Add(new ParameterNode(paramName, paramType));
                } while (Match(TokenKind.Comma));
            }
            Consume(TokenKind.RParen);
        }
        
        Consume(TokenKind.Is);
        var body = new List<StatementNode>();
        while (!Check(TokenKind.End) && !IsAtEnd())
        {
            body.Add(ParseStatement());
        }
        Consume(TokenKind.End);
        
        return new ConstructorNode(parameters, body);
    }

    private MethodNode ParseMethod()
    {
        Consume(TokenKind.Method);
        var name = Consume(TokenKind.Identifier).Lexeme;
        
        var parameters = new List<ParameterNode>();
        if (Match(TokenKind.LParen))
        {
            if (!Check(TokenKind.RParen))
            {
                do
                {
                    var paramName = Consume(TokenKind.Identifier).Lexeme;
                    Consume(TokenKind.Colon);
                    var paramType = Consume(TokenKind.Identifier).Lexeme;
                    
                    // Handle generic types in parameters
                    if (Match(TokenKind.LBracket))
                    {
                        paramType += "[";
                        while (!Check(TokenKind.RBracket) && !IsAtEnd())
                        {
                            paramType += Advance().Lexeme;
                        }
                        if (Check(TokenKind.RBracket))
                        {
                            paramType += Advance().Lexeme;
                        }
                    }
                    
                    parameters.Add(new ParameterNode(paramName, paramType));
                } while (Match(TokenKind.Comma));
            }
            Consume(TokenKind.RParen);
        }
        
        var returnType = "";
        if (Match(TokenKind.Colon))
        {
            returnType = Consume(TokenKind.Identifier).Lexeme;
            
            // Handle generic return types
            if (Match(TokenKind.LBracket))
            {
                returnType += "[";
                while (!Check(TokenKind.RBracket) && !IsAtEnd())
                {
                    returnType += Advance().Lexeme;
                }
                if (Check(TokenKind.RBracket))
                {
                    returnType += Advance().Lexeme;
                }
            }
        }
        
        var body = new List<StatementNode>();
        
        if (Match(TokenKind.Arrow)) // =>
        {
            var expr = ParseExpression();
            body.Add(new ReturnNode(expr));
        }
        else
        {
            Consume(TokenKind.Is);
            while (!Check(TokenKind.End) && !IsAtEnd())
            {
                if (Check(TokenKind.Class) || Check(TokenKind.Method))
                    throw new ParseException($"Missing 'end' for method '{name}' - found '{Current().Lexeme}'");
                if (Check(TokenKind.This))
                {
                    var nextKind = (_position + 1) < _tokens.Count ? _tokens[_position + 1].Kind : TokenKind.EndOfFile;
                    if (nextKind != TokenKind.Dot && nextKind != TokenKind.LParen)
                        throw new ParseException($"Missing 'end' for method '{name}' - found '{Current().Lexeme}'");
                }
                body.Add(ParseStatement());
            }
            if (IsAtEnd())
                throw new ParseException($"Expected 'end' to close method '{name}', but reached end of file");
            Consume(TokenKind.End);
        }
        return new MethodNode(name, parameters, returnType, body);
    }

    private StatementNode ParseStatement()
    {
        if (Check(TokenKind.Var))
        {
            Consume(TokenKind.Var);
            var name = Consume(TokenKind.Identifier).Lexeme;
            Consume(TokenKind.Colon);
            // Same dual-form as class fields
            string type = "";
            ExpressionNode? initializer = null;
            if (Check(TokenKind.Identifier))
            {
                var nextNextKind = (_position + 1) < _tokens.Count ? _tokens[_position + 1].Kind : TokenKind.EndOfFile;
                bool parseAsExpression = false;
                if (nextNextKind == TokenKind.LParen || nextNextKind == TokenKind.Dot)
                {
                    parseAsExpression = true;
                }
                else if (nextNextKind == TokenKind.LBracket)
                {
                    int i = _position + 2;
                    int depth = 1;
                    while (i < _tokens.Count && depth > 0)
                    {
                        if (_tokens[i].Kind == TokenKind.LBracket) depth++;
                        else if (_tokens[i].Kind == TokenKind.RBracket) depth--;
                        i++;
                    }
                    var afterGeneric = i < _tokens.Count ? _tokens[i].Kind : TokenKind.EndOfFile;
                    if (afterGeneric == TokenKind.LParen) parseAsExpression = true;
                }

                if (parseAsExpression)
                {
                    initializer = ParseExpression();
                }
                else
                {
                    type = Consume(TokenKind.Identifier).Lexeme;
                    if (Match(TokenKind.LBracket))
                    {
                        type += "[";
                        while (!Check(TokenKind.RBracket) && !IsAtEnd())
                            type += Advance().Lexeme;
                        if (Check(TokenKind.RBracket)) type += Advance().Lexeme;
                    }
                }
            }
            else
            {
                initializer = ParseExpression();
            }
            return new VariableStatementNode(name, type, initializer);
        }
        
        if (Check(TokenKind.If))
            return ParseIf();
        
        if (Check(TokenKind.While))
            return ParseWhile();
        
        if (Check(TokenKind.Return))
        {
            Advance();
            var expr = ParseExpression();
            return new ReturnNode(expr);
        }
        
        // Check for assignment
        if (Check(TokenKind.Identifier))
        {
            var pos = _position;
            var name = Advance().Lexeme;
            if (Check(TokenKind.Assign))
            {
                Advance();
                var value = ParseExpression();
                return new ExpressionStatementNode(new AssignmentNode(new IdentifierNode(name), value));
            }
            _position = pos; // backtrack
        }
        
        return new ExpressionStatementNode(ParseExpression());
    }

    private IfNode ParseIf()
    {
        Consume(TokenKind.If);
        var condition = ParseExpression();
        Consume(TokenKind.Then);
        
        var thenBody = new List<StatementNode>();
        while (!Check(TokenKind.Else) && !Check(TokenKind.End) && !IsAtEnd())
        {
            thenBody.Add(ParseStatement());
        }
        
        List<StatementNode>? elseBody = null;
        if (Match(TokenKind.Else))
        {
            elseBody = new List<StatementNode>();
            while (!Check(TokenKind.End) && !IsAtEnd())
            {
                elseBody.Add(ParseStatement());
            }
        }
        
        Consume(TokenKind.End);
        return new IfNode(condition, thenBody, elseBody);
    }

    private WhileNode ParseWhile()
    {
        Consume(TokenKind.While);
        var condition = ParseExpression();
        Consume(TokenKind.Loop);
        
        var body = new List<StatementNode>();
        while (!Check(TokenKind.End) && !IsAtEnd())
        {
            body.Add(ParseStatement());
        }
        
        Consume(TokenKind.End);
        return new WhileNode(condition, body);
    }

    private ExpressionNode ParseExpression()
    {
        return ParseAssignment();
    }

    private ExpressionNode ParseAssignment()
    {
        var expr = ParseCall();
        
        if (Check(TokenKind.Assign))
        {
            Advance();
            var value = ParseAssignment();
            return new AssignmentNode(expr, value);
        }
        
        return expr;
    }

    private ExpressionNode ParseCall()
    {
        var expr = ParsePrimary();
        
        while (true)
        {
            if (Check(TokenKind.LParen))
            {
                Advance();
                var args = new List<ExpressionNode>();
                if (!Check(TokenKind.RParen))
                {
                    do
                    {
                        args.Add(ParseExpression());
                    } while (Match(TokenKind.Comma));
                }
                Consume(TokenKind.RParen);
                
                // If expr is an identifier or generic type, treat as constructor call
                if (expr is IdentifierNode id)
                    expr = new ConstructorCallNode(id.Name, args);
                else if (expr is GenericTypeNode gt)
                    expr = new ConstructorCallNode($"{gt.BaseType}[{gt.TypeParameter}]", args);
                else
                    expr = new CallNode(expr, args);
            }
            else if (Check(TokenKind.Dot))
            {
                Advance();
                var memberName = Consume(TokenKind.Identifier).Lexeme;
                
                // Check if it's a method call
                if (Check(TokenKind.LParen))
                {
                    Advance();
                    var args = new List<ExpressionNode>();
                    if (!Check(TokenKind.RParen))
                    {
                        do
                        {
                            args.Add(ParseExpression());
                        } while (Match(TokenKind.Comma));
                    }
                    Consume(TokenKind.RParen);
                    expr = new MemberAccessNode(expr, new CallNode(new IdentifierNode(memberName), args));
                }
                else
                {
                    expr = new MemberAccessNode(expr, new IdentifierNode(memberName));
                }
            }
            else if (Check(TokenKind.LBracket))
            {
                Advance();
                var index = ParseExpression();
                Consume(TokenKind.RBracket);
                expr = new IndexAccessNode(expr, index);
            }
            else
            {
                break;
            }
        }
        
        return expr;
    }

    private ExpressionNode ParsePrimary()
    {
        if (Match(TokenKind.This))
            return new ThisNode();
        
        if (Check(TokenKind.IntegerLiteral))
        {
            var value = Advance().Lexeme;
            return new IntegerLiteralNode(value);
        }
        
        if (Check(TokenKind.RealLiteral))
        {
            var value = Advance().Lexeme;
            return new RealLiteralNode(value);
        }
        
        if (Check(TokenKind.Identifier))
        {
            var name = Advance().Lexeme;
            
            // Handle negative numeric literals like -1 or -3.14
            if (name == "-" && (Check(TokenKind.IntegerLiteral) || Check(TokenKind.RealLiteral)))
            {
                if (Check(TokenKind.IntegerLiteral))
                {
                    var number = Advance().Lexeme;
                    return new IntegerLiteralNode("-" + number);
                }
                else
                {
                    var number = Advance().Lexeme;
                    return new RealLiteralNode("-" + number);
                }
            }
            
            // Handle generic types like Array[Integer]
            if (Match(TokenKind.LBracket))
            {
                var typeParam = Consume(TokenKind.Identifier).Lexeme;
                Consume(TokenKind.RBracket);
                return new GenericTypeNode(name, typeParam);
            }
            
            return new IdentifierNode(name);
        }
        
        if (Check(TokenKind.StringLiteral))
        {
            var value = Advance().Lexeme;
            return new StringLiteralNode(value);
        }
        
        if (Check(TokenKind.BooleanLiteral))
        {
            var value = Advance().Lexeme;
            return new BooleanLiteralNode(value);
        }
        
        if (Match(TokenKind.LParen))
        {
            var expr = ParseExpression();
            Consume(TokenKind.RParen);
            return expr;
        }
        
        throw new ParseException($"Unexpected token: {Current().Kind}");
    }


    private bool Match(TokenKind kind)
    {
        if (Check(kind))
        {
            Advance();
            return true;
        }
        return false;
    }

    private bool Check(TokenKind kind) => !IsAtEnd() && Current().Kind == kind;
    private Token Advance() => IsAtEnd() ? Current() : _tokens[_position++];
    private bool IsAtEnd() => _position >= _tokens.Count || Current().Kind == TokenKind.EndOfFile;
    private Token Current() => _tokens[_position];
    
    private Token Consume(TokenKind kind)
    {
        if (Check(kind)) return Advance();
        throw new ParseException($"Expected {kind}, got {Current().Kind} at position {_position} ('{Current().Lexeme}')");
    }
}

public class ParseException : Exception
{
    public ParseException(string message) : base(message) { }
}

public abstract record Node;
public abstract record StatementNode : Node;
public abstract record ExpressionNode : Node;
public abstract record MemberNode : Node;

public record ProgramNode(List<ClassNode> Classes) : Node;
public record ClassNode(string Name, ClassNode? Parent, List<MemberNode> Members) : Node;
public record VariableNode(string Name, string Type, ExpressionNode? Initializer) : MemberNode;
public record VariableStatementNode(string Name, string Type, ExpressionNode? Initializer) : StatementNode;
public record ParameterNode(string Name, string Type) : Node;
public record MethodNode(string Name, List<ParameterNode> Parameters, string ReturnType, List<StatementNode> Body) : MemberNode;
public record ConstructorNode(List<ParameterNode> Parameters, List<StatementNode> Body) : MemberNode;
public record IfNode(ExpressionNode Condition, List<StatementNode> ThenBody, List<StatementNode>? ElseBody) : StatementNode;
public record WhileNode(ExpressionNode Condition, List<StatementNode> Body) : StatementNode;
public record ReturnNode(ExpressionNode Expression) : StatementNode;
public record ExpressionStatementNode(ExpressionNode Expression) : StatementNode;
public record AssignmentNode(ExpressionNode Target, ExpressionNode Value) : ExpressionNode;
public record CallNode(ExpressionNode Target, List<ExpressionNode> Arguments) : ExpressionNode;
public record MemberAccessNode(ExpressionNode Target, ExpressionNode Member) : ExpressionNode;
public record ConstructorCallNode(string ClassName, List<ExpressionNode> Arguments) : ExpressionNode;
public record IndexAccessNode(ExpressionNode Target, ExpressionNode Index) : ExpressionNode;
public record IdentifierNode(string Name) : ExpressionNode;
public record GenericTypeNode(string BaseType, string TypeParameter) : ExpressionNode;
public record StringLiteralNode(string Value) : ExpressionNode;
public record IntegerLiteralNode(string Value) : ExpressionNode;
public record RealLiteralNode(string Value) : ExpressionNode;
public record BooleanLiteralNode(string Value) : ExpressionNode;
public record ThisNode : ExpressionNode;