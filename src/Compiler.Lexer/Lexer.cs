using System.Diagnostics;
using System.Text;

namespace Compiler.Lexer;

public sealed class Lexer
{
    private readonly string _text;
    private int _position;

    private static readonly Dictionary<string, TokenKind> s_keywords = new(StringComparer.Ordinal)
    {
        ["class"] = TokenKind.Class,
        ["is"] = TokenKind.Is,
        ["var"] = TokenKind.Var,
        ["method"] = TokenKind.Method,
        ["return"] = TokenKind.Return,
        ["end"] = TokenKind.End,
        ["this"] = TokenKind.This,
        ["extends"] = TokenKind.Extends,
        ["while"] = TokenKind.While,
        ["loop"] = TokenKind.Loop,
        ["if"] = TokenKind.If,
        ["then"] = TokenKind.Then,
        ["else"] = TokenKind.Else,
    };

    public Lexer(string text)
    {
        _text = text ?? string.Empty;
        _position = 0;
    }

    public IEnumerable<Token> Lex()
    {
        while (true)
        {
            SkipWhitespaceAndComments();
            if (IsAtEnd())
            {
                yield return Make(TokenKind.EndOfFile, string.Empty, 0);
                yield break;
            }

            char ch = Peek();

            if (char.IsDigit(ch))
            {
                yield return LexNumber();
                continue;
            }

            if (IsIdentifierStart(ch))
            {
                yield return LexIdentifierOrKeyword();
                continue;
            }

            if (ch == '"')
            {
                yield return LexString();
                continue;
            }

            switch (ch)
            {
                case ':':
                    if (MatchNext('='))
                        yield return AdvanceMake(TokenKind.Assign, ":=");
                    else
                        yield return AdvanceMake(TokenKind.Colon, ":");
                    break;
                case ',':
                    yield return AdvanceMake(TokenKind.Comma, ",");
                    break;
                case '.':
                    yield return AdvanceMake(TokenKind.Dot, ".");
                    break;
                case '(':
                    yield return AdvanceMake(TokenKind.LParen, "(");
                    break;
                case ')':
                    yield return AdvanceMake(TokenKind.RParen, ")");
                    break;
                case '[':
                    yield return AdvanceMake(TokenKind.LBracket, "[");
                    break;
                case ']':
                    yield return AdvanceMake(TokenKind.RBracket, "]");
                    break;
                default:
                    // Unknown character, consume one and emit as Identifier for now
                    yield return AdvanceMake(TokenKind.Identifier, ch.ToString());
                    break;
            }
        }
    }

    private void SkipWhitespaceAndComments()
    {
        while (!IsAtEnd())
        {
            char c = Peek();
            if (char.IsWhiteSpace(c))
            {
                _position++;
                continue;
            }

            // Line comment starts with //
            if (c == '/' && PeekNext() == '/')
            {
                _position += 2;
                while (!IsAtEnd() && Peek() != '\n') _position++;
                continue;
            }

            break;
        }
    }

    private Token LexIdentifierOrKeyword()
    {
        int start = _position;
        var sb = new StringBuilder();

        sb.Append(Peek());
        _position++;
        while (!IsAtEnd() && IsIdentifierPart(Peek()))
        {
            sb.Append(Peek());
            _position++;
        }

        string text = sb.ToString();
        if (s_keywords.TryGetValue(text, out var kind))
            return new Token(kind, text, new SourceSpan(start, _position - start));

        return new Token(TokenKind.Identifier, text, new SourceSpan(start, _position - start));
    }

    private Token LexNumber()
    {
        int start = _position;
        var sb = new StringBuilder();
        
        while (!IsAtEnd() && char.IsDigit(Peek()))
        {
            sb.Append(Peek());
            _position++;
        }
        
        // Handle decimal point
        if (!IsAtEnd() && Peek() == '.' && _position + 1 < _text.Length && char.IsDigit(_text[_position + 1]))
        {
            sb.Append(Peek());
            _position++;
            while (!IsAtEnd() && char.IsDigit(Peek()))
            {
                sb.Append(Peek());
                _position++;
            }
        }
        
        string text = sb.ToString();
        return new Token(TokenKind.Identifier, text, new SourceSpan(start, _position - start));
    }

    private Token LexString()
    {
        Debug.Assert(Peek() == '"');
        int start = _position;
        _position++; // opening quote
        var sb = new StringBuilder();
        while (!IsAtEnd() && Peek() != '"')
        {
            char c = Peek();
            if (c == '\\')
            {
                _position++;
                if (IsAtEnd()) break;
                char esc = Peek();
                _position++;
                sb.Append(esc switch
                {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '"' => '"',
                    '\\' => '\\',
                    _ => esc
                });
                continue;
            }
            sb.Append(c);
            _position++;
        }
        if (!IsAtEnd() && Peek() == '"') _position++; // closing quote
        string lexeme = sb.ToString();
        return new Token(TokenKind.StringLiteral, lexeme, new SourceSpan(start, _position - start));
    }

    private bool IsIdentifierStart(char c) => char.IsLetter(c) || c == '_';
    private bool IsIdentifierPart(char c) => char.IsLetterOrDigit(c) || c == '_';

    private bool IsAtEnd() => _position >= _text.Length;
    private char Peek() => _text[_position];
    private char PeekNext() => _position + 1 < _text.Length ? _text[_position + 1] : '\0';
    private bool MatchNext(char expected)
    {
        if (_position + 1 >= _text.Length) return false;
        return _text[_position + 1] == expected ? (_position += 2) > 0 : false;
    }
    private Token AdvanceMake(TokenKind kind, string lexeme)
    {
        int start = _position;
        _position++;
        return new Token(kind, lexeme, new SourceSpan(start, 1));
    }
    private Token Make(TokenKind kind, string lexeme, int length)
        => new(kind, lexeme, new SourceSpan(_position, length));
}


