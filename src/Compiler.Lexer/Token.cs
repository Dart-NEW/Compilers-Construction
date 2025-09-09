namespace Compiler.Lexer;

public readonly record struct SourceSpan(int Start, int Length)
{
    public int End => Start + Length;
}

public readonly record struct Token(TokenKind Kind, string Lexeme, SourceSpan Span)
{
    public override string ToString()
        => $"{Kind} '{Lexeme}' @{Span.Start}+{Span.Length}";
}


