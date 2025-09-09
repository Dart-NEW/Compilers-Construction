namespace Compiler.Lexer;

public enum TokenKind
{
    EndOfFile,

    // Identifiers and literals
    Identifier,
    StringLiteral,

    // Keywords
    Class,
    Is,
    Var,
    Method,
    Return,
    End,
    This,
    Extends,
    While,
    Loop,
    If,
    Then,
    Else,

    // Operators and punctuation
    Assign,      // :=
    Colon,       // :
    Comma,       // ,
    Dot,         // .
    LParen,      // (
    RParen,      // )
    LBracket,    // [
    RBracket     // ]
}


