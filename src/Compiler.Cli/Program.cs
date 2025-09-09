using Compiler.Lexer;

static class Program
{
    static int Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.Error.WriteLine("usage: compiler-cli <source-file>");
            return 1;
        }

        var path = args[0];
        var text = File.ReadAllText(path);
        var lexer = new Lexer(text);
        foreach (var token in lexer.Lex())
        {
            Console.WriteLine(token);
            if (token.Kind == TokenKind.EndOfFile) break;
        }
        return 0;
    }
}


