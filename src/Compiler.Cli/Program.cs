using Compiler.Lexer;
using Compiler.Parser;

static class Program
{
    static int Main(string[] args)
    {
        if (args.Length == 0)
        {
            Console.Error.WriteLine("usage: compiler-cli [--lex|--parse|--analyze|--optimize|--compile] <args>");
            return 1;
        }

        string mode;
        string path;
        if (args.Length == 1)
        {
            mode = "--lex";
            path = args[0];
        }
        else
        {
            mode = args[0];
            path = args[1];
        }

        var text = File.ReadAllText(path);
        var lexer = new Lexer(text);
        var tokens = lexer.Lex().ToList();

        switch (mode)
        {
            case "--lex":
                foreach (var token in tokens)
                {
                    Console.WriteLine(token);
                    if (token.Kind == TokenKind.EndOfFile) break;
                }
                return 0;

			case "--parse":
			{
				try
				{
					var parser = new Parser(tokens);
					var program = parser.Parse();
					PrintProgram(program);
					return 0;
				}
				catch (Exception ex)
				{
					Console.Error.WriteLine($"Parse error: {ex.Message}");
					return 2;
				}
			}

            case "--analyze":
            {
                try
                {
                    var parser = new Parser(tokens);
                    var program = parser.Parse();
                    var analyzer = new SemanticAnalyzer();
                    analyzer.Analyze(program);
                    if (analyzer.Diagnostics.Count == 0)
                    {
                        Console.WriteLine("Semantic analysis passed with no issues.");
                        return 0;
                    }
                    foreach (var d in analyzer.Diagnostics)
                        Console.WriteLine($"error: {d}");
                    return 3;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine($"Error: {ex.Message}");
                    return 2;
                }
            }

            case "--optimize":
            {
                try
                {
                    var parser = new Parser(tokens);
                    var program = parser.Parse();
                    var optimizer = new AstOptimizer();
                    var optimized = optimizer.Optimize(program);
                    PrintProgram(optimized);
                    return 0;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine($"Error: {ex.Message}");
                    return 2;
                }
            }

            case "--compile":
            {
                if (args.Length < 3)
                {
                    Console.Error.WriteLine("usage: compiler-cli --compile <source-file> <output-exe> [ilasm-path]");
                    return 1;
                }

                var outputPath = args[2];
                var ilasmPath = args.Length > 3 ? args[3] : null;

                try
                {
                    var parser = new Parser(tokens);
                    var program = parser.Parse();

                    var analyzer = new SemanticAnalyzer();
                    analyzer.Analyze(program);
                    if (analyzer.Diagnostics.Count > 0)
                    {
                        foreach (var diagnostic in analyzer.Diagnostics)
                            Console.Error.WriteLine($"error: {diagnostic.Message}");
                        return 3;
                    }

                    var optimizer = new AstOptimizer();
                    var optimized = optimizer.Optimize(program);

                    var compiler = new IlCompiler();
                    var options = new IlCompilationOptions(
                        outputPath: outputPath,
                        ilasmPath: ilasmPath);
                    compiler.Compile(optimized, options);
                    Console.WriteLine($"Wrote executable to {options.OutputPath}");
                    return 0;
                }
                catch (Exception ex)
                {
                    Console.Error.WriteLine($"Compilation failed: {ex.Message}");
                    return 4;
                }
            }

	static void PrintProgram(ProgramNode program)
	{
		Console.WriteLine("Program");
		foreach (var cls in program.Classes)
		{
			PrintClass(cls, 1);
		}
	}

	static void PrintClass(ClassNode cls, int indent)
	{
		WriteIndented(indent, $"Class {cls.Name}{(cls.Parent is null ? string.Empty : $" extends {cls.Parent.Name}")}");
		foreach (var member in cls.Members)
		{
			switch (member)
			{
				case VariableNode v:
					WriteIndented(indent + 1, $"Field {v.Name}{(string.IsNullOrEmpty(v.Type) ? string.Empty : $": {v.Type}")}");
					if (v.Initializer is not null)
					{
						WriteIndented(indent + 2, "Initializer:");
						PrintExpression(v.Initializer, indent + 3);
					}
					break;
				case MethodNode m:
					WriteIndented(indent + 1, $"Method {m.Name}({string.Join(", ", m.Parameters.Select(p => $"{p.Name}: {p.Type}"))}){(string.IsNullOrEmpty(m.ReturnType) ? string.Empty : $": {m.ReturnType}")}");
					foreach (var s in m.Body)
						PrintStatement(s, indent + 2);
					break;
				case ConstructorNode c:
					WriteIndented(indent + 1, $"Constructor({string.Join(", ", c.Parameters.Select(p => $"{p.Name}: {p.Type}"))})");
					foreach (var s in c.Body)
						PrintStatement(s, indent + 2);
					break;
			}
		}
	}

	static void PrintStatement(StatementNode stmt, int indent)
	{
		switch (stmt)
		{
			case VariableStatementNode v:
				WriteIndented(indent, $"Var {v.Name}{(string.IsNullOrEmpty(v.Type) ? string.Empty : $": {v.Type}")}");
				if (v.Initializer is not null)
				{
					WriteIndented(indent + 1, "Initializer:");
					PrintExpression(v.Initializer, indent + 2);
				}
				break;
			case IfNode i:
				WriteIndented(indent, "If");
				WriteIndented(indent + 1, "Condition:");
				PrintExpression(i.Condition, indent + 2);
				WriteIndented(indent + 1, "Then:");
				foreach (var s in i.ThenBody) PrintStatement(s, indent + 2);
				if (i.ElseBody is not null)
				{
					WriteIndented(indent + 1, "Else:");
					foreach (var s in i.ElseBody) PrintStatement(s, indent + 2);
				}
				break;
			case WhileNode w:
				WriteIndented(indent, "While");
				WriteIndented(indent + 1, "Condition:");
				PrintExpression(w.Condition, indent + 2);
				WriteIndented(indent + 1, "Body:");
				foreach (var s in w.Body) PrintStatement(s, indent + 2);
				break;
			case ReturnNode r:
				WriteIndented(indent, "Return");
				PrintExpression(r.Expression, indent + 1);
				break;
			case ExpressionStatementNode e:
				WriteIndented(indent, "Expr");
				PrintExpression(e.Expression, indent + 1);
				break;
		}
	}

	static void PrintExpression(ExpressionNode expr, int indent)
	{
		switch (expr)
		{
			case AssignmentNode a:
				WriteIndented(indent, "Assign");
				WriteIndented(indent + 1, "Target:");
				PrintExpression(a.Target, indent + 2);
				WriteIndented(indent + 1, "Value:");
				PrintExpression(a.Value, indent + 2);
				break;
			case CallNode c:
				WriteIndented(indent, "Call");
				WriteIndented(indent + 1, "Target:");
				PrintExpression(c.Target, indent + 2);
				WriteIndented(indent + 1, "Args:");
				for (int i = 0; i < c.Arguments.Count; i++)
				{
					PrintExpression(c.Arguments[i], indent + 2);
				}
				break;
			case MemberAccessNode m:
				WriteIndented(indent, "MemberAccess");
				WriteIndented(indent + 1, "Target:");
				PrintExpression(m.Target, indent + 2);
				WriteIndented(indent + 1, "Member:");
				PrintExpression(m.Member, indent + 2);
				break;
			case ConstructorCallNode cc:
				WriteIndented(indent, $"Construct {cc.ClassName}");
				WriteIndented(indent + 1, "Args:");
				for (int i = 0; i < cc.Arguments.Count; i++)
				{
					PrintExpression(cc.Arguments[i], indent + 2);
				}
				break;
			case IndexAccessNode ia:
				WriteIndented(indent, "IndexAccess");
				WriteIndented(indent + 1, "Target:");
				PrintExpression(ia.Target, indent + 2);
				WriteIndented(indent + 1, "Index:");
				PrintExpression(ia.Index, indent + 2);
				break;
			case IdentifierNode id:
				WriteIndented(indent, $"Id {id.Name}");
				break;
			case GenericTypeNode gt:
				WriteIndented(indent, $"GenericType {gt.BaseType}[{gt.TypeParameter}]");
				break;
			case StringLiteralNode s:
				WriteIndented(indent, $"String \"{s.Value}\"");
				break;
			case IntegerLiteralNode i:
				WriteIndented(indent, $"Integer {i.Value}");
				break;
			case RealLiteralNode r:
				WriteIndented(indent, $"Real {r.Value}");
				break;
			case BooleanLiteralNode b:
				WriteIndented(indent, $"Boolean {b.Value}");
				break;
			case ThisNode:
				WriteIndented(indent, "This");
				break;
		}
	}

	static void WriteIndented(int indent, string text)
	{
		Console.WriteLine(new string(' ', indent * 2) + text);
	}

            default:
                Console.Error.WriteLine("Unknown mode. Use --lex, --parse, --analyze, --optimize, or --compile.");
                return 1;
        }
    }
}


