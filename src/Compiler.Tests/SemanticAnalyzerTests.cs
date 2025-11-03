using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Compiler.Lexer;
using Compiler.Parser;
using Xunit;

namespace Compiler.Tests;

public sealed class SemanticAnalyzerTests
{
    private static ProgramNode ParseText(string text)
    {
        var tokens = new Compiler.Lexer.Lexer(text).Lex().ToList();
        return new Compiler.Parser.Parser(tokens).Parse();
    }

    [Fact]
    public void Positive_Files_Should_Have_No_Diagnostics()
    {
        var dir = "/home/dart/Programming/Compilers-Construction/tests/positive";
        foreach (var file in Directory.EnumerateFiles(dir, "*.o"))
        {
            var text = File.ReadAllText(file);
            var program = ParseText(text);
            var analyzer = new SemanticAnalyzer();
            analyzer.Analyze(program);
            Assert.True(analyzer.Diagnostics.Count == 0, $"{Path.GetFileName(file)} has diagnostics: {string.Join("; ", analyzer.Diagnostics.Select(d => d.Message))}");
        }
    }

    [Fact]
    public void Negative_Files_Should_Produce_Diagnostics_Or_Fail_Parse()
    {
        var dir = "/home/dart/Programming/Compilers-Construction/tests/negative";
        foreach (var file in Directory.EnumerateFiles(dir, "*.o"))
        {
            var text = File.ReadAllText(file);
            try
            {
                var program = ParseText(text);
                var analyzer = new SemanticAnalyzer();
                analyzer.Analyze(program);
                Assert.True(analyzer.Diagnostics.Count > 0, $"{Path.GetFileName(file)} unexpectedly has no diagnostics");
            }
            catch (ParseException)
            {
                // Syntax errors are acceptable for negative tests
            }
        }
    }
}


