using System.Collections.Generic;

namespace Compiler.Parser.Structures;

public record MethodDeclaration(string Name, List<ParameterNode> Parameters, string ReturnType, List<StatementNode> Body) : MemberNode;