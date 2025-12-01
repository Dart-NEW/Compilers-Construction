using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;

namespace Compiler.Parser;

/// <summary>
/// Options that drive how IL code is emitted and assembled via <c>ilasm</c>.
/// </summary>
public sealed class IlCompilationOptions
{
    public string AssemblyName { get; }
    public string Namespace { get; }
    public string OutputPath { get; }
    public string? EntryPointClass { get; }
    public string? EntryPointMethod { get; }
    public string? IlasmPath { get; }

    public IlCompilationOptions(
        string outputPath,
        string? assemblyName = null,
        string? @namespace = null,
        string? entryPointClass = null,
        string? entryPointMethod = null,
        string? ilasmPath = null)
    {
        if (string.IsNullOrWhiteSpace(outputPath))
            throw new ArgumentException("An output path is required.", nameof(outputPath));

        OutputPath = Path.GetFullPath(outputPath);
        AssemblyName = string.IsNullOrWhiteSpace(assemblyName)
            ? Path.GetFileNameWithoutExtension(OutputPath)
            : assemblyName;
        Namespace = string.IsNullOrWhiteSpace(@namespace) ? "UserProgram" : @namespace!;
        EntryPointClass = entryPointClass;
        EntryPointMethod = entryPointMethod;
        IlasmPath = ilasmPath;
    }
}

/// <summary>
/// Consumes the AST and produces an executable by emitting IL and invoking ilasm.
/// </summary>
public sealed class IlCompiler
{
    public string Compile(ProgramNode program, IlCompilationOptions options)
    {
        if (program is null) throw new ArgumentNullException(nameof(program));
        if (options is null) throw new ArgumentNullException(nameof(options));

        Directory.CreateDirectory(Path.GetDirectoryName(options.OutputPath)!);

        var generator = new IlGenerator(options);
        var ilText = generator.Generate(program);

        var ilPath = Path.ChangeExtension(options.OutputPath, ".il");
        var utf8NoBom = new UTF8Encoding(encoderShouldEmitUTF8Identifier: false);
        File.WriteAllText(ilPath, ilText, utf8NoBom);
        Console.WriteLine($"IL file saved at: {ilPath}");

        RunIlasm(ilPath, options);

        return options.OutputPath;
    }

    private static void RunIlasm(string ilPath, IlCompilationOptions options)
    {
        var ilasm = ResolveIlasm(options.IlasmPath);
        var arguments = new[]
        {
            $"-output=\"{options.OutputPath}\"",
            "-exe",
            ilPath
        };

        var psi = new ProcessStartInfo(ilasm, string.Join(' ', arguments))
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false
        };

        using var process = Process.Start(psi)
            ?? throw new InvalidOperationException("Failed to start ilasm process.");

        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit();

        if (process.ExitCode != 0)
        {
            throw new InvalidOperationException(
                $"ilasm failed with exit code {process.ExitCode}.{Environment.NewLine}{stdout}{Environment.NewLine}{stderr}");
        }
    }

    private static string ResolveIlasm(string? configuredPath)
    {
        if (!string.IsNullOrWhiteSpace(configuredPath))
            return configuredPath!;

        var envPath = Environment.GetEnvironmentVariable("ILASM_PATH");
        if (!string.IsNullOrWhiteSpace(envPath))
            return envPath!;

        var candidates = OperatingSystem.IsWindows()
            ? new[] { "ilasm.exe", "ilasm" }
            : new[] { "ilasm" };

        foreach (var candidate in candidates)
        {
            var resolved = FindOnPath(candidate);
            if (resolved is not null)
                return resolved;
        }

        throw new InvalidOperationException("Unable to locate ilasm. Set ILASM_PATH or pass IlCompilationOptions.ilasmPath.");
    }

    private static string? FindOnPath(string executable)
    {
        var paths = Environment.GetEnvironmentVariable("PATH")?.Split(Path.PathSeparator) ?? Array.Empty<string>();
        foreach (var path in paths)
        {
            if (string.IsNullOrWhiteSpace(path)) continue;
            var candidate = Path.Combine(path, executable);
            if (OperatingSystem.IsWindows() && !candidate.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                candidate += ".exe";
            if (File.Exists(candidate))
                return candidate;
        }
        return null;
    }
}

internal sealed class IlGenerator
{
    private readonly IlCompilationOptions _options;
    private readonly Dictionary<string, ClassModel> _classes = new(StringComparer.Ordinal);
    private int _labelCounter;

    public IlGenerator(IlCompilationOptions options) => _options = options;
    
    private string NewLabel(string prefix) => $"IL_{prefix}_{_labelCounter++:D4}";

    private string EscapeIlIdentifier(string name)
    {
        // "value" is a reserved word in IL (used in property setters)
        if (string.Equals(name, "value", StringComparison.Ordinal))
            return "'value'";
        return name;
    }

    private string EscapeIlMethodName(string name)
    {
        // IL instruction keywords that need to be escaped
        var ilKeywords = new HashSet<string>(StringComparer.Ordinal)
        {
            "add", "and", "arglist", "beq", "bge", "bgt", "ble", "blt", "bne.un", "br", "break",
            "brfalse", "brtrue", "call", "calli", "callvirt", "castclass", "ceq", "cgt", "clt",
            "conv.i", "conv.i1", "conv.i2", "conv.i4", "conv.i8", "conv.r4", "conv.r8", "conv.u",
            "conv.u1", "conv.u2", "conv.u4", "conv.u8", "cpblk", "cpobj", "div", "dup", "endfilter",
            "endfinally", "initblk", "initobj", "isinst", "jmp", "ldarg", "ldarga", "ldc.i4", "ldc.i8",
            "ldc.r4", "ldc.r8", "ldelem", "ldelema", "ldfld", "ldflda", "ldftn", "ldind", "ldlen",
            "ldloc", "ldloca", "ldnull", "ldobj", "ldsfld", "ldsflda", "ldstr", "ldtoken", "ldvirtftn",
            "leave", "localloc", "mkrefany", "mul", "neg", "newarr", "newobj", "nop", "not", "or",
            "pop", "readonly", "refanytype", "refanyval", "rem", "ret", "rethrow", "shl", "shr",
            "sizeof", "starg", "stelem", "stfld", "stind", "stloc", "stobj", "stsfld", "sub", "switch",
            "tail", "throw", "unaligned", "unbox", "volatile", "xor"
        };
        
        if (ilKeywords.Contains(name))
            return $"'{name}'";
        return name;
    }

    public string Generate(ProgramNode program)
    {
        if (program.Classes.Count == 0)
            throw new InvalidOperationException("Program does not contain any classes.");

        foreach (var cls in program.Classes)
        {
            if (_classes.ContainsKey(cls.Name))
                throw new InvalidOperationException($"Duplicate class '{cls.Name}'.");
            _classes[cls.Name] = new ClassModel(cls, this);
        }

        foreach (var cls in _classes.Values)
            cls.ResolveBase(_classes);

        var entry = ((string, string)?)null;
        var defaultEntryClass = entry is null ? FindDefaultEntryClass(program) : null;

        var sb = new StringBuilder();
        EmitHeader(sb);
        foreach (var cls in program.Classes)
        {
            var model = _classes[cls.Name];
            EmitClass(sb, model, null);
        }
        
        // Always generate a Main class with static Main method that reads args
        // The entry point is determined at runtime via command-line arguments
        EmitRuntimeEntryPoint(sb, program);
        
        return sb.ToString();
    }

    private (string Class, string Method)? ResolveEntryPoint(ProgramNode program)
    {
        if (!string.IsNullOrWhiteSpace(_options.EntryPointClass) && !string.IsNullOrWhiteSpace(_options.EntryPointMethod))
            return (_options.EntryPointClass!, _options.EntryPointMethod!);

        foreach (var cls in program.Classes)
        {
            foreach (var member in cls.Members.OfType<MethodNode>())
            {
                if (string.Equals(member.Name, "main", StringComparison.OrdinalIgnoreCase))
                    return (cls.Name, member.Name);
            }
        }
        return null;
    }

    private string? FindDefaultEntryClass(ProgramNode program)
    {
        // Look for a class with "Test" in the name first
        foreach (var cls in program.Classes)
        {
            if (cls.Name.Contains("Test", StringComparison.OrdinalIgnoreCase))
            {
                // Check if it has a parameterless constructor
                var constructors = cls.Members.OfType<ConstructorNode>().ToList();
                var hasParameterlessCtor = constructors.Any(c => c.Parameters.Count == 0);
                if (hasParameterlessCtor || constructors.Count == 0)
                    return cls.Name;
            }
        }
        
        // Otherwise, find the first class with a parameterless constructor
        foreach (var cls in program.Classes)
        {
            var constructors = cls.Members.OfType<ConstructorNode>().ToList();
            var hasParameterlessCtor = constructors.Any(c => c.Parameters.Count == 0);
            if (hasParameterlessCtor || constructors.Count == 0)
                return cls.Name;
        }
        
        return program.Classes.Count > 0 ? program.Classes[0].Name : null;
    }

    private void EmitRuntimeEntryPoint(StringBuilder sb, ProgramNode program)
    {
        var exitLabel = NewLabel("EXIT");
        var argErrorLabel = NewLabel("ARG_ERROR");
        
        // Найти класс Main
        var mainClass = program.Classes.FirstOrDefault(c => c.Name == "Main");
        
        sb.AppendLine($".class public auto ansi beforefieldinit {_options.Namespace}.Program");
        sb.AppendLine("         extends [mscorlib]System.Object");
        sb.AppendLine("{");
        sb.AppendLine("  .method public hidebysig static void Main(string[] args) cil managed");
        sb.AppendLine("  {");
        sb.AppendLine("    .entrypoint");
        sb.AppendLine("    .maxstack 16");
        sb.AppendLine("    .locals init (string V_0)");
        
        // Если args пустой
        sb.AppendLine("    ldarg.0");
        sb.AppendLine("    brfalse IL_NO_ARGS");
        sb.AppendLine("    ldarg.0");
        sb.AppendLine("    ldlen");
        sb.AppendLine("    conv.i4");
        sb.AppendLine("    ldc.i4.0");
        sb.AppendLine("    ble IL_NO_ARGS");
        
        sb.AppendLine("    ldarg.0");
        sb.AppendLine("    ldc.i4.0");
        sb.AppendLine("    ldelem.ref");
        sb.AppendLine("    stloc V_0");
        
        foreach (var cls in program.Classes)
        {
            var className = cls.Name;
            var qualifiedName = $"{_options.Namespace}.{className}";
            var checkLabel = $"IL_{className}_CHECK";
            var doneLabel = $"IL_{className}_DONE";
            
            sb.AppendLine($"  {checkLabel}:");
            sb.AppendLine("    ldloc V_0");
            sb.AppendLine($"    ldstr \"{className}\"");
            sb.AppendLine("    call bool [mscorlib]System.String::op_Equality(string, string)");
            sb.AppendLine($"    brtrue.s IL_{className}_MATCH");
            sb.AppendLine($"    br.s {doneLabel}");
            sb.AppendLine($"  IL_{className}_MATCH:");
            
            var constructors = cls.Members.OfType<ConstructorNode>().ToList();
            var ctor = constructors.Count > 0 ? constructors[0] : null;
            var paramCount = ctor?.Parameters.Count ?? 0;
            
            sb.AppendLine("    ldarg.0");
            sb.AppendLine("    ldlen");
            sb.AppendLine("    conv.i4");
            sb.AppendLine($"    ldc.i4 {paramCount + 1}");
            sb.AppendLine($"    bne.un {argErrorLabel}");
            
            for (int i = 0; i < paramCount; i++)
            {
                sb.AppendLine("    ldarg.0");
                sb.AppendLine($"    ldc.i4 {i + 1}");
                sb.AppendLine("    ldelem.ref");
                
                var paramType = ctor!.Parameters[i].Type;
                if (paramType == "Integer")
                    sb.AppendLine("    call int32 [mscorlib]System.Int32::Parse(string)");
                else if (paramType == "Real")
                    sb.AppendLine("    call float64 [mscorlib]System.Double::Parse(string)");
                else if (paramType == "Boolean")
                    sb.AppendLine("    call bool [mscorlib]System.Boolean::Parse(string)");
            }
            
            var paramTypes = paramCount == 0 ? "" : string.Join(", ", ctor!.Parameters.Select(p => 
                p.Type switch
                {
                    "Integer" => "int32",
                    "Real" => "float64",
                    "Boolean" => "valuetype [mscorlib]System.Boolean",
                    "String" => "string",
                    _ => $"class {_options.Namespace}.{p.Type}"
                }));
            
            sb.AppendLine($"    newobj instance void {qualifiedName}::.ctor({paramTypes})");
            sb.AppendLine("    pop");
            sb.AppendLine($"    br {exitLabel}");
            
            sb.AppendLine($"  {doneLabel}:");
        }
        
        sb.AppendLine("    ldstr \"Error: Class not found\"");
        sb.AppendLine("    call void [mscorlib]System.Console::WriteLine(string)");
        sb.AppendLine("    ret");
        
        sb.AppendLine($"  {argErrorLabel}:");
        sb.AppendLine("    ldstr \"Error: Class not found or invalid number of arguments\"");
        sb.AppendLine("    call void [mscorlib]System.Console::WriteLine(string)");
        sb.AppendLine("    ret");
        
        sb.AppendLine("  IL_NO_ARGS:");
        if (mainClass != null)
        {
            var mainQualifiedName = $"{_options.Namespace}.Main";
            var mainCtor = mainClass.Members.OfType<ConstructorNode>().FirstOrDefault();
            var mainParamCount = mainCtor?.Parameters.Count ?? 0;
            
            if (mainParamCount == 0)
            {
                sb.AppendLine($"    newobj instance void {mainQualifiedName}::.ctor()");
                sb.AppendLine("    pop");
                sb.AppendLine($"    br {exitLabel}");
            }
            else
            {
                sb.AppendLine($"    ldstr \"Error: Class 'Main' requires {mainParamCount} arguments\"");
                sb.AppendLine("    call void [mscorlib]System.Console::WriteLine(string)");
                sb.AppendLine("    ret");
            }
        }
        else
        {
            sb.AppendLine("    ldstr \"Error: No class name specified and no 'Main' class found\"");
            sb.AppendLine("    call void [mscorlib]System.Console::WriteLine(string)");
            sb.AppendLine("    ret");
        }
        
        sb.AppendLine($"  {exitLabel}:");
        sb.AppendLine("    ret");
        sb.AppendLine("  }");
        sb.AppendLine("}");
    }

    
    private void EmitClassConstructorDispatch(StringBuilder sb, ClassNode cls)
    {
        var className = cls.Name;
        var qualifiedName = $"{_options.Namespace}.{className}";
        var constructors = cls.Members.OfType<ConstructorNode>().ToList();
        
        // If no constructors, generate default one
        if (constructors.Count == 0)
        {
            constructors.Add(null!); // Will be handled as parameterless
        }
        
        var labelPrefix = $"IL_{className}_";
        var checkLabel = $"{labelPrefix}CHECK";
        var doneLabel = $"{labelPrefix}DONE";
        
        // Check if class name matches
        sb.AppendLine($"  {checkLabel}:");
        sb.AppendLine("    ldloc V_0");
        sb.AppendLine($"    ldstr \"{className}\"");
        sb.AppendLine("    call bool [mscorlib]System.String::op_Equality(string, string)");
        sb.AppendLine($"    brfalse.s {doneLabel}");
        
        // For each constructor, check argument count and call if match
        for (int i = 0; i < constructors.Count; i++)
        {
            var ctor = constructors[i];
            var paramCount = ctor?.Parameters.Count ?? 0;
            var ctorLabel = $"{labelPrefix}CTOR{paramCount}";
            var nextLabel = $"{labelPrefix}NEXT{paramCount}";
            
            sb.AppendLine($"  {ctorLabel}:");
            sb.AppendLine("    ldloc V_1");
            sb.AppendLine($"    ldc.i4 {paramCount}");
            sb.AppendLine("    bne.un.s " + (i < constructors.Count - 1 ? nextLabel : doneLabel));
            
            // Emit constructor call with argument parsing
            EmitConstructorCallWithArgs(sb, qualifiedName, ctor);
            sb.AppendLine("    pop");
            sb.AppendLine($"    br.s {doneLabel}");
            
            if (i < constructors.Count - 1)
            {
                sb.AppendLine($"  {nextLabel}:");
            }
        }
        
        sb.AppendLine($"  {doneLabel}:");
    }
    
    private void EmitConstructorCallWithArgs(StringBuilder sb, string qualifiedClassName, ConstructorNode? ctor)
    {
        var paramCount = ctor?.Parameters.Count ?? 0;
        
        if (paramCount == 0)
        {
            sb.AppendLine($"    newobj instance void {qualifiedClassName}::.ctor()");
        }
        else
        {
            // Load arguments from args array (args[1], args[2], etc.)
            for (int i = 0; i < paramCount; i++)
            {
                var param = ctor!.Parameters[i];
                sb.AppendLine("    ldarg.0");
                sb.AppendLine($"    ldc.i4 {i + 1}");
                sb.AppendLine("    ldelem.ref");
                
                // Convert string to appropriate type
                if (string.Equals(param.Type, "Integer", StringComparison.Ordinal))
                {
                    sb.AppendLine("    call int32 [mscorlib]System.Int32::Parse(string)");
                }
                else if (string.Equals(param.Type, "Real", StringComparison.Ordinal))
                {
                    sb.AppendLine("    call float64 [mscorlib]System.Double::Parse(string)");
                }
                else if (string.Equals(param.Type, "Boolean", StringComparison.Ordinal))
                {
                    sb.AppendLine("    call bool [mscorlib]System.Boolean::Parse(string)");
                }
                // String and class types are already objects, no conversion needed
            }
            
            var paramTypes = string.Join(", ", ctor!.Parameters.Select(p => 
            {
                var type = p.Type switch
                {
                    "Integer" => "int32",
                    "Real" => "float64",
                    "Boolean" => "valuetype [mscorlib]System.Boolean",
                    "String" => "string",
                    _ => $"class {_options.Namespace}.{p.Type}"
                };
                return type;
            }));
            
            sb.AppendLine($"    newobj instance void {qualifiedClassName}::.ctor({paramTypes})");
        }
    }

    private void EmitHeader(StringBuilder sb)
    {
        sb.AppendLine(".assembly extern mscorlib { }");
        sb.AppendLine($".assembly {_options.AssemblyName}");
        sb.AppendLine("{");
        sb.AppendLine("}");
        sb.AppendLine($".module {_options.AssemblyName}.exe");
    }

    private void EmitClass(StringBuilder sb, ClassModel cls, (string Class, string Method)? entryPoint)
    {
        var extends = cls.BaseClass is null
            ? "[mscorlib]System.Object"
            : cls.BaseClass.GetQualifiedName();

        sb.AppendLine($".class public auto ansi beforefieldinit {_options.Namespace}.{cls.Name}");
        sb.AppendLine($"         extends {extends}");
        sb.AppendLine("{");

        foreach (var field in cls.Fields.Values)
        {
            var fieldName = EscapeIlIdentifier(field.Name);
            sb.AppendLine($"  .field public {field.Type.IlName} {fieldName}");
        }


        if (cls.Constructors.Count == 0)
        {
            EmitConstructor(sb, cls, null);
        }
        else
        {
            foreach (var ctor in cls.Constructors)
                EmitConstructor(sb, cls, ctor);
        }

        foreach (var method in cls.Methods)
        {
            var isEntryPoint = entryPoint is not null &&
                               string.Equals(entryPoint.Value.Class, cls.Name, StringComparison.Ordinal) &&
                               string.Equals(entryPoint.Value.Method, method.Name, StringComparison.OrdinalIgnoreCase);
            EmitMethod(sb, cls, method, isEntryPoint);
        }

        sb.AppendLine("  }");
    }

    private void EmitConstructor(StringBuilder sb, ClassModel cls, ConstructorNode? ctorNode)
    {
        IReadOnlyList<ParameterNode> parameters = ctorNode is null
            ? Array.Empty<ParameterNode>()
            : ctorNode.Parameters;
        var parameterSignature = string.Join(", ", parameters.Select(FormatParameterType));
        if (parameterSignature.Length == 0)
            parameterSignature = string.Empty;

        sb.AppendLine($"  .method public hidebysig specialname rtspecialname instance void .ctor({parameterSignature}) cil managed");
        sb.AppendLine("  {");
        sb.AppendLine("    .maxstack 8");

        var context = new MethodContext(this, cls, sb, ctorNode, null);
        context.EmitConstructorBody(ctorNode);

        sb.AppendLine("  }");
    }

    private void EmitMethod(StringBuilder sb, ClassModel cls, MethodNode method, bool isEntryPoint)
    {
        var returnType = string.IsNullOrWhiteSpace(method.ReturnType)
            ? TypeInfo.Void
            : ResolveType(method.ReturnType);
        var parameterSignature = string.Join(", ", method.Parameters.Select(FormatParameterType));
        var methodName = EscapeIlMethodName(method.Name);
        var methodHeader = $"  .method public hidebysig instance {returnType.IlName} {methodName}({parameterSignature}) cil managed";
        sb.AppendLine(methodHeader);
        sb.AppendLine("  {");
        if (isEntryPoint)
            sb.AppendLine("    .entrypoint");
        sb.AppendLine("    .maxstack 8");

        var context = new MethodContext(this, cls, sb, null, method);
        context.EmitMethodBody(method);

        sb.AppendLine("  }");
    }

    private string FormatParameterType(ParameterNode parameter)
    {
        var type = ResolveType(parameter.Type);
        return $"{type.IlName} {parameter.Name}";
    }

    private TypeInfo ResolveType(string? sourceType)
    {
        if (string.IsNullOrWhiteSpace(sourceType))
            return TypeInfo.Object;

        // Handle Array[T]
        if (sourceType.StartsWith("Array[") && sourceType.EndsWith("]"))
        {
            var elementType = sourceType.Substring(6, sourceType.Length - 7);
            var ilType = elementType switch
            {
                "Integer" => "int32",
                "Real" => "float64",
                "Boolean" => "int32",
                "String" => "string",
                _ => $"class {_options.Namespace}.{elementType}"
            };
            return new TypeInfo(sourceType, $"{ilType}[]", false);
        }

        // Handle List[T]
        if (sourceType.StartsWith("List[") && sourceType.EndsWith("]"))
        {
            var elementType = sourceType.Substring(5, sourceType.Length - 6);
            var ilType = elementType switch
            {
                "Integer" => "int32",
                "Real" => "float64",
                "Boolean" => "int32",
                "String" => "string",
                _ => $"class {_options.Namespace}.{elementType}"
            };
            return new TypeInfo(sourceType, $"class [mscorlib]System.Collections.Generic.List`1<{ilType}>", false);
        }

        return sourceType switch
        {
            "Integer" => TypeInfo.Int32,
            "Real" => TypeInfo.Float64,
            "Boolean" => TypeInfo.Boolean,
            "String" => TypeInfo.String,
            _ when _classes.ContainsKey(sourceType) => new TypeInfo(sourceType, $"class {_options.Namespace}.{sourceType}", false),
            _ => TypeInfo.Object
        };
    }

    private bool TryResolveClass(string name, out ClassModel model)
        => _classes.TryGetValue(name, out model!);

    private static TypeInfo InferFieldType(VariableNode field, IlGenerator owner)
    {
        // If type is explicitly provided, use it
        if (!string.IsNullOrWhiteSpace(field.Type))
            return owner.ResolveType(field.Type);

        // Otherwise, try to infer from initializer
        if (field.Initializer is ConstructorCallNode ctor)
        {
            // Integer(0) -> Integer type
            if (string.Equals(ctor.ClassName, "Integer", StringComparison.Ordinal))
                return TypeInfo.Int32;
            if (string.Equals(ctor.ClassName, "Real", StringComparison.Ordinal))
                return TypeInfo.Float64;
            if (string.Equals(ctor.ClassName, "Boolean", StringComparison.Ordinal))
                return TypeInfo.Boolean;
            if (string.Equals(ctor.ClassName, "String", StringComparison.Ordinal))
                return TypeInfo.String;
            
            // Otherwise, it's a class type
            return owner.ResolveType(ctor.ClassName);
        }

        // Default to Object if we can't infer
        return TypeInfo.Object;
    }

    private sealed record TypeInfo(string SourceName, string IlName, bool IsValueType)
    {
        public static readonly TypeInfo Void = new("void", "void", true);
        public static readonly TypeInfo Int32 = new("Integer", "int32", true);
        public static readonly TypeInfo Float64 = new("Real", "float64", true);
        public static readonly TypeInfo Boolean = new("Boolean", "int32", true);
        public static readonly TypeInfo String = new("String", "string", false);
        public static readonly TypeInfo Object = new("Object", "class [mscorlib]System.Object", false);
    }

    private sealed class ClassModel
    {
        private readonly IlGenerator _owner;

        public ClassNode Node { get; }
        public string Name => Node.Name;
        public IReadOnlyDictionary<string, FieldDefinition> Fields => _fields;
        public IReadOnlyList<ConstructorNode> Constructors => _constructors;
        public IReadOnlyList<MethodNode> Methods => _methods;
        public ClassModel? BaseClass { get; private set; }

        private readonly Dictionary<string, FieldDefinition> _fields = new(StringComparer.Ordinal);
        private readonly List<ConstructorNode> _constructors = new();
        private readonly List<MethodNode> _methods = new();

        public ClassModel(ClassNode node, IlGenerator owner)
        {
            Node = node;
            _owner = owner;

            foreach (var member in node.Members)
            {
                switch (member)
                {
                    case VariableNode field:
                        var fieldType = InferFieldType(field, owner);
                        _fields[field.Name] = new FieldDefinition(field.Name, fieldType, field);
                        break;
                    case ConstructorNode ctor:
                        _constructors.Add(ctor);
                        break;
                    case MethodNode method:
                        _methods.Add(method);
                        break;
                }
            }
        }

        public void ResolveBase(Dictionary<string, ClassModel> classes)
        {
            if (Node.Parent is not null && classes.TryGetValue(Node.Parent.Name, out var baseClass))
            {
                BaseClass = baseClass;
            }
        }

        public string GetQualifiedName() => $"{_owner._options.Namespace}.{Name}";

        public FieldDefinition? FindField(string name)
        {
            if (_fields.TryGetValue(name, out var field))
                return field;
            return BaseClass?.FindField(name);
        }

        public MethodNode? FindMethod(string name, int arity)
        {
            var method = _methods.FirstOrDefault(m => string.Equals(m.Name, name, StringComparison.Ordinal) &&
                                                      m.Parameters.Count == arity);
            if (method is not null)
                return method;
            return BaseClass?.FindMethod(name, arity);
        }
        
        public MethodNode? FindMethod(string name, List<string> paramTypes)
        {
            var method = _methods.FirstOrDefault(m => 
                string.Equals(m.Name, name, StringComparison.Ordinal) &&
                m.Parameters.Count == paramTypes.Count &&
                m.Parameters.Zip(paramTypes, (p, t) => string.Equals(p.Type, t, StringComparison.Ordinal)).All(x => x));
            if (method is not null)
                return method;
            return BaseClass?.FindMethod(name, paramTypes);
        }

        public ConstructorNode? FindConstructor(int arity)
            => _constructors.FirstOrDefault(c => c.Parameters.Count == arity);
    }

    private sealed record FieldDefinition(string Name, TypeInfo Type, VariableNode Node);

    private sealed class MethodContext
    {
        private readonly IlGenerator _owner;
        private readonly ClassModel _class;
        private readonly StringBuilder _sb;
        private readonly MethodNode? _methodNode;
        private readonly ConstructorNode? _ctorNode;

        private readonly Dictionary<string, LocalInfo> _locals = new(StringComparer.Ordinal);
        private readonly Dictionary<string, ParameterInfo> _parameters = new(StringComparer.Ordinal);

        private int _nextLocalSlot;
        private int _labelCounter;
        private bool _emittedReturn;

        public MethodContext(IlGenerator owner, ClassModel cls, StringBuilder sb, ConstructorNode? ctorNode, MethodNode? methodNode)
        {
            _owner = owner;
            _class = cls;
            _sb = sb;
            _ctorNode = ctorNode;
            _methodNode = methodNode;

            InitializeParameters();
            RegisterLocals();
        }

        private void InitializeParameters()
        {
            int startIndex = 0;
            if (_methodNode is not null)
            {
                for (int i = 0; i < _methodNode.Parameters.Count; i++)
                {
                    var parameter = _methodNode.Parameters[i];
                    var type = _owner.ResolveType(parameter.Type);
                    _parameters[parameter.Name] = new ParameterInfo(parameter.Name, type, startIndex + i + 1);
                }
            }
            else if (_ctorNode is not null)
            {
                for (int i = 0; i < _ctorNode.Parameters.Count; i++)
                {
                    var parameter = _ctorNode.Parameters[i];
                    var type = _owner.ResolveType(parameter.Type);
                    _parameters[parameter.Name] = new ParameterInfo(parameter.Name, type, startIndex + i + 1);
                }
            }
        }

        private void RegisterLocals()
        {
            IReadOnlyList<StatementNode> statements = _methodNode is not null
                ? _methodNode.Body
                : _ctorNode is not null
                    ? _ctorNode.Body
                    : Array.Empty<StatementNode>();
            foreach (var statement in statements)
            {
                if (statement is VariableStatementNode variable)
                {
                    var typeName = !string.IsNullOrWhiteSpace(variable.Type)
                        ? variable.Type
                        : InferType(variable.Initializer);
                    var type = _owner.ResolveType(typeName);
                    var local = new LocalInfo(variable.Name, type, _nextLocalSlot++);
                    _locals[variable.Name] = local;
                }
            }

            if (_locals.Count > 0)
            {
                var localsSignature = string.Join(", ", _locals.Values
                    .OrderBy(l => l.Slot)
                    .Select(l => $"{l.Type.IlName} V_{l.Slot}"));
                _sb.AppendLine($"      .locals init ({localsSignature})");
            }
        }

        public void EmitConstructorBody(ConstructorNode? ctor)
        {
            EmitLoadThis();
            
            // Вызвать конструктор базового класса с параметрами
            if (_class.BaseClass is not null)
            {
                var baseName = _class.BaseClass.GetQualifiedName();
                var baseCtors = _class.BaseClass.Node.Members.OfType<ConstructorNode>().ToList();
                
                if (baseCtors.Count > 0)
                {
                    var baseCtor = baseCtors[0];
                    // Передать первые N параметров базовому конструктору
                    for (int i = 0; i < baseCtor.Parameters.Count && i < (ctor?.Parameters.Count ?? 0); i++)
                    {
                        EmitLoadArgument(i + 1);
                    }
                    var baseParamTypes = string.Join(", ", baseCtor.Parameters.Select(p => _owner.ResolveType(p.Type).IlName));
                    _sb.AppendLine($"      call instance void {baseName}::.ctor({baseParamTypes})");
                }
                else
                {
                    _sb.AppendLine($"      call instance void {baseName}::.ctor()");
                }
            }
            else
            {
                _sb.AppendLine("      call instance void [mscorlib]System.Object::.ctor()");
            }

            // Инициализировать поля
            foreach (var field in _class.Fields.Values)
            {
                if (field.Node.Initializer is null) continue;
                EmitLoadThis();
                EmitExpression(field.Node.Initializer);
                _sb.AppendLine($"      stfld {field.Type.IlName} {_class.GetQualifiedName()}::{_owner.EscapeIlIdentifier(field.Name)}");
            }

            // Выполнить тело конструктора
            if (ctor is not null)
            {
                foreach (var statement in ctor.Body)
                    EmitStatement(statement);
            }

            if (!_emittedReturn)
            {
                _sb.AppendLine("      ret");
                _emittedReturn = true;
            }
        }

        public void EmitMethodBody(MethodNode method)
        {
            foreach (var statement in method.Body)
            {
                EmitStatement(statement);
                if (_emittedReturn)
                    return;
            }

            if (string.IsNullOrWhiteSpace(method.ReturnType) && !_emittedReturn)
            {
                _sb.AppendLine("      ret");
                _emittedReturn = true;
            }
        }

        private void EmitStatement(StatementNode statement)
        {
            switch (statement)
            {
                case VariableStatementNode variable:
                    if (variable.Initializer is not null && _locals.TryGetValue(variable.Name, out var local))
                    {
                        EmitExpression(variable.Initializer);
                        _sb.AppendLine($"      stloc V_{local.Slot}");
                    }
                    break;
                case ReturnNode ret:
                    EmitExpression(ret.Expression);
                    _sb.AppendLine("      ret");
                    _emittedReturn = true;
                    break;
                case ExpressionStatementNode expr:
                    var type = EmitExpression(expr.Expression);
                    if (type != TypeInfo.Void)
                        _sb.AppendLine("      pop");
                    break;
                case IfNode ifNode:
                    EmitIf(ifNode);
                    break;
                case WhileNode whileNode:
                    EmitWhile(whileNode);
                    break;
            }
        }

        private void EmitIf(IfNode node)
        {
            var elseLabel = NewLabel("ELSE");
            var endLabel = NewLabel("ENDIF");

            EmitExpression(node.Condition);
            _sb.AppendLine($"      brfalse {elseLabel}");

            foreach (var statement in node.ThenBody)
                EmitStatement(statement);
            _sb.AppendLine($"      br {endLabel}");

            _sb.AppendLine($"    {elseLabel}:");
            if (node.ElseBody is not null)
            {
                foreach (var statement in node.ElseBody)
                    EmitStatement(statement);
            }

            _sb.AppendLine($"    {endLabel}:");
        }

        private void EmitWhile(WhileNode node)
        {
            var startLabel = NewLabel("WHILE_START");
            var endLabel = NewLabel("WHILE_END");

            _sb.AppendLine($"    {startLabel}:");
            EmitExpression(node.Condition);
            _sb.AppendLine($"      brfalse {endLabel}");

            foreach (var statement in node.Body)
                EmitStatement(statement);
            _sb.AppendLine($"      br {startLabel}");
            _sb.AppendLine($"    {endLabel}:");
        }

        private string InferType(ExpressionNode? expression)
        {
            return expression switch
            {
                IntegerLiteralNode => "Integer",
                RealLiteralNode => "Real",
                BooleanLiteralNode => "Boolean",
                StringLiteralNode => "String",
                ThisNode => _class.Name,
                ConstructorCallNode ctor => ctor.ClassName,
                IdentifierNode id when _locals.TryGetValue(id.Name, out var local) => local.Type.SourceName,
                IdentifierNode id when _parameters.TryGetValue(id.Name, out var parameter) => parameter.Type.SourceName,
                IdentifierNode id => _class.FindField(id.Name)?.Type.SourceName ?? "Object",
                MemberAccessNode member when member.Member is IdentifierNode fieldName => InferFieldAccessType(member, fieldName),
                MemberAccessNode member => InferMemberAccessType(member),
                _ => "Object"
            };
        }

        private string InferFieldAccessType(MemberAccessNode member, IdentifierNode fieldName)
        {
            var receiverType = InferType(member.Target);
            if (_owner.TryResolveClass(receiverType, out var receiverClass))
            {
                var field = receiverClass.FindField(fieldName.Name);
                if (field is not null)
                    return field.Type.SourceName;
            }
            return "Object"
;        }

        private string InferMemberAccessType(MemberAccessNode member)
        {
            if (member.Member is CallNode call && call.Target is IdentifierNode methodName)
            {
                var receiverType = InferType(member.Target);
                
                // Handle Array methods
                if (receiverType.StartsWith("Array["))
                {
                    var elementType = receiverType.Substring(6, receiverType.Length - 7);
                    return methodName.Name switch
                    {
                        "Length" => "Integer",
                        "get" => elementType,
                        "set" => "void",
                        "Print" => "void",
                        _ => "Object"
                    };
                }
                
                // Handle List methods
                if (receiverType.StartsWith("List["))
                {
                    var elementType = receiverType.Substring(5, receiverType.Length - 6);
                    return methodName.Name switch
                    {
                        "Length" => "Integer",
                        "head" => elementType,
                        "tail" => receiverType,
                        "append" => receiverType,
                        _ => "Object"
                    };
                }
                
                if (receiverType == "Integer")
                {
                    if (methodName.Name is "Plus" or "Minus" or "Mult" or "Div")
                    {
                        if (call.Arguments.Count > 0)
                        {
                            var argType = InferType(call.Arguments[0]);
                            if (argType == "Real")
                                return "Real";
                        }
                        return "Integer";
                    }
                    return methodName.Name switch
                    {
                        "Print" => "void",
                        "Rem" => "Integer",
                        "Less" or "Greater" or "Equal" or "LessEqual" or "GreaterEqual" => "Boolean",
                        "toReal" => "Real",
                        "UnaryMinus" => "Integer",
                        _ => "Object"
                    };
                }
                
                if (receiverType == "Real")
                {
                    return methodName.Name switch
                    {
                        "Print" => "void",
                        "Plus" or "Minus" or "Mult" or "Div" => "Real",
                        "Less" or "Greater" or "Equal" or "LessEqual" or "GreaterEqual" => "Boolean",
                        "toInteger" => "Integer",
                        "UnaryMinus" => "Real",
                        _ => "Object"
                    };
                }
                
                if (receiverType == "Boolean")
                {
                    return methodName.Name switch
                    {
                        "Print" => "void",
                        "And" or "Or" or "Xor" or "Not" => "Boolean",
                        "toInteger" => "Integer",
                        _ => "Object"
                    };
                }
                
                if (receiverType == "String")
                {
                    return methodName.Name switch
                    {
                        "Print" => "void",
                        "Plus" => "String",
                        _ => "Object"
                    };
                }
                
                if (_owner.TryResolveClass(receiverType, out var receiverClass))
                {
                    // Try to find method by parameter types for overload resolution
                    var argTypes = call.Arguments.Select(arg => InferType(arg)).ToList();
                    var method = receiverClass.FindMethod(methodName.Name, argTypes);
                    
                    // Fallback to arity-based lookup
                    if (method is null)
                        method = receiverClass.FindMethod(methodName.Name, call.Arguments.Count);
                    
                    if (method is not null)
                        return string.IsNullOrWhiteSpace(method.ReturnType) ? "void" : method.ReturnType;
                }
            }
            else if (member.Member is IdentifierNode fieldName)
            {
                var receiverType = InferType(member.Target);
                if (_owner.TryResolveClass(receiverType, out var receiverClass))
                {
                    var field = receiverClass.FindField(fieldName.Name);
                    if (field is not null)
                        return field.Type.SourceName;
                }
            }
            
            return "Object";
        }

        private TypeInfo EmitExpression(ExpressionNode expression)
        {
            return expression switch
            {
                IntegerLiteralNode literal => EmitIntegerLiteral(literal),
                RealLiteralNode real => EmitRealLiteral(real),
                BooleanLiteralNode boolean => EmitBooleanLiteral(boolean),
                StringLiteralNode str => EmitStringLiteral(str),
                ThisNode => EmitThis(),
                IdentifierNode id => EmitIdentifier(id),
                AssignmentNode assignment => EmitAssignment(assignment),
                MemberAccessNode member => EmitMemberAccess(member),
                ConstructorCallNode ctor => EmitConstructorCall(ctor),
                CallNode call => EmitCall(call),
                _ => TypeInfo.Object
            };
        }

        private TypeInfo EmitIntegerLiteral(IntegerLiteralNode literal)
        {
            if (int.TryParse(literal.Value, NumberStyles.Integer, CultureInfo.InvariantCulture, out var value))
            {
                _sb.AppendLine($"      ldc.i4 {value}");
            }
            else
            {
                _sb.AppendLine("      ldc.i4 0");
            }
            return TypeInfo.Int32;
        }

        private TypeInfo EmitRealLiteral(RealLiteralNode literal)
        {
            if (double.TryParse(literal.Value, NumberStyles.Float, CultureInfo.InvariantCulture, out var value))
            {
                _sb.AppendLine($"      ldc.r8 {value.ToString(CultureInfo.InvariantCulture)}");
            }
            else
            {
                _sb.AppendLine("      ldc.r8 0.0");
            }
            return TypeInfo.Float64;
        }

        private TypeInfo EmitBooleanLiteral(BooleanLiteralNode literal)
        {
            var value = string.Equals(literal.Value, "true", StringComparison.OrdinalIgnoreCase) ? 1 : 0;
            _sb.AppendLine($"      ldc.i4 {value}");
            return TypeInfo.Boolean;
        }

        private TypeInfo EmitStringLiteral(StringLiteralNode literal)
        {
            var escaped = literal.Value.Replace("\\", "\\\\").Replace("\"", "\\\"");
            _sb.AppendLine($"      ldstr \"{escaped}\"");
            return TypeInfo.String;
        }

        private TypeInfo EmitThis()
        {
            EmitLoadThis();
            return new TypeInfo(_class.Name, $"class {_class.GetQualifiedName()}", false);
        }

        private TypeInfo EmitIdentifier(IdentifierNode identifier)
        {
            if (_locals.TryGetValue(identifier.Name, out var local))
            {
                _sb.AppendLine($"      ldloc V_{local.Slot}");
                return local.Type;
            }

            if (_parameters.TryGetValue(identifier.Name, out var parameter))
            {
                EmitLoadArgument(parameter.Index);
                return parameter.Type;
            }

            var field = _class.FindField(identifier.Name);
            if (field is not null)
            {
                EmitLoadThis();
                _sb.AppendLine($"      ldfld {field.Type.IlName} {_class.GetQualifiedName()}::{_owner.EscapeIlIdentifier(field.Name)}");
                return field.Type;
            }

            return TypeInfo.Object;
        }

        private TypeInfo EmitAssignment(AssignmentNode assignment)
        {
            if (assignment.Target is IdentifierNode id)
            {
                if (_locals.TryGetValue(id.Name, out var local))
                {
                    EmitExpression(assignment.Value);
                    _sb.AppendLine($"      stloc V_{local.Slot}");
                    return TypeInfo.Void;
                }

                if (_parameters.TryGetValue(id.Name, out var parameter))
                {
                    EmitExpression(assignment.Value);
                    EmitStoreArgument(parameter.Index);
                    return TypeInfo.Void;
                }

                var field = _class.FindField(id.Name);
                if (field is not null)
                {
                    EmitLoadThis();
                    EmitExpression(assignment.Value);
                    _sb.AppendLine($"      stfld {field.Type.IlName} {_class.GetQualifiedName()}::{_owner.EscapeIlIdentifier(field.Name)}");
                    return TypeInfo.Void;
                }
            }

            if (assignment.Target is MemberAccessNode member && member.Member is IdentifierNode fieldName)
            {
                EmitExpression(member.Target);
                EmitExpression(assignment.Value);
                var receiverType = InferType(member.Target);
                if (_owner.TryResolveClass(receiverType, out var receiverClass))
                {
                    var field = receiverClass.FindField(fieldName.Name);
                    if (field is not null)
                    {
                        _sb.AppendLine($"      stfld {field.Type.IlName} {receiverClass.GetQualifiedName()}::{_owner.EscapeIlIdentifier(field.Name)}");
                        return TypeInfo.Void;
                    }
                }
                return TypeInfo.Void;
            }

            EmitExpression(assignment.Value);
            return TypeInfo.Object;
        }

        private TypeInfo EmitMemberAccess(MemberAccessNode member)
        {
            if (member.Member is CallNode call && call.Target is IdentifierNode methodName)
            {
                var receiverTypeName2 = InferType(member.Target);
                
                // Handle Array methods
                if (receiverTypeName2.StartsWith("Array["))
                {
                    var elementType = receiverTypeName2.Substring(6, receiverTypeName2.Length - 7);
                    var ilElementType = elementType switch
                    {
                        "Integer" => "int32",
                        "Real" => "float64",
                        "Boolean" => "int32",
                        "String" => "string",
                        _ => $"class {_owner._options.Namespace}.{elementType}"
                    };
                    
                    if (methodName.Name == "Length" && call.Arguments.Count == 0)
                    {
                        EmitExpression(member.Target);
                        _sb.AppendLine("      ldlen");
                        _sb.AppendLine("      conv.i4");
                        return TypeInfo.Int32;
                    }
                    else if (methodName.Name == "get" && call.Arguments.Count == 1)
                    {
                        EmitExpression(member.Target);
                        EmitExpression(call.Arguments[0]);
                        _sb.AppendLine($"      ldelem {ilElementType}");
                        return _owner.ResolveType(elementType);
                    }
                    else if (methodName.Name == "set" && call.Arguments.Count == 2)
                    {
                        EmitExpression(member.Target);
                        EmitExpression(call.Arguments[0]);
                        EmitExpression(call.Arguments[1]);
                        _sb.AppendLine($"      stelem {ilElementType}");
                        return TypeInfo.Void;
                    }
                    else if (methodName.Name == "Print" && call.Arguments.Count == 0)
                    {
                        _sb.AppendLine($"      ldstr \"Array[{elementType}]\"");
                        _sb.AppendLine("      call void [mscorlib]System.Console::WriteLine(string)");
                        return TypeInfo.Void;
                    }
                }
                
                // Handle List methods
                if (receiverTypeName2.StartsWith("List["))
                {
                    var elementType = receiverTypeName2.Substring(5, receiverTypeName2.Length - 6);
                    var ilElementType = elementType switch
                    {
                        "Integer" => "int32",
                        "Real" => "float64",
                        "Boolean" => "int32",
                        "String" => "string",
                        _ => $"class {_owner._options.Namespace}.{elementType}"
                    };
                    var listType = $"class [mscorlib]System.Collections.Generic.List`1<{ilElementType}>";
                    
                    if (methodName.Name == "Length" && call.Arguments.Count == 0)
                    {
                        EmitExpression(member.Target);
                        _sb.AppendLine($"      callvirt instance int32 {listType}::get_Count()");
                        return TypeInfo.Int32;
                    }
                    else if (methodName.Name == "head" && call.Arguments.Count == 0)
                    {
                        EmitExpression(member.Target);
                        _sb.AppendLine("      ldc.i4.0");
                        _sb.AppendLine($"      callvirt instance !0 {listType}::get_Item(int32)");
                        return _owner.ResolveType(elementType);
                    }
                    else if (methodName.Name == "tail" && call.Arguments.Count == 0)
                    {
                        EmitExpression(member.Target);
                        _sb.AppendLine("      ldc.i4.1");
                        EmitExpression(member.Target);
                        _sb.AppendLine($"      callvirt instance int32 {listType}::get_Count()");
                        _sb.AppendLine("      ldc.i4.1");
                        _sb.AppendLine("      sub");
                        _sb.AppendLine($"      callvirt instance class [mscorlib]System.Collections.Generic.List`1<!0> {listType}::GetRange(int32, int32)");
                        return _owner.ResolveType(receiverTypeName2);
                    }
                    else if (methodName.Name == "append" && call.Arguments.Count == 1)
                    {
                        _sb.AppendLine($"      newobj instance void {listType}::.ctor()");
                        _sb.AppendLine("      dup");
                        EmitExpression(member.Target);
                        _sb.AppendLine($"      callvirt instance void {listType}::AddRange(class [mscorlib]System.Collections.Generic.IEnumerable`1<!0>)");
                        _sb.AppendLine("      dup");
                        EmitExpression(call.Arguments[0]);
                        _sb.AppendLine($"      callvirt instance void {listType}::Add(!0)");
                        return _owner.ResolveType(receiverTypeName2);
                    }
                }
                
                // Handle String methods
                if (receiverTypeName2 == "String")
                {
                    if (methodName.Name == "Plus" && call.Arguments.Count == 1)
                    {
                        EmitExpression(member.Target);
                        EmitExpression(call.Arguments[0]);
                        _sb.AppendLine("      call string [mscorlib]System.String::Concat(string, string)");
                        return TypeInfo.String;
                    }
                }
                
                // Handle built-in Print() method for primitives
                if (methodName.Name == "Print" && call.Arguments.Count == 0 && IsPrimitive(receiverTypeName2))
                {
                    var receiverType = EmitExpression(member.Target);
                    _sb.AppendLine($"      call void [mscorlib]System.Console::WriteLine({receiverType.IlName})");
                    return TypeInfo.Void;
                }
                
                // Handle built-in Integer methods
                if (receiverTypeName2 == "Integer" && methodName.Name != "Print")
                {
                    EmitExpression(member.Target);
                    var argType = call.Arguments.Count > 0 ? InferType(call.Arguments[0]) : "";
                    
                    switch (methodName.Name)
                    {
                        case "Plus":
                        case "Minus":
                        case "Mult":
                        case "Div":
                            if (argType == "Real")
                                _sb.AppendLine("      conv.r8");
                            EmitExpression(call.Arguments[0]);
                            _sb.AppendLine(methodName.Name switch
                            {
                                "Plus" => "      add",
                                "Minus" => "      sub",
                                "Mult" => "      mul",
                                "Div" => "      div",
                                _ => ""
                            });
                            return argType == "Real" ? TypeInfo.Float64 : TypeInfo.Int32;
                            
                        case "Rem":
                            EmitExpression(call.Arguments[0]);
                            _sb.AppendLine("      rem");
                            return TypeInfo.Int32;
                            
                        case "Less":
                        case "Greater":
                        case "Equal":
                        case "LessEqual":
                        case "GreaterEqual":
                            if (argType == "Real")
                                _sb.AppendLine("      conv.r8");
                            EmitExpression(call.Arguments[0]);
                            if (methodName.Name == "Less")
                                _sb.AppendLine("      clt");
                            else if (methodName.Name == "Greater")
                                _sb.AppendLine("      cgt");
                            else if (methodName.Name == "Equal")
                                _sb.AppendLine("      ceq");
                            else if (methodName.Name == "LessEqual")
                            {
                                _sb.AppendLine("      cgt");
                                _sb.AppendLine("      ldc.i4.0");
                                _sb.AppendLine("      ceq");
                            }
                            else if (methodName.Name == "GreaterEqual")
                            {
                                _sb.AppendLine("      clt");
                                _sb.AppendLine("      ldc.i4.0");
                                _sb.AppendLine("      ceq");
                            }
                            return TypeInfo.Boolean;
                            
                        case "toReal":
                            _sb.AppendLine("      conv.r8");
                            return TypeInfo.Float64;
                            
                        case "UnaryMinus":
                            _sb.AppendLine("      neg");
                            return TypeInfo.Int32;
                    }
                }
                
                // Handle built-in Real methods
                if (receiverTypeName2 == "Real")
                {
                    EmitExpression(member.Target);
                    var argType = call.Arguments.Count > 0 ? InferType(call.Arguments[0]) : "";
                    
                    switch (methodName.Name)
                    {
                        case "Plus":
                        case "Minus":
                        case "Mult":
                        case "Div":
                            EmitExpression(call.Arguments[0]);
                            if (argType == "Integer")
                                _sb.AppendLine("      conv.r8");
                            _sb.AppendLine(methodName.Name switch
                            {
                                "Plus" => "      add",
                                "Minus" => "      sub",
                                "Mult" => "      mul",
                                "Div" => "      div",
                                _ => ""
                            });
                            return TypeInfo.Float64;
                            
                        case "Less":
                        case "Greater":
                        case "Equal":
                        case "LessEqual":
                        case "GreaterEqual":
                            EmitExpression(call.Arguments[0]);
                            if (argType == "Integer")
                                _sb.AppendLine("      conv.r8");
                            if (methodName.Name == "Less")
                                _sb.AppendLine("      clt");
                            else if (methodName.Name == "Greater")
                                _sb.AppendLine("      cgt");
                            else if (methodName.Name == "Equal")
                                _sb.AppendLine("      ceq");
                            else if (methodName.Name == "LessEqual")
                            {
                                _sb.AppendLine("      cgt");
                                _sb.AppendLine("      ldc.i4.0");
                                _sb.AppendLine("      ceq");
                            }
                            else if (methodName.Name == "GreaterEqual")
                            {
                                _sb.AppendLine("      clt");
                                _sb.AppendLine("      ldc.i4.0");
                                _sb.AppendLine("      ceq");
                            }
                            return TypeInfo.Boolean;
                            
                        case "toInteger":
                            _sb.AppendLine("      conv.i4");
                            return TypeInfo.Int32;
                            
                        case "UnaryMinus":
                            _sb.AppendLine("      neg");
                            return TypeInfo.Float64;
                    }
                }
                
                // Handle built-in Boolean methods
                if (receiverTypeName2 == "Boolean")
                {
                    EmitExpression(member.Target);
                    
                    switch (methodName.Name)
                    {
                        case "And":
                            EmitExpression(call.Arguments[0]);
                            _sb.AppendLine("      and");
                            return TypeInfo.Boolean;
                            
                        case "Or":
                            EmitExpression(call.Arguments[0]);
                            _sb.AppendLine("      or");
                            return TypeInfo.Boolean;
                            
                        case "Xor":
                            EmitExpression(call.Arguments[0]);
                            _sb.AppendLine("      xor");
                            return TypeInfo.Boolean;
                            
                        case "Not":
                            _sb.AppendLine("      ldc.i4.0");
                            _sb.AppendLine("      ceq");
                            return TypeInfo.Boolean;
                            
                        case "toInteger":
                            // Boolean is already int32 in IL
                            return TypeInfo.Int32;
                    }
                }
                
                // Handle user-defined classes
                if (_owner.TryResolveClass(receiverTypeName2, out var receiverClass))
                {
                    EmitExpression(member.Target);
                    
                    // Infer argument types for overload resolution
                    var argTypes = call.Arguments.Select(arg => InferType(arg)).ToList();
                    var method = receiverClass.FindMethod(methodName.Name, argTypes);
                    
                    // Fallback to arity-based lookup if type-based fails
                    if (method is null)
                        method = receiverClass.FindMethod(methodName.Name, call.Arguments.Count);
                    
                    var returnType = method is null || string.IsNullOrWhiteSpace(method.ReturnType)
                        ? TypeInfo.Void
                        : _owner.ResolveType(method.ReturnType);
                    foreach (var argument in call.Arguments)
                        EmitExpression(argument);
                    var argumentSignature = method is null
                        ? string.Join(", ", Enumerable.Repeat("class [mscorlib]System.Object", call.Arguments.Count))
                        : string.Join(", ", method.Parameters.Select(p => _owner.ResolveType(p.Type).IlName));
                    var escapedMethodName = _owner.EscapeIlMethodName(methodName.Name);
                    _sb.AppendLine($"      callvirt instance {returnType.IlName} {receiverClass.GetQualifiedName()}::{escapedMethodName}({argumentSignature})");
                    return returnType;
                }
                return TypeInfo.Void;
            }
            else if (member.Member is IdentifierNode fieldName)
            {
                var receiverTypeName = InferType(member.Target);
                if (IsPrimitive(receiverTypeName) || string.IsNullOrEmpty(receiverTypeName)) return TypeInfo.Object;
                if (!_owner.TryResolveClass(receiverTypeName, out var receiverClass)) return TypeInfo.Object;
                var fieldType = receiverClass.FindField(fieldName.Name);
                if (fieldType is null)
                    return TypeInfo.Object;
                EmitExpression(member.Target);
                _sb.AppendLine($"      ldfld {fieldType.Type.IlName} {receiverClass.GetQualifiedName()}::{_owner.EscapeIlIdentifier(fieldName.Name)}");
                return fieldType.Type;
            }
            else
            {
                EmitExpression(member.Member);
                return TypeInfo.Object;
            }
        }

        private static bool IsPrimitive(string type)
            => type is "String" or "Integer" or "Real" or "Boolean";



        private TypeInfo EmitConstructorCall(ConstructorCallNode cc)
        {
            // Check if it's actually a method call (not a constructor)
            var method = _class.FindMethod(cc.ClassName, cc.Arguments.Count);
            if (method is not null)
            {
                EmitLoadThis();
                foreach (var argument in cc.Arguments)
                    EmitExpression(argument);
                var returnType = string.IsNullOrWhiteSpace(method.ReturnType)
                    ? TypeInfo.Void
                    : _owner.ResolveType(method.ReturnType);
                var argumentSignature = string.Join(", ", method.Parameters.Select(p => _owner.ResolveType(p.Type).IlName));
                var escapedMethodName = _owner.EscapeIlMethodName(cc.ClassName);
                _sb.AppendLine($"      callvirt instance {returnType.IlName} {_class.GetQualifiedName()}::{escapedMethodName}({argumentSignature})");
                return returnType;
            }
            
            // Handle Array[T]
            if (cc.ClassName.StartsWith("Array[") && cc.ClassName.EndsWith("]"))
            {
                var elementType = cc.ClassName.Substring(6, cc.ClassName.Length - 7);
                if (cc.Arguments.Count > 0)
                {
                    EmitExpression(cc.Arguments[0]); // array length
                    var ilType = elementType switch
                    {
                        "Integer" => "int32",
                        "Real" => "float64",
                        "Boolean" => "int32",
                        "String" => "string",
                        _ => $"class {_owner._options.Namespace}.{elementType}"
                    };
                    _sb.AppendLine($"      newarr {ilType}");
                    return new TypeInfo(cc.ClassName, $"{ilType}[]", false);
                }
            }
            
            // Handle List[T]
            if (cc.ClassName.StartsWith("List[") && cc.ClassName.EndsWith("]"))
            {
                var elementType = cc.ClassName.Substring(5, cc.ClassName.Length - 6);
                var ilType = elementType switch
                {
                    "Integer" => "int32",
                    "Real" => "float64",
                    "Boolean" => "int32",
                    "String" => "string",
                    _ => $"class {_owner._options.Namespace}.{elementType}"
                };
                _sb.AppendLine($"      newobj instance void class [mscorlib]System.Collections.Generic.List`1<{ilType}>::.ctor()");
                return new TypeInfo(cc.ClassName, $"class [mscorlib]System.Collections.Generic.List`1<{ilType}>", false);
            }
            
            // Для примитивных типов
            if (cc.ClassName == "Integer")
            {
                if (cc.Arguments.Count > 0)
                    return EmitExpression(cc.Arguments[0]);
                _sb.AppendLine("      ldc.i4 0");
                return TypeInfo.Int32;
            }
            if (cc.ClassName == "String")
            {
                if (cc.Arguments.Count > 0)
                    return EmitExpression(cc.Arguments[0]);
                _sb.AppendLine("      ldstr \"\"");
                return TypeInfo.String;
            }
            if (cc.ClassName == "Real")
            {
                if (cc.Arguments.Count > 0)
                    return EmitExpression(cc.Arguments[0]);
                _sb.AppendLine("      ldc.r8 0.0");
                return TypeInfo.Float64;
            }
            if (cc.ClassName == "Boolean")
            {
                if (cc.Arguments.Count > 0)
                    return EmitExpression(cc.Arguments[0]);
                _sb.AppendLine("      ldc.i4 0");
                return TypeInfo.Boolean;
            }
            
            // Для пользовательских классов
            foreach (var arg in cc.Arguments)
                EmitExpression(arg);
            
            if (_owner.TryResolveClass(cc.ClassName, out var cls))
            {
                var ctor = cls.Constructors.FirstOrDefault(c => c.Parameters.Count == cc.Arguments.Count);
                var paramTypes = ctor is null ? "" : string.Join(", ", ctor.Parameters.Select(p => _owner.ResolveType(p.Type).IlName));
                var qualifiedName = cls.GetQualifiedName();
                _sb.AppendLine($"      newobj instance void {qualifiedName}::.ctor({paramTypes})");
                return new TypeInfo(cc.ClassName, $"class {qualifiedName}", false);
            }
            
            return TypeInfo.Object;
        }

        private TypeInfo EmitCall(CallNode call)
        {
            if (call.Target is IdentifierNode methodName)
            {
                var method = _class.FindMethod(methodName.Name, call.Arguments.Count);
                if (method is not null)
                {
                    EmitLoadThis();
                    foreach (var argument in call.Arguments)
                        EmitExpression(argument);
                    var returnType = string.IsNullOrWhiteSpace(method.ReturnType)
                        ? TypeInfo.Void
                        : _owner.ResolveType(method.ReturnType);
                    var argumentSignature = string.Join(", ", method.Parameters.Select(p => _owner.ResolveType(p.Type).IlName));
                    var escapedMethodName = _owner.EscapeIlMethodName(methodName.Name);
                    _sb.AppendLine($"      callvirt instance {returnType.IlName} {_class.GetQualifiedName()}::{escapedMethodName}({argumentSignature})");
                    return returnType;
                }
            }
            
            foreach (var argument in call.Arguments)
                EmitExpression(argument);
            _sb.AppendLine("      pop");
            return TypeInfo.Object;
        }

        private string NewLabel(string prefix) => $"IL_{prefix}_{_labelCounter++:D4}";

        private void EmitLoadThis() => _sb.AppendLine("      ldarg.0");

        private void EmitLoadArgument(int index)
        {
            _sb.AppendLine(index switch
            {
                0 => "      ldarg.0",
                1 => "      ldarg.1",
                2 => "      ldarg.2",
                3 => "      ldarg.3",
                _ => $"      ldarg.s {index}"
            });
        }

        private void EmitStoreArgument(int index)
        {
            _sb.AppendLine(index switch
            {
                0 => "      starg.s 0",
                1 => "      starg.s 1",
                2 => "      starg.s 2",
                3 => "      starg.s 3",
                _ => $"      starg.s {index}"
            });
        }

        private sealed record LocalInfo(string Name, TypeInfo Type, int Slot);
        private sealed record ParameterInfo(string Name, TypeInfo Type, int Index);
    }
}

