.assembly extern mscorlib { }
.assembly test_args
{
}
.module test_args.exe
.class public auto ansi beforefieldinit UserProgram.Person
         extends [mscorlib]System.Object
{
  .field public string name
  .field public int32 age
  .method public hidebysig specialname rtspecialname instance void .ctor(string personName, int32 personAge) cil managed
  {
    .maxstack 8
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldarg.0
      ldarg.1
      stfld string UserProgram.Person::name
      ldarg.0
      ldarg.2
      stfld int32 UserProgram.Person::age
      ldarg.0
      callvirt instance void UserProgram.Person::greet()
      ret
  }
  .method public hidebysig instance void greet() cil managed
  {
    .maxstack 8
      ldarg.0
      ldfld string UserProgram.Person::name
      call void [mscorlib]System.Console::WriteLine(string)
      ldarg.0
      ldfld int32 UserProgram.Person::age
      call void [mscorlib]System.Console::WriteLine(int32)
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.Calculator
         extends [mscorlib]System.Object
{
  .field public int32 a
  .field public int32 b
  .method public hidebysig specialname rtspecialname instance void .ctor(int32 x, int32 y) cil managed
  {
    .maxstack 8
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldarg.0
      ldarg.1
      stfld int32 UserProgram.Calculator::a
      ldarg.0
      ldarg.2
      stfld int32 UserProgram.Calculator::b
      ldarg.0
      callvirt instance void UserProgram.Calculator::'add'()
      ret
  }
  .method public hidebysig instance void 'add'() cil managed
  {
    .maxstack 8
      .locals init (int32 V_0)
      ldarg.0
      ldfld int32 UserProgram.Calculator::a
      ldarg.0
      ldfld int32 UserProgram.Calculator::b
      add
      stloc V_0
      ldloc V_0
      call void [mscorlib]System.Console::WriteLine(int32)
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.Main
         extends [mscorlib]System.Object
{
  .method public hidebysig specialname rtspecialname instance void .ctor() cil managed
  {
    .maxstack 8
      .locals init (string V_0)
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldstr "Hello from Main!"
      stloc V_0
      ldloc V_0
      call void [mscorlib]System.Console::WriteLine(string)
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.Program
         extends [mscorlib]System.Object
{
  .method public hidebysig static void Main(string[] args) cil managed
  {
    .entrypoint
    .maxstack 16
    .locals init (string V_0)
    ldarg.0
    brfalse IL_NO_ARGS
    ldarg.0
    ldlen
    conv.i4
    ldc.i4.0
    ble IL_NO_ARGS
    ldarg.0
    ldc.i4.0
    ldelem.ref
    stloc V_0
  IL_Person_CHECK:
    ldloc V_0
    ldstr "Person"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_Person_MATCH
    br.s IL_Person_DONE
  IL_Person_MATCH:
    ldarg.0
    ldlen
    conv.i4
    ldc.i4 3
    bne.un IL_ARG_ERROR_0001
    ldarg.0
    ldc.i4 1
    ldelem.ref
    ldarg.0
    ldc.i4 2
    ldelem.ref
    call int32 [mscorlib]System.Int32::Parse(string)
    newobj instance void UserProgram.Person::.ctor(string, int32)
    pop
    br IL_EXIT_0000
  IL_Person_DONE:
  IL_Calculator_CHECK:
    ldloc V_0
    ldstr "Calculator"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_Calculator_MATCH
    br.s IL_Calculator_DONE
  IL_Calculator_MATCH:
    ldarg.0
    ldlen
    conv.i4
    ldc.i4 3
    bne.un IL_ARG_ERROR_0001
    ldarg.0
    ldc.i4 1
    ldelem.ref
    call int32 [mscorlib]System.Int32::Parse(string)
    ldarg.0
    ldc.i4 2
    ldelem.ref
    call int32 [mscorlib]System.Int32::Parse(string)
    newobj instance void UserProgram.Calculator::.ctor(int32, int32)
    pop
    br IL_EXIT_0000
  IL_Calculator_DONE:
  IL_Main_CHECK:
    ldloc V_0
    ldstr "Main"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_Main_MATCH
    br.s IL_Main_DONE
  IL_Main_MATCH:
    ldarg.0
    ldlen
    conv.i4
    ldc.i4 1
    bne.un IL_ARG_ERROR_0001
    newobj instance void UserProgram.Main::.ctor()
    pop
    br IL_EXIT_0000
  IL_Main_DONE:
    ldstr "Error: Class not found"
    call void [mscorlib]System.Console::WriteLine(string)
    ret
  IL_ARG_ERROR_0001:
    ldstr "Error: Class not found or invalid number of arguments"
    call void [mscorlib]System.Console::WriteLine(string)
    ret
  IL_NO_ARGS:
    newobj instance void UserProgram.Main::.ctor()
    pop
    br IL_EXIT_0000
  IL_EXIT_0000:
    ret
  }
}
