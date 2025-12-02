.assembly extern mscorlib { }
.assembly arithmetic_operations
{
}
.module arithmetic_operations.exe
.class public auto ansi beforefieldinit UserProgram.Calculator
         extends [mscorlib]System.Object
{
  .method public hidebysig specialname rtspecialname instance void .ctor() cil managed
  {
    .maxstack 8
      .locals init (int32 V_0, int32 V_1, float64 V_2, float64 V_3, int32 V_4, int32 V_5, int32 V_6, int32 V_7, int32 V_8, float64 V_9, float64 V_10, float64 V_11, float64 V_12, float64 V_13, float64 V_14, int32 V_15, int32 V_16, int32 V_17, float64 V_18, int32 V_19)
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldc.i4 10
      stloc V_0
      ldc.i4 5
      stloc V_1
      ldc.r8 3.14
      stloc V_2
      ldc.r8 2.5
      stloc V_3
      ldloc V_0
      ldloc V_1
      add
      stloc V_4
      ldloc V_0
      ldloc V_1
      sub
      stloc V_5
      ldloc V_0
      ldloc V_1
      mul
      stloc V_6
      ldloc V_0
      ldloc V_1
      div
      stloc V_7
      ldloc V_0
      ldloc V_1
      rem
      stloc V_8
      ldloc V_2
      ldloc V_3
      add
      stloc V_9
      ldloc V_2
      ldloc V_3
      sub
      stloc V_10
      ldloc V_2
      ldloc V_3
      mul
      stloc V_11
      ldloc V_2
      ldloc V_3
      div
      stloc V_12
      ldloc V_0
      conv.r8
      ldloc V_2
      add
      stloc V_13
      ldloc V_2
      ldloc V_1
      conv.r8
      mul
      stloc V_14
      ldloc V_0
      ldloc V_1
      cgt
      stloc V_15
      ldloc V_0
      ldloc V_1
      ceq
      stloc V_16
      ldloc V_0
      ldloc V_1
      clt
      stloc V_17
      ldloc V_0
      conv.r8
      stloc V_18
      ldloc V_18
      conv.i4
      stloc V_19
      ldloc V_4
      call void [mscorlib]System.Console::WriteLine(int32)
      ldloc V_5
      call void [mscorlib]System.Console::WriteLine(int32)
      ldloc V_6
      call void [mscorlib]System.Console::WriteLine(int32)
      ldloc V_7
      call void [mscorlib]System.Console::WriteLine(int32)
      ldloc V_8
      call void [mscorlib]System.Console::WriteLine(int32)
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.ArrayOperations
         extends [mscorlib]System.Object
{
  .method public hidebysig specialname rtspecialname instance void .ctor() cil managed
  {
    .maxstack 8
      .locals init (int32[] V_0, int32 V_1, int32 V_2)
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldc.i4 5
      newarr int32
      stloc V_0
      ldc.i4 0
      stloc V_1
    IL_WHILE_START_0000:
      ldloc V_1
      ldloc V_0
      ldlen
      conv.i4
      clt
      brfalse IL_WHILE_END_0001
      ldloc V_0
      ldloc V_1
      ldloc V_1
      ldloc V_1
      mul
      stelem int32
      ldloc V_1
      ldc.i4 1
      add
      stloc V_1
      br IL_WHILE_START_0000
    IL_WHILE_END_0001:
      ldc.i4 0
      stloc V_2
      ldc.i4 0
      stloc V_1
    IL_WHILE_START_0002:
      ldloc V_1
      ldloc V_0
      ldlen
      conv.i4
      clt
      brfalse IL_WHILE_END_0003
      ldloc V_2
      ldloc V_0
      ldloc V_1
      ldelem int32
      add
      stloc V_2
      ldloc V_1
      ldc.i4 1
      add
      stloc V_1
      br IL_WHILE_START_0002
    IL_WHILE_END_0003:
      ldloc V_2
      call void [mscorlib]System.Console::WriteLine(int32)
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.TestArithmetic
         extends [mscorlib]System.Object
{
  .method public hidebysig specialname rtspecialname instance void .ctor() cil managed
  {
    .maxstack 8
      .locals init (class UserProgram.Calculator V_0, class UserProgram.ArrayOperations V_1)
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      newobj instance void UserProgram.Calculator::.ctor()
      stloc V_0
      newobj instance void UserProgram.ArrayOperations::.ctor()
      stloc V_1
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
    ldc.i4 1
    bne.un IL_ARG_ERROR_0001
    newobj instance void UserProgram.Calculator::.ctor()
    pop
    br IL_EXIT_0000
  IL_Calculator_DONE:
  IL_ArrayOperations_CHECK:
    ldloc V_0
    ldstr "ArrayOperations"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_ArrayOperations_MATCH
    br.s IL_ArrayOperations_DONE
  IL_ArrayOperations_MATCH:
    ldarg.0
    ldlen
    conv.i4
    ldc.i4 1
    bne.un IL_ARG_ERROR_0001
    newobj instance void UserProgram.ArrayOperations::.ctor()
    pop
    br IL_EXIT_0000
  IL_ArrayOperations_DONE:
  IL_TestArithmetic_CHECK:
    ldloc V_0
    ldstr "TestArithmetic"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_TestArithmetic_MATCH
    br.s IL_TestArithmetic_DONE
  IL_TestArithmetic_MATCH:
    ldarg.0
    ldlen
    conv.i4
    ldc.i4 1
    bne.un IL_ARG_ERROR_0001
    newobj instance void UserProgram.TestArithmetic::.ctor()
    pop
    br IL_EXIT_0000
  IL_TestArithmetic_DONE:
    ldstr "Error: Class not found"
    call void [mscorlib]System.Console::WriteLine(string)
    ret
  IL_ARG_ERROR_0001:
    ldstr "Error: Class not found or invalid number of arguments"
    call void [mscorlib]System.Console::WriteLine(string)
    ret
  IL_NO_ARGS:
    ldstr "Error: No class name specified and no 'Main' class found"
    call void [mscorlib]System.Console::WriteLine(string)
    ret
  IL_EXIT_0000:
    ret
  }
}
