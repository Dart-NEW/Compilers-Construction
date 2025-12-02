.assembly extern mscorlib { }
.assembly polymorphism
{
}
.module polymorphism.exe
.class public auto ansi beforefieldinit UserProgram.Point
         extends [mscorlib]System.Object
{
  .field public int32 x
  .field public int32 y
  .method public hidebysig specialname rtspecialname instance void .ctor(int32 px, int32 py) cil managed
  {
    .maxstack 8
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldarg.0
      ldarg.1
      stfld int32 UserProgram.Point::x
      ldarg.0
      ldarg.2
      stfld int32 UserProgram.Point::y
      ret
  }
  .method public hidebysig instance void Print() cil managed
  {
    .maxstack 8
      ldarg.0
      ldfld int32 UserProgram.Point::x
      call void [mscorlib]System.Console::WriteLine(int32)
      ldarg.0
      ldfld int32 UserProgram.Point::y
      call void [mscorlib]System.Console::WriteLine(int32)
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.Point2
         extends UserProgram.Point
{
  .field public int32 x
  .field public int32 y
  .method public hidebysig specialname rtspecialname instance void .ctor(int32 px, int32 py) cil managed
  {
    .maxstack 8
      ldarg.0
      ldarg.1
      ldarg.2
      call instance void UserProgram.Point::.ctor(int32, int32)
      ldarg.0
      ldarg.1
      stfld int32 UserProgram.Point2::x
      ldarg.0
      ldarg.2
      stfld int32 UserProgram.Point2::y
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.Main
         extends [mscorlib]System.Object
{
  .method public hidebysig specialname rtspecialname instance void .ctor() cil managed
  {
    .maxstack 8
      .locals init (class UserProgram.Point V_0, class UserProgram.Point2 V_1)
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldc.i4 1
      ldc.i4 2
      newobj instance void UserProgram.Point::.ctor(int32, int32)
      stloc V_0
      ldarg.0
      ldloc V_0
      callvirt instance void UserProgram.Main::PrintPoint(class UserProgram.Point)
      ldc.i4 10
      ldc.i4 20
      newobj instance void UserProgram.Point2::.ctor(int32, int32)
      stloc V_1
      ldarg.0
      ldloc V_1
      callvirt instance void UserProgram.Main::PrintPoint(class UserProgram.Point)
      ret
  }
  .method public hidebysig instance void PrintPoint(class UserProgram.Point p) cil managed
  {
    .maxstack 8
      ldarg.1
      callvirt instance void UserProgram.Point::Print()
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
  IL_Point_CHECK:
    ldloc V_0
    ldstr "Point"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_Point_MATCH
    br.s IL_Point_DONE
  IL_Point_MATCH:
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
    newobj instance void UserProgram.Point::.ctor(int32, int32)
    pop
    br IL_EXIT_0000
  IL_Point_DONE:
  IL_Point2_CHECK:
    ldloc V_0
    ldstr "Point2"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_Point2_MATCH
    br.s IL_Point2_DONE
  IL_Point2_MATCH:
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
    newobj instance void UserProgram.Point2::.ctor(int32, int32)
    pop
    br IL_EXIT_0000
  IL_Point2_DONE:
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
