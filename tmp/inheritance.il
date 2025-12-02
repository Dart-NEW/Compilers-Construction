.assembly extern mscorlib { }
.assembly inheritance
{
}
.module inheritance.exe
.class public auto ansi beforefieldinit UserProgram.Animal
         extends [mscorlib]System.Object
{
  .field public string name
  .field public int32 age
  .method public hidebysig specialname rtspecialname instance void .ctor(string n, int32 a) cil managed
  {
    .maxstack 8
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldarg.0
      ldstr ""
      stfld string UserProgram.Animal::name
      ldarg.0
      ldc.i4 0
      stfld int32 UserProgram.Animal::age
      ldarg.0
      ldarg.1
      stfld string UserProgram.Animal::name
      ldarg.0
      ldarg.2
      stfld int32 UserProgram.Animal::age
      ret
  }
  .method public hidebysig instance string getName() cil managed
  {
    .maxstack 8
      ldarg.0
      ldfld string UserProgram.Animal::name
      ret
  }
  .method public hidebysig instance int32 getAge() cil managed
  {
    .maxstack 8
      ldarg.0
      ldfld int32 UserProgram.Animal::age
      ret
  }
  .method public hidebysig instance string speak() cil managed
  {
    .maxstack 8
      .locals init (string V_0)
      ldstr "Animal sound"
      stloc V_0
      ldloc V_0
      call void [mscorlib]System.Console::WriteLine(string)
      ldloc V_0
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.Dog
         extends UserProgram.Animal
{
  .field public string breed
  .method public hidebysig specialname rtspecialname instance void .ctor(string n, int32 a, string b) cil managed
  {
    .maxstack 8
      ldarg.0
      ldarg.1
      ldarg.2
      call instance void UserProgram.Animal::.ctor(string, int32)
      ldarg.0
      ldstr ""
      stfld string UserProgram.Dog::breed
      ldarg.0
      ldarg.1
      stfld string UserProgram.Dog::name
      ldarg.0
      ldarg.2
      stfld int32 UserProgram.Dog::age
      ldarg.0
      ldarg.3
      stfld string UserProgram.Dog::breed
      ret
  }
  .method public hidebysig instance string getBreed() cil managed
  {
    .maxstack 8
      ldarg.0
      ldfld string UserProgram.Dog::breed
      ret
  }
  .method public hidebysig instance string speak() cil managed
  {
    .maxstack 8
      .locals init (string V_0)
      ldstr "Woof!"
      stloc V_0
      ldloc V_0
      call void [mscorlib]System.Console::WriteLine(string)
      ldloc V_0
      ret
  }
  }
.class public auto ansi beforefieldinit UserProgram.TestInheritance
         extends [mscorlib]System.Object
{
  .method public hidebysig specialname rtspecialname instance void .ctor() cil managed
  {
    .maxstack 8
      .locals init (class UserProgram.Animal V_0, class UserProgram.Dog V_1, string V_2, string V_3, string V_4)
      ldarg.0
      call instance void [mscorlib]System.Object::.ctor()
      ldstr "Generic"
      ldc.i4 5
      newobj instance void UserProgram.Animal::.ctor(string, int32)
      stloc V_0
      ldstr "Buddy"
      ldc.i4 3
      ldstr "Golden Retriever"
      newobj instance void UserProgram.Dog::.ctor(string, int32, string)
      stloc V_1
      ldloc V_0
      callvirt instance string UserProgram.Animal::speak()
      stloc V_2
      ldloc V_1
      callvirt instance string UserProgram.Dog::speak()
      stloc V_3
      ldloc V_1
      callvirt instance string UserProgram.Dog::getBreed()
      stloc V_4
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
  IL_Animal_CHECK:
    ldloc V_0
    ldstr "Animal"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_Animal_MATCH
    br.s IL_Animal_DONE
  IL_Animal_MATCH:
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
    newobj instance void UserProgram.Animal::.ctor(string, int32)
    pop
    br IL_EXIT_0000
  IL_Animal_DONE:
  IL_Dog_CHECK:
    ldloc V_0
    ldstr "Dog"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_Dog_MATCH
    br.s IL_Dog_DONE
  IL_Dog_MATCH:
    ldarg.0
    ldlen
    conv.i4
    ldc.i4 4
    bne.un IL_ARG_ERROR_0001
    ldarg.0
    ldc.i4 1
    ldelem.ref
    ldarg.0
    ldc.i4 2
    ldelem.ref
    call int32 [mscorlib]System.Int32::Parse(string)
    ldarg.0
    ldc.i4 3
    ldelem.ref
    newobj instance void UserProgram.Dog::.ctor(string, int32, string)
    pop
    br IL_EXIT_0000
  IL_Dog_DONE:
  IL_TestInheritance_CHECK:
    ldloc V_0
    ldstr "TestInheritance"
    call bool [mscorlib]System.String::op_Equality(string, string)
    brtrue.s IL_TestInheritance_MATCH
    br.s IL_TestInheritance_DONE
  IL_TestInheritance_MATCH:
    ldarg.0
    ldlen
    conv.i4
    ldc.i4 1
    bne.un IL_ARG_ERROR_0001
    newobj instance void UserProgram.TestInheritance::.ctor()
    pop
    br IL_EXIT_0000
  IL_TestInheritance_DONE:
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
