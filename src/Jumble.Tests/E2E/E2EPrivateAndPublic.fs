namespace Jumble.Tests.Integration

open Jumble
open Jumble.Rename
open NUnit.Framework

[<TestFixture>]
type E2EPrivateAndPublic() =
    inherit E2ETestsBase()

    [<Test>]
    member this.``Enum values in attributes are not renamed``() =
        // Mono.Cecil does not currently support that.
        let t = this.Setup.ObfuscatedLibB.GetType("LibB.CustomAttributeEnum");
        Assert.That(t, Is.Not.Null)
        Assert.That(t.Fields |> Seq.exists (fun f -> f.Name = "EnumValue"), "Enum values used in attribute ctors can't be renamed");

    [<Test>]
    member this.``Attributes are renamed``() =
        Assert.That(this.Setup.ObfuscatedLibB.GetType("LibB.CustomAttribute"), Is.Null);

    [<Test>]
    member this.``Attribute properties are not renamed``() =
        // Mono.Cecil does not currently support changes in custom attributes (since these are not regular type and member references).
        let t = this.Setup.ObfuscatedLibB.GetType(NameGenerators.testingTypeGen "LibB.CustomAttribute")
        Assert.That(t.Properties |> Seq.exists(fun p -> p.Name = "IntProperty"))

    [<Test>]
    member this.``Attribute property defined in ancestor is not renamed``() =
        // Mono.Cecil does not currently support changes in custom attributes (since these are not regular type and member references).
        let t = this.Setup.ObfuscatedLibB.GetType(NameGenerators.testingTypeGen "LibB.CustomBaseAttribute")
        Assert.That(t.Properties |> Seq.exists(fun p -> p.Name = "BaseIntProperty"))

    [<Test>]
    member this.``Explicit impl names overriding public interface are properly renamed too``() =
        let cls = this.Setup.ObfuscatedLibA.GetType(NameGenerators.testingTypeGen "LibA.CInheritsPublicInterfaceExplicitImpl")
        let method = cls.Methods |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "System.Collections.IEnumerable.GetEnumerator")
        Assert.That(method, Is.Not.Null)

    [<Test>]
    member this.``Interface generic parameters are renamed``() =
        let original = this.FindTypeByDescriptionAttribute this.Setup.OriginalLibA "LibA.IGeneric<TValue>"
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IGeneric<TValue>"

        obfuscated.GenericParameters[0].Name |> assert_not_equal original.GenericParameters[0].Name
        
    [<Test>]
    member this.``Interface public static method is renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IWithStaticMember"
        let method = obfuscated.Methods |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "StaticMethod")
        method |> assert_notnull

        
    [<Test>]
    member this.``Interface public static field is renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IWithStaticMember"
        let method = obfuscated.Fields |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "StaticField")
        Assert.That(method, Is.Not.Null)
        
    [<Test>]
    member this.``Generic interface name has generic suffix indicating number of generic parameters``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IGeneric<TValue>"
        Assert.That(obfuscated.Name.EndsWith("`1"), $"Expected name ending with `1, got: %s{obfuscated.Name}")

    [<Test>]
    member this.``Nested classes are properly nested after obfuscation``() =
        let obfuscatedParent = this.Setup.ObfuscatedLibA.GetType(NameGenerators.testingTypeGen "LibA.CNestedParent")
        Assert.That(obfuscatedParent.NestedTypes |> Seq.exists (fun t -> t.Name = NameGenerators.testingTypeGen "CNestedChild"))

    [<Ignore("Not applicable for testing name generator.")>]
    [<Test>]
    member this.``Namespace-less class is moved to namespace``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "NamespacelessClass"
        Assert.That(obfuscated.Namespace, Is.Not.Empty)

    [<Test>]
    member this.``Constructor parameters are renamed``() =
        let className = typeof<LibA.CWithConstructors>.FullName
        let findCtor (t:Mono.Cecil.TypeDefinition) = t.Methods |> Seq.find (fun m -> m.IsConstructor && m.Parameters.Count = 1)
        let originalCtor = this.FindTypeByDescriptionAttribute this.Setup.OriginalLibA className |> findCtor
        let obfuscatedCtor = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA className |> findCtor

        obfuscatedCtor.Parameters[0].Name |> assert_not_equal originalCtor.Parameters[0].Name

    [<Test>]
    member this.``Constructors are not renamed``() =
        let className = typeof<LibA.CWithConstructors>.FullName
        let findCtor (t:Mono.Cecil.TypeDefinition) = t.Methods |> Seq.find (fun m -> m.IsConstructor && m.Parameters.Count = 1)
        let originalCtor = this.FindTypeByDescriptionAttribute this.Setup.OriginalLibA className |> findCtor
        let obfuscatedCtor = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA className |> findCtor

        obfuscatedCtor.DeclaringType.Name |> assert_not_equal originalCtor.DeclaringType.Name

        // ctor names are actually '.ctor' in IL, not the name of the declaring type as in C#
        obfuscatedCtor.Name |> assert_equal originalCtor.Name

    [<Test>]
    member this.``Implementation of non-obfuscated abstract method is also not obfuscated``() =
        let abstractClassName = typeof<LibA.AbstractMethodImplementation.AbstractClassNoObfuscation>.FullName
        let abstractClass = this.Setup.ObfuscatedLibA.GetType(abstractClassName)
        let className = typeof<LibA.AbstractMethodImplementation.C>.FullName
        let class' = this.Setup.ObfuscatedLibA.GetType(NameGenerators.testingTypeGen className)

        let methodName = nameof Unchecked.defaultof<LibA.AbstractMethodImplementation.C>.Method
        let otherMethodName = nameof Unchecked.defaultof<LibA.AbstractMethodImplementation.C>.OtherMethod

        // sanity test - abstract method name is not obfuscated
        abstractClass.Methods |> Seq.filter (fun m -> m.Name = methodName) |> Seq.length |> assert_equal 1

        // test implementation name is not obfuscated
        class'.Methods |> Seq.filter (fun m -> m.Name = methodName) |> Seq.length |> assert_equal 1

        // other methods are obfuscated
        class'.Methods |> Seq.filter (fun m -> m.Name = otherMethodName) |> Seq.length |> assert_equal 0

    [<Test>]
    member this.``Implementation of non-obfuscated abstract method in a nested class is also not obfuscated``() =
        let abstractClassName = typeof<LibA.AbstractMethodImplementation.AbstractClassNoObfuscation>.FullName
        let abstractClass = this.Setup.ObfuscatedLibA.GetType(abstractClassName)
        let classOuterName = typeof<LibA.AbstractMethodImplementation.COuter>.FullName
        let classOuter = this.Setup.ObfuscatedLibA.GetType(NameGenerators.testingTypeGen classOuterName)
        let classInner = classOuter.NestedTypes |> Seq.exactlyOne

        let methodName = nameof Unchecked.defaultof<LibA.AbstractMethodImplementation.C>.Method
        let otherMethodName = nameof Unchecked.defaultof<LibA.AbstractMethodImplementation.C>.OtherMethod

        // sanity test - abstract method name is not obfuscated
        abstractClass.Methods |> Seq.filter (fun m -> m.Name = methodName) |> Seq.length |> assert_equal 1

        // test implementation name is not obfuscated
        classInner.Methods |> Seq.filter (fun m -> m.Name = methodName) |> Seq.length |> assert_equal 1

        // other methods are obfuscated
        classInner.Methods |> Seq.filter (fun m -> m.Name = otherMethodName) |> Seq.length |> assert_equal 0