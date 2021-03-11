namespace Jumble.Tests.Integration

open Jumble.Rename
open NUnit.Framework

[<TestFixture>]
type E2EPrivateAndPublic() =
    inherit E2ETestsBase()

    [<Test>]
    member this.``Enum values in attributes are not renamed``() =
        // Mono.Cecil does not currently support that.
        let t = this.Setup.ObfuscatedLibB.GetType("LibB.CustomAttributeEnum");
        Assert.IsNotNull(t)
        Assert.IsTrue(t.Fields |> Seq.exists (fun f -> f.Name = "EnumValue"), "Enum values used in attribute ctors can't be renamed");

    [<Test>]
    member this.``Attributes are not renamed``() =
        // Mono.Cecil does not currently support changes in custom attributes (since these are not regular type and member references).
        Assert.IsNotNull(this.Setup.ObfuscatedLibB.GetType("LibB.CustomAttribute"));

    [<Test>]
    member this.``Attribute properties are not renamed``() =
        // Mono.Cecil does not currently support changes in custom attributes (since these are not regular type and member references).
        let t = this.Setup.ObfuscatedLibB.GetType("LibB.CustomAttribute")
        Assert.IsTrue(t.Properties |> Seq.exists(fun p -> p.Name = "IntProperty"))

    [<Test>]
    member this.``Attribute property defined in ancestor is not renamed``() =
        // Mono.Cecil does not currently support changes in custom attributes (since these are not regular type and member references).
        let t = this.Setup.ObfuscatedLibB.GetType(NameGenerators.testingTypeGen "LibB.CustomBaseAttribute")
        Assert.IsTrue(t.Properties |> Seq.exists(fun p -> p.Name = "BaseIntProperty"))

    [<Test>]
    member this.``Explicit impl names overriding public interface are properly renamed too``() =
        let cls = this.Setup.ObfuscatedLibA.GetType(NameGenerators.testingTypeGen "LibA.CInheritsPublicInterfaceExplicitImpl")
        let method = cls.Methods |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "System.Collections.IEnumerable.GetEnumerator")
        Assert.IsNotNull(method)


    [<Test>]
    member this.``Interface generic parameters are renamed``() =
        let original = this.FindTypeByDescriptionAttribute this.Setup.OriginalLibA "LibA.IGeneric<TValue>"
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IGeneric<TValue>"

        Assert.AreNotEqual(original.GenericParameters.[0].Name, obfuscated.GenericParameters.[0].Name)
        
    [<Test>]
    member this.``Interface public static method is renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IWithStaticMember"
        let method = obfuscated.Methods |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "StaticMethod")
        Assert.IsNotNull(method)
        
    [<Test>]
    member this.``Interface public static field is renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IWithStaticMember"
        let method = obfuscated.Fields |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "StaticField")
        Assert.IsNotNull(method)
        
    [<Test>]
    member this.``Generic interface name has generic suffix indicating number of generic parameters``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "LibA.IGeneric<TValue>"
        Assert.IsTrue(obfuscated.Name.EndsWith("`1"), sprintf "Expected name ending with `1, got: %s" obfuscated.Name)

    [<Test>]
    member this.``Nested classes are properly nested after obfuscation``() =
        let obfuscatedParent = this.Setup.ObfuscatedLibA.GetType(NameGenerators.testingTypeGen "LibA.CNestedParent")
        Assert.IsTrue(obfuscatedParent.NestedTypes |> Seq.exists (fun t -> t.Name = NameGenerators.testingTypeGen "CNestedChild"))