namespace Jumble.Tests.Integration

open Jumble.Rename
open NUnit.Framework

[<TestFixture>]
type E2EPrivateOnly() =
    inherit E2ETestsBase()

    [<Test>]
    member this.``PrivateOnly does not rename public enum members``() =
        let originalEnum = this.Setup.OriginalLibC.GetType("LibC.PublicEnum")
        let obfuscatedEnum = this.Setup.ObfuscatedLibC.GetType("LibC.PublicEnum")
        CollectionAssert.AreEquivalent(originalEnum.Fields |> Seq.map (fun f -> f.Name), obfuscatedEnum.Fields |> Seq.map (fun f -> f.Name))

    [<Test>]
    member this.``When PrivateOnly public events are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        Assert.IsTrue(obfuscated.Events |> Seq.exists (fun e -> e.Name = "PublicEvent"))

    [<Test>]
    member this.``When PrivateOnly private events are renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        Assert.IsFalse(obfuscated.Events |> Seq.exists (fun e -> e.Name = "PrivateEvent"))
        Assert.IsTrue(obfuscated.Events |> Seq.exists (fun e -> e.Name = NameGenerators.testingMethodGenF "PrivateEvent"))

    [<Test>]
    member this.``When PrivateOnly protected internal method is not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = "ProtectedInternalMethod")
        Assert.IsNotNull(obfMethod)

    [<Test>]
    member this.``When PrivateOnly protected internal property is not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        let obfProperty = obfuscated.Properties |> Seq.find (fun m -> m.Name = "ProtectedInternalProperty")
        Assert.IsNotNull(obfProperty)
    
    [<Test>]
    member this.``Protected members are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        Assert.IsTrue(obfuscated.Fields |> Seq.exists (fun e -> e.Name = "ProtectedField"), "Field was renamed")
        Assert.IsTrue(obfuscated.Methods |> Seq.exists (fun e -> e.Name = "ProtectedMethod"), "Method was renamed")
        Assert.IsTrue(obfuscated.Properties |> Seq.exists (fun e -> e.Name = "ProtectedProperty"), "Property was renamed")
        Assert.IsTrue(obfuscated.Events |> Seq.exists (fun e -> e.Name = "ProtectedEvent"), "Event was renamed")

    [<Test>]
    member this.``Public member parameters are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = "PublicMethod")
        let methodParameter = obfMethod.Parameters.[0]
        Assert.AreEqual("parameter1", methodParameter.Name)


    [<Test>]
    member this.``Protected member parameters are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = "ProtectedMethod")
        let methodParameter = obfMethod.Parameters.[0]
        Assert.AreEqual("parameter1", methodParameter.Name)

    [<Test>]
    member this.``Private member parameters are renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "PrivateMethod")
        let methodParameter = obfMethod.Parameters.[0]
        Assert.AreEqual("p0", methodParameter.Name)
        
    [<Test>]
    member this.``Nested public class of public class is not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.COuterClass")
        Assert.IsNotNull(obfuscated)
        let inner = obfuscated.NestedTypes |> Seq.find (fun t -> t.Name.EndsWith("CInnerClass"))
        Assert.IsNotNull(inner)
        
    [<Test>]
    member this.``Nested public class of internal class is renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType(NameGenerators.testingTypeGen "LibC.COuterInternalClass")
        Assert.IsNotNull(obfuscated)
        let inner = obfuscated.NestedTypes |> Seq.find (fun t -> t.Name.EndsWith("CInnerClass" + this.Setup.Obfuscated.Suffix))
        Assert.IsNotNull(inner)

    [<Test>]
    member this.``Interface public static method is NOT renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibC "LibC.IWithStaticMember"
        let method = obfuscated.Methods |> Seq.find (fun m -> m.Name = "StaticMethod")
        Assert.IsNotNull(method)
        
    [<Test>]
    member this.``Interface public static field is NOT renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibC "LibC.IWithStaticMember"
        let method = obfuscated.Fields |> Seq.find (fun m -> m.Name = "StaticField")
        Assert.IsNotNull(method)