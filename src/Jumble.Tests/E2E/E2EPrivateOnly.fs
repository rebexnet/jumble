﻿namespace Jumble.Tests.Integration

open Jumble.Rename
open Mono.Cecil
open NUnit.Framework

[<TestFixture>]
type E2EPrivateOnly() =
    inherit E2ETestsBase()

    [<Test>]
    member this.``PrivateOnly does not rename public enum members``() =
        let originalEnum = this.Setup.OriginalLibC.GetType("LibC.PublicEnum")
        let obfuscatedEnum = this.Setup.ObfuscatedLibC.GetType("LibC.PublicEnum")

        Assert.That(obfuscatedEnum.Fields |> Seq.map _.Name, Is.EquivalentTo(originalEnum.Fields |> Seq.map _.Name))


    [<Test>]
    member this.``PrivateOnly does rename internal enum members``() =
        let originalEnum = this.Setup.OriginalLibC.GetType("LibC.InternalEnum")
        let obfuscatedEnum = this.Setup.ObfuscatedLibC.GetType(NameGenerators.testingTypeGen "LibC.InternalEnum")
        let fltSpecial (xs:FieldDefinition seq) = xs |> Seq.filter (fun f -> f.Attributes.HasFlag(FieldAttributes.SpecialName) = false)

        Assert.That(
            obfuscatedEnum.Fields |> fltSpecial |> Seq.map _.Name,
            Is.EquivalentTo(originalEnum.Fields |> fltSpecial |> Seq.map (fun f -> NameGenerators.testingMethodGenF f.Name)))


    [<Test>]
    member this.``PrivateOnly does not rename internal enums (and members) which are converted to string``() =
        let originalEnum = this.Setup.OriginalLibC.GetType("LibC.InternalEnumWithToStringConversion")
        let obfuscatedEnum = this.Setup.ObfuscatedLibC.GetType("LibC.InternalEnumWithToStringConversion")

        Assert.That(obfuscatedEnum.Fields |> Seq.map _.Name, Is.EquivalentTo(originalEnum.Fields |> Seq.map _.Name))


    [<Test>]
    member this.``When PrivateOnly public events are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        Assert.That(obfuscated.Events |> Seq.exists (fun e -> e.Name = "PublicEvent"))

    [<Test>]
    member this.``When PrivateOnly private events are renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        Assert.That(obfuscated.Events |> Seq.exists (fun e -> e.Name = "PrivateEvent") |> not)
        Assert.That(obfuscated.Events |> Seq.exists (fun e -> e.Name = NameGenerators.testingMethodGenF "PrivateEvent"))

    [<Test>]
    member this.``When PrivateOnly protected internal method is not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = "ProtectedInternalMethod")
        Assert.That(obfMethod, Is.Not.Null)

    [<Test>]
    member this.``When PrivateOnly protected internal property is not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")
        let obfProperty = obfuscated.Properties |> Seq.find (fun m -> m.Name = "ProtectedInternalProperty")
        Assert.That(obfProperty, Is.Not.Null)
    
    [<Test>]
    member this.``Protected members are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        Assert.That(obfuscated.Fields |> Seq.exists (fun e -> e.Name = "ProtectedField"), "Field was renamed")
        Assert.That(obfuscated.Methods |> Seq.exists (fun e -> e.Name = "ProtectedMethod"), "Method was renamed")
        Assert.That(obfuscated.Properties |> Seq.exists (fun e -> e.Name = "ProtectedProperty"), "Property was renamed")
        Assert.That(obfuscated.Events |> Seq.exists (fun e -> e.Name = "ProtectedEvent"), "Event was renamed")

    [<Test>]
    member this.``Public member parameters are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = "PublicMethod")
        let methodParameter = obfMethod.Parameters[0]
        Assert.That(methodParameter.Name, Is.EqualTo "parameter1")


    [<Test>]
    member this.``Protected member parameters are not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = "ProtectedMethod")
        let methodParameter = obfMethod.Parameters[0]
        Assert.That(methodParameter.Name, Is.EqualTo "parameter1")

    [<Test>]
    member this.``Private member parameters are renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.CWithDifferentVisibilities")

        let obfMethod = obfuscated.Methods |> Seq.find (fun m -> m.Name = NameGenerators.testingMethodGenF "PrivateMethod")
        let methodParameter = obfMethod.Parameters[0]
        Assert.That(methodParameter.Name, Is.EqualTo "p0")
        
    [<Test>]
    member this.``Nested public class of public class is not renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType("LibC.COuterClass")
        Assert.That(obfuscated, Is.Not.Null)
        let inner = obfuscated.NestedTypes |> Seq.find (_.Name.EndsWith("CInnerClass"))
        Assert.That(inner, Is.Not.Null)
        
    [<Test>]
    member this.``Nested public class of internal class is renamed``() =
        let obfuscated = this.Setup.ObfuscatedLibC.GetType(NameGenerators.testingTypeGen "LibC.COuterInternalClass")
        Assert.That(obfuscated, Is.Not.Null)
        let inner = obfuscated.NestedTypes |> Seq.find (fun t -> t.Name.EndsWith("CInnerClass" + this.Setup.Obfuscated.Suffix))
        Assert.That(inner, Is.Not.Null)

    [<Test>]
    member this.``Interface public static method is NOT renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibC "LibC.IWithStaticMember"
        let method = obfuscated.Methods |> Seq.find (fun m -> m.Name = "StaticMethod")
        Assert.That(method, Is.Not.Null)
        
    [<Test>]
    member this.``Interface public static field is NOT renamed``() =
        let obfuscated = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibC "LibC.IWithStaticMember"
        let method = obfuscated.Fields |> Seq.find (fun m -> m.Name = "StaticField")
        Assert.That(method, Is.Not.Null)