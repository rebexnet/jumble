namespace Jumble.Tests.Integration

open Mono.Cecil
open System.Reflection
open Jumble
open Jumble.Rename
open Mono.Cecil.Rocks
open NUnit.Framework
open LibA.Annotated

type ObfuscationStatus =
    {
        NestedPrivateRenamed: bool
        NestedPublicRenamed: bool
        PublicMethodRenamed: bool
        PrivateMethodRenamed: bool
        PrivateFieldRenamed: bool
        TypeNameRenamed: bool
    }

module ObfuscationStatus =
    let assertEquals (expected:ObfuscationStatus) (result:ObfuscationStatus) =
        Assert.That(result, Is.EqualTo(expected))

[<TestFixture>]
type E2EAnnotationTests() =
    inherit E2ETestsBase()

    member private this.getStatus (tn:string) =
       let fType (ts:TypeDefinition seq) tnameTrue tnameFalse =
           if ts |> Seq.exists (fun t -> t.Name = tnameTrue) then true else
           if ts |> Seq.exists (fun t -> t.Name = tnameFalse) then false else
           failwith "Neither type name found"

       let isRenamedType tn (ts:TypeDefinition seq) =
           fType ts (NameGenerators.testingTypeGen tn) tn

       let t = this.Setup.ObfuscatedLibA.GetType(tn)
       {
            TypeNameRenamed = t.Name.Contains(NameGenerators.testNameSuffix)
            PublicMethodRenamed = TypeDefinition.existsMethod t (NameGenerators.testingMethodGenF "PublicMethod")
            PrivateMethodRenamed = TypeDefinition.existsMethod t (NameGenerators.testingMethodGenF "PrivateMethod")
            PrivateFieldRenamed = TypeDefinition.existsField t (NameGenerators.testingMethodGenF "privateField")
            NestedPrivateRenamed = isRenamedType "CNestedPrivate" t.NestedTypes
            NestedPublicRenamed = isRenamedType "CNestedPublic" t.NestedTypes
        }

    [<Test>]
    member this.``ObfuscationAttribute default check`` () =
        let attr = ObfuscationAttribute()
        Assert.That(attr.ApplyToMembers)
        Assert.That(attr.Exclude)
        Assert.That(attr.Feature, Is.EqualTo "all")

    [<Test>]
    member this.``[Obfuscation] excludes all public members (renames only private members)`` () =
        let expected =
            {
                PublicMethodRenamed = false
                PrivateMethodRenamed = true
                PrivateFieldRenamed = true
                TypeNameRenamed = false
                NestedPrivateRenamed = true
                NestedPublicRenamed = false
            }
        let result = this.getStatus typedefof<CDefault>.FullName
        ObfuscationStatus.assertEquals expected result

    [<Test>]
    member this.``[Obfuscation(ApplyToMembers = false)] only excludes type name (renames all members)`` () =
        let expected =
            {
                PublicMethodRenamed = true
                PrivateMethodRenamed = true
                PrivateFieldRenamed = true
                TypeNameRenamed = false
                NestedPrivateRenamed = true
                NestedPublicRenamed = true

            }
        let result = this.getStatus typedefof<CApplyToMembersFalse>.FullName
        ObfuscationStatus.assertEquals expected result

    [<Test>]
    member this.``[Obfuscation(Features = "excludePrivate")] excludes everything (renames nothing)`` () =
        let expected =
            {
                PublicMethodRenamed = false
                PrivateMethodRenamed = false
                PrivateFieldRenamed = false
                TypeNameRenamed = false
                NestedPrivateRenamed = false
                NestedPublicRenamed = false
            }

        let result = this.getStatus typedefof<CApplyToPrivate>.FullName
        ObfuscationStatus.assertEquals expected result

    [<Test>]
    member this.``[Obfuscation(Features = "applyToChildren")] excludes all public members and affects children (renames only private members)`` () =
        let expected =
            {
                PublicMethodRenamed = false
                PrivateMethodRenamed = true
                PrivateFieldRenamed = true
                TypeNameRenamed = false
                NestedPrivateRenamed = true
                NestedPublicRenamed = false
        }

        let result = this.getStatus typedefof<CApplyToChildrenChild>.FullName
        ObfuscationStatus.assertEquals expected result

    [<Test>]
    member this.``[Obfuscation(Exclude = false)] excludes nothing (renames everything)`` () =
        let expected =
            {
                PublicMethodRenamed = true
                PrivateMethodRenamed = true
                PrivateFieldRenamed = true
                TypeNameRenamed = true
                NestedPrivateRenamed = true
                NestedPublicRenamed = true
            }

        let result = this.getStatus (NameGenerators.testingTypeGen typedefof<CExcludeFalse>.FullName)
        ObfuscationStatus.assertEquals expected result

    [<Test>]
    member this.``[Obfuscation] on a method / constructor parameter prevents renaming`` () =
        let t = this.FindTypeByDescriptionAttribute this.Setup.ObfuscatedLibA "CWithAttributedParameters"

        let test (method: MethodDefinition) =
            let matchOne name = Has.One.Matches<ParameterDefinition>(fun p -> p.Name = name)
            let matchNone name = Has.None.Matches<ParameterDefinition>(fun p -> p.Name = name)

            Assert.That(method.Parameters, matchOne "value1")
            Assert.That(method.Parameters, matchNone "value2")

        t.GetConstructors() |> Seq.exactlyOne |> test
        t.GetMethods() |> Seq.filter _.IsPublic |> Seq.exactlyOne |> test
        t.GetMethods() |> Seq.filter _.IsPrivate |> Seq.exactlyOne |> test