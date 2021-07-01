namespace Jumble.Tests.Integration

open Jumble
open NUnit.Framework

[<TestFixture>]
type E2ESigningTests() =
    inherit E2ETestsBase()

    [<Test>]
    member this.``When signing key is specified then assembly is signed``() =
        let libAName = this.Setup.ObfuscatedLibA.Assembly.Name

        Assert.IsTrue(libAName.HasPublicKey)
        CollectionAssert.AreEqual(this.LibABSigningKey.PublicKeyBlob, libAName.PublicKey)
        CollectionAssert.AreEqual(this.LibABSigningKey.PublicKeyToken, libAName.PublicKeyToken)

    [<Test>]
    member this.``When signing key is not specified then assembly is not signed``() =
        let libCName = this.Setup.ObfuscatedLibC.Assembly.Name

        Assert.IsFalse(libCName.HasPublicKey)
        CollectionAssert.IsEmpty(libCName.PublicKey)
        CollectionAssert.IsEmpty(libCName.PublicKeyToken)

    [<Test>]
    member this.``Updates public keys in references``() =
        let libAPublicKey = this.Setup.ObfuscatedLibA.Assembly.Name.PublicKey
        CollectionAssert.IsNotEmpty(libAPublicKey)

        let asmRef = this.Setup.ObfuscatedLibB.Assembly.MainModule.AssemblyReferences
                     |> Seq.find (fun a -> a.Name = "LibA")


        CollectionAssert.AreEqual(libAPublicKey, asmRef.PublicKey)

    [<Test>]
    member this.``Updates InternalsVisibleTo public keys``() =
        let libBName = this.Setup.ObfuscatedLibB.Assembly.Name

        let intVis = this.Setup.ObfuscatedLibA.Assembly.CustomAttributes
                     |> Seq.filter (fun a -> a.AttributeType.Name = "InternalsVisibleToAttribute")
                     |> Seq.exactlyOne

        let intVisAttrValue = intVis.ConstructorArguments.[0].Value :?> string
        let expected = $"%s{libBName.Name}, PublicKey=%s{SigningKey.publicKeyString libBName.PublicKey}"

        Assert.AreEqual(expected, intVisAttrValue)