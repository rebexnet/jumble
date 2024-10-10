namespace Jumble.Tests.Integration

open Jumble
open NUnit.Framework

[<TestFixture>]
type E2ESigningTests() =
    inherit E2ETestsBase()

    [<Test>]
    member this.``When signing key is specified then assembly is signed``() =
        let libAName = this.Setup.ObfuscatedLibA.Assembly.Name

        Assert.That(libAName.HasPublicKey)

        Assert.That(libAName.PublicKey, Is.EqualTo(this.LibABSigningKey.PublicKeyBlob))
        Assert.That(libAName.PublicKeyToken, Is.EqualTo(this.LibABSigningKey.PublicKeyToken))

    [<Test>]
    member this.``When signing key is not specified then assembly is not signed``() =
        let libCName = this.Setup.ObfuscatedLibC.Assembly.Name

        Assert.That(libCName.HasPublicKey, Is.False)

        Assert.That(libCName.PublicKey, Is.Empty)
        Assert.That(libCName.PublicKeyToken, Is.Empty)

    [<Test>]
    member this.``Updates public keys in references``() =
        let libAPublicKey = this.Setup.ObfuscatedLibA.Assembly.Name.PublicKey

        Assert.That(libAPublicKey, Is.Not.Empty)

        let asmRef = this.Setup.ObfuscatedLibB.Assembly.MainModule.AssemblyReferences
                     |> Seq.find (fun a -> a.Name = "LibA")

        Assert.That(asmRef.PublicKey, Is.EqualTo(libAPublicKey))

    [<Test>]
    member this.``Updates InternalsVisibleTo public keys``() =
        let libBName = this.Setup.ObfuscatedLibB.Assembly.Name

        let intVis = this.Setup.ObfuscatedLibA.Assembly.CustomAttributes
                     |> Seq.filter (fun a -> a.AttributeType.Name = "InternalsVisibleToAttribute")
                     |> Seq.exactlyOne

        let intVisAttrValue = intVis.ConstructorArguments[0].Value :?> string
        let expected = $"%s{libBName.Name}, PublicKey=%s{SigningKey.publicKeyString libBName.PublicKey}"

        Assert.That(intVisAttrValue, Is.EqualTo(expected))