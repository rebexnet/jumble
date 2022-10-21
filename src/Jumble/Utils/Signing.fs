namespace Jumble

open System
open System.IO
open Mono.Cecil
open System.Security.Cryptography

type SigningKey = {
    Path: string
    KeyBlob: byte[]
    PublicKeyBlob: byte[]
    PublicKeyToken: byte[]
}
with
    override this.ToString() = sprintf "%s (%s)" this.Path (BitConverter.ToString(this.PublicKeyToken).Replace("-", ""))
    
module rec SigningKey =
    let apply (p:WriterParameters) (key:SigningKey) =
        p.StrongNameKeyBlob <- key.KeyBlob

    // .NET Core does not support System.Reflection.StrongNameKeyPair
    /// Gets full public key
    let private toPublicKey keyBlob =
        use rsa = new RSACryptoServiceProvider()
        rsa.ImportCspBlob(keyBlob)
        let cspBlob = rsa.ExportCspBlob(false)
        let publicKey = Array.zeroCreate<byte> (12 + cspBlob.Length)

        Array.Copy(cspBlob, 0, publicKey, 12, cspBlob.Length);
        publicKey[1] <- 36uy;
        publicKey[4] <- 4uy;
        publicKey[5] <- 128uy;
        publicKey[8] <- byte (cspBlob.Length >>> 0);
        publicKey[9] <- byte (cspBlob.Length >>> 8);
        publicKey[10] <- byte (cspBlob.Length >>> 16);
        publicKey[11] <- byte (cspBlob.Length >>> 24);
        publicKey

    /// Gets the short "fingerprint" of public key, displayed e.g. in full assembly name
    let private publicKeyToken (publicKey:byte[]) =
        let hash = SHA1.HashData(publicKey)
        // last 8 bytes reversed
        hash |> Array.skip 12 |> Array.rev

    let fromSnkFile (path:string) =
        let keyBlob = File.ReadAllBytes(path)

        let publicKey = toPublicKey keyBlob
        {
            Path = path
            KeyBlob = File.ReadAllBytes(path)
            PublicKeyBlob = publicKey
            PublicKeyToken = publicKeyToken publicKey
        }

    let publicKeyString (publicKey:byte[]) =
        BitConverter.ToString(publicKey).Replace("-", "")