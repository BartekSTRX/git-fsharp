namespace GitLib

type Sha1 = Sha1 of string

module Hash = 
    open System
    open System.Security.Cryptography
    open System.Globalization

    let fromByteArray = 
        Array.collect (fun (x: byte) -> 
        [| 
            (x &&& byte(0b11110000)) >>> 4; 
            x &&& byte(0b00001111) 
        |])
        >> Array.map (sprintf "%x")
        >> (fun chars -> String.Join("", chars))
        >> Sha1

    let toByteArray (Sha1 hash) : byte[] = 
        hash.ToCharArray()
        |> Array.map (fun (c:char) -> Byte.Parse(c.ToString(), NumberStyles.HexNumber))
        |> Array.chunkBySize 2
        |> Array.map (fun chunk -> (chunk.[0] <<< 4 |> byte) + chunk.[1])

    let sha1Bytes (object: byte array) = 
        let sha = new SHA1CryptoServiceProvider()
        object |> sha.ComputeHash |> fromByteArray

    let split (Sha1 hash) = 
        hash.Substring(0, 2), hash.Substring(2)
    
    let private isHex (c: char) = "1234567890abcdefABCDEF".Contains(c)

    let parse (str: string) =
        if str.Length <> 40 then
            Error "hash lenght different than 40 characters"
        elif str.ToCharArray() |> Array.forall isHex |> not then
            Error "hash contains non-hexadecimal character"
        else 
            Ok (Sha1 str)

