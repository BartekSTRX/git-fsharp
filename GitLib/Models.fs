module GitLib.Models

type ObjectType = Blob | Tree | Commit

module ObjectTypes = 
    let toStr = function
        | Blob -> "blob"
        | Tree -> "tree"
        | Commit -> "commit"

    let fromStr = function
        | "blob" -> Result.Ok Blob
        | "tree" -> Result.Ok Tree
        | "commit" -> Result.Ok Commit
        | _ -> Result.Error "incorrect git object type"


type GitObject = {
    ObjectType: ObjectType
    Object: byte array
}
with 
    member this.Size = this.Object.Length

module GitObjects =
    open System
    open ObjectTypes
    open System.Text

    let wrap { ObjectType = objectType; Object = object }: byte array =
        let headerEncoded = 
            sprintf "%s %i%c" (toStr objectType) (object.Length) (Convert.ToChar(0))
            |> Encoding.UTF8.GetBytes
        Array.concat [headerEncoded; object]
    
    let unwrap (content: byte array): Result<GitObject, string> =
        let spaceChar = Convert.ToByte(' ')
        let firstSpace = Array.IndexOf(content, spaceChar)
        let nullChar = Convert.ToChar(0) |> Convert.ToByte
        let firstNull = Array.IndexOf(content, nullChar)
        
        let encodedType = 
            Array.sub content 0 firstSpace
            |> Encoding.UTF8.GetString
            |> fromStr
        let encodedLength = 
            Array.sub content (firstSpace + 1)  (firstNull - (firstSpace + 1))
            |> Encoding.UTF8.GetString
            |> Int32.Parse

        let actualLength = content.Length - (firstNull + 1)

        let object = Array.sub content (firstNull + 1) (actualLength)

        if encodedLength <> actualLength then 
            Error "incorrect git object length"
        else
            encodedType
            |> Result.map (fun t -> { ObjectType = t; Object = object })


type Sha1 = Sha1 of string

module Hash = 
    open System
    open System.Security.Cryptography
    open GitObjects

    let private toStr = 
        Array.collect (fun (x: byte) -> 
        [| 
            (x &&& byte(0b11110000)) >>> 4; 
            x &&& byte(0b00001111) 
        |])
        >> Array.map (sprintf "%x")
        >> (fun chars -> String.Join("", chars))

    let sha1Bytes (object: byte array) = 
        let sha = new SHA1CryptoServiceProvider()
        object |> sha.ComputeHash |> toStr |> Sha1
        
    let sha1Object = wrap >> sha1Bytes

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

type ObjectFormat = Deflated | Wrapped | Content

type ReadObject = {
    Format: ObjectFormat
    Content: byte[]
}

module Storage =
    open System.IO
    open System.IO.Compression
    open Ionic.Zlib

    let private readObjectLoose (rootDir: string) (objectId: Sha1) =
        let id1, id2 = Hash.split objectId
        let objectPath = Path.Combine(rootDir, ".git", "objects", id1, id2)
        let fileStream = File.OpenRead(objectPath)
        (fileStream, Deflated)
        
    // no support for packfiles for now
    let readObject (rootDir: string) (objectId: Sha1) (*(format: ObjectFormat)*) = 
        let (fileStream, format) = readObjectLoose rootDir objectId
        
        use gzipStream = new ZlibStream(fileStream, CompressionMode.Decompress)
        use memoryStream = new MemoryStream()
        gzipStream.CopyTo(memoryStream)

        { Format = format; Content = memoryStream.ToArray() }

    let writeObjectContent (rootDir: string) (objectId: Sha1) (content: byte array) : unit =
        let id1, id2 = Hash.split objectId
        let path = Path.Combine(rootDir, ".git", "objects", id1, id2)

        use memoryStream = new MemoryStream(content)
        use gzipStream = new ZlibStream(memoryStream, CompressionMode.Compress)

        let fileInfo = new FileInfo(path)
        fileInfo.Directory.Create()
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)
