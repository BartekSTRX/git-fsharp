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
    Object: string
}
with 
    member this.Size = this.Object.Length

module GitObjects =
    open System
    open ObjectTypes

    let wrap { ObjectType = objectType; Object = object }: string =
        let header = sprintf "%s %i%c" (toStr objectType) (object.Length) (Convert.ToChar(0))
        let content = sprintf "%s%s" header object
        content
    
    let unwrap (content: string): Result<GitObject, string> =
        let firstSpace = content.IndexOf(' ')
        let nullChar = Convert.ToChar(0)
        let firstNull = content.IndexOf(nullChar)
        
        let objectType = content.Substring(0, firstSpace)
        let objectLength = content.Substring(firstSpace + 1, firstNull - (firstSpace + 1))
        
        let actualLength = content.Length - (firstNull + 1)
        let encodedLength = Int32.Parse(objectLength)

        if encodedLength <> actualLength then 
            Result.Error "incorrect git object length"
        else
            fromStr objectType
            |> Result.map (fun x -> 
                {
                    ObjectType = x
                    Object = content.Substring(firstNull + 1)
                })


type ObjectFormat = Deflated | Wrapped | Content

type ReadObject = {
    Format: ObjectFormat
    Content: string
}

module Storage =
    open System.IO
    open System.IO.Compression
    open Ionic.Zlib

    let private readObjectLoose (gitDir: string) (objectId: string) =
        let id1, id2 = objectId.Substring(0, 2), objectId.Substring(2)
        let objectPath = Path.Combine(gitDir, "objects", id1, id2)
        let fileStream = File.OpenRead(objectPath)
        (fileStream, Deflated)
        
    // no support for packfiles for now
    let readObject (gitDir: string) (objectId: string) (format: ObjectFormat) = 
        let (fileStream, format) = readObjectLoose gitDir objectId
        
        use gzipStream = new ZlibStream(fileStream, CompressionMode.Decompress)
        use reader = new StreamReader(gzipStream)
        { Format = format; Content = reader.ReadToEnd() }


 module Hash = 
    open GitObjects
    open System.Security.Cryptography
    open System.Text

    let sha1Bytes (object: byte array) = 
        let sha = new SHA1CryptoServiceProvider()
        object |> sha.ComputeHash

    let sha1String (object: string) = 
        object |> Encoding.Unicode.GetBytes |> sha1Bytes
        
    let sha1Object = wrap >> sha1String 
