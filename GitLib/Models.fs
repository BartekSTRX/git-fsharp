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
        let headerStr = sprintf "%s %i%c" (toStr objectType) (object.Length) (Convert.ToChar(0))
        let headerEncoded = headerStr |> Encoding.UTF8.GetBytes
        Array.concat [headerEncoded; object]
    
    let unwrap (content: byte array): Result<GitObject, string> =
        let spaceChar = Convert.ToByte(' ')
        let firstSpace = Array.IndexOf(content, spaceChar)
        let nullChar = Convert.ToChar(0) |> Convert.ToByte
        let firstNull = Array.IndexOf(content, nullChar)
        
        let objectType = Array.sub content 0 firstSpace
        let objectLength = Array.sub content (firstSpace + 1)  (firstNull - (firstSpace + 1))
        
        let encodedType = fromStr (Encoding.UTF8.GetString(objectType))
        let encodedLength = Int32.Parse(Encoding.UTF8.GetString(objectLength))

        let actualLength = content.Length - (firstNull + 1)

        if encodedLength <> actualLength then 
            Result.Error "incorrect git object length"
        else
            encodedType
            |> Result.map (fun x -> 
                {
                    ObjectType = x
                    Object = Array.sub content (firstNull + 1) (actualLength)
                })


type ObjectFormat = Deflated | Wrapped | Content

type ReadObject = {
    Format: ObjectFormat
    Content: byte[]
}

module Storage =
    open System.IO
    open System.IO.Compression
    open Ionic.Zlib

    let private splitHash (objectId: string) = 
        objectId.Substring(0, 2), objectId.Substring(2)

    let private readObjectLoose (rootDir: string) (objectId: string) =
        let id1, id2 = splitHash objectId
        let objectPath = Path.Combine(rootDir, ".git", "objects", id1, id2)
        let fileStream = File.OpenRead(objectPath)
        (fileStream, Deflated)
        
    // no support for packfiles for now
    let readObject (rootDir: string) (objectId: string) (*(format: ObjectFormat)*) = 
        let (fileStream, format) = readObjectLoose rootDir objectId
        
        use gzipStream = new ZlibStream(fileStream, CompressionMode.Decompress)
        use memoryStream = new MemoryStream()
        gzipStream.CopyTo(memoryStream)

        { Format = format; Content = memoryStream.ToArray() }

    let writeObjectContent (rootDir: string) (objectId: string) (content: byte array) =
        let id1, id2 = splitHash objectId
        let path = Path.Combine(rootDir, ".git", "objects", id1, id2)

        use memoryStream = new MemoryStream(content)
        use gzipStream = new ZlibStream(memoryStream, CompressionMode.Compress)

        let fileInfo = new FileInfo(path)
        fileInfo.Directory.Create()
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)


 module Hash = 
    open GitObjects
    open System.Security.Cryptography

    let sha1Bytes (object: byte array) : byte array = 
        let sha = new SHA1CryptoServiceProvider()
        object |> sha.ComputeHash
        
    let sha1Object = wrap >> sha1Bytes
