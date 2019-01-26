module GitLib.Models

type ObjectType = Blob | Tree | Commit

module ObjectTypes = 
    let toStr = function
        | Blob -> "blob"
        | Tree -> "tree"
        | Commit -> "commit"

    let fromStr = function
        | "blob" -> Blob
        | "tree" -> Tree
        | "commit" -> Commit
        | _ -> failwith "incorrect git object type"


type GitObject = {
    objectType: ObjectType
    object: string
}

module GitObjects =
    open System
    open ObjectTypes

    let wrap { objectType = objectType; object = object }: string =
        let header = sprintf "%s %i\0" (toStr objectType) (object.Length)
        let content = sprintf "%s%s" header object
        content
    
    let unwrap (content: string): GitObject =
        let firstSpace = content.IndexOf(' ')
        let nullChar: char = Convert.ToChar(0)
        let firstNull = content.IndexOf(nullChar)
        
        let objectType = content.Substring(0, firstSpace)
        let objectLength = content.Substring(firstSpace + 1, firstNull - (firstSpace + 1))
        
        let actualLength = content.Length - (firstNull + 1)
        let encodedLength = Int32.Parse(objectLength)

        //if encodedLength <> actualLength then 
        //    failwith "incorrect git object"
        //else
        {
            objectType = fromStr objectType
            object = content.Substring(firstNull + 1)
        }


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

