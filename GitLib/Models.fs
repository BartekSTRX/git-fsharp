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

    let fromByteArray = 
        Array.collect (fun (x: byte) -> 
        [| 
            (x &&& byte(0b11110000)) >>> 4; 
            x &&& byte(0b00001111) 
        |])
        >> Array.map (sprintf "%x")
        >> (fun chars -> String.Join("", chars))
        >> Sha1

    let sha1Bytes (object: byte array) = 
        let sha = new SHA1CryptoServiceProvider()
        object |> sha.ComputeHash |> fromByteArray
        
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


type IndexEntryStage = int

type IndexEntryFlags = {
    AssumeValid: bool
    Extended: bool
    Stage: IndexEntryStage
    NameLength: int
}

type GitIndexEntry = {
    Ctime: byte[]
    Mtime: byte[]
    Device: uint32
    Inode: uint32
    Mode: uint32
    UserId: uint32
    GroupId: uint32
    FileSize: uint32
    Hash: Sha1
    Flags: IndexEntryFlags
    RelativeFilePath: string
}

type GitIndex = {
    Entries: GitIndexEntry list
}

module GitIndexes = 
    open System
    open System.IO
    open System.Text
    
    // TODO
    let private parseFlags (arr: byte[]): IndexEntryFlags =
        // 1 assume valid
        // 1 extended - must be 0 in version 2
        // 2 stage
        // 12 name length if less than FFF or FFF if longer
        {
            AssumeValid = true
            Extended = true
            Stage = 2 //true, true
            NameLength = 42
        }
    
    let private readUInt32LE (reader: BinaryReader): uint32 =
        let bytes = reader.ReadBytes(4) |> Array.rev
        BitConverter.ToUInt32(bytes, 0)

    let private readFileName (reader: BinaryReader): string =
        let mutable arr = []
        let mutable c: byte = reader.ReadByte()

        while c <> 0uy do
            arr <- c :: arr
            c <- reader.ReadByte()
        
        // read null bytes used for padding (padded to multiply of 8)
        // ugly and hacky solution
        while c = 0uy do
            c <- reader.ReadByte()
        reader.BaseStream.Position <- reader.BaseStream.Position - 1L;

        arr |> Array.ofList |> Array.rev |> Encoding.UTF8.GetString

    let private parseEntry (reader: BinaryReader) = 
        {
            Ctime = reader.ReadBytes(8)
            Mtime = reader.ReadBytes(8)
            Device = readUInt32LE reader
            Inode = readUInt32LE reader
            Mode = readUInt32LE reader // TODO validate, correct is 100644 for normal blobs
            // 100755 executable, 120000 symbolic link
            UserId = readUInt32LE reader
            GroupId = readUInt32LE reader
            FileSize = readUInt32LE reader
            Hash = (reader.ReadBytes(20) |> Hash.fromByteArray)
            Flags = (reader.ReadBytes(2) |> parseFlags)
            // possibly 2 more bytes here in version 3
            RelativeFilePath = readFileName reader
        }

    let parse (input: byte[]): Result<GitIndex, string> = 
        use reader = new BinaryReader(new MemoryStream(input))

        // TODO validate
        let signature = reader.ReadBytes(4) |> Encoding.UTF8.GetString
        let version = readUInt32LE reader
        let entriesCount = readUInt32LE reader |> int

        let entries = [
            for i in 0..(entriesCount - 1) do
                let entry = parseEntry reader
                yield (entry)
        ]

        Ok { Entries = entries }


type ObjectFormat = Deflated | Wrapped | Content

type ReadObject = {
    Format: ObjectFormat
    Content: byte[]
}

module Storage =
    open System.IO
    open System.IO.Compression
    open Ionic.Zlib

    let private readDecompressed (stream: FileStream) : byte[] =
        use gzipStream = new ZlibStream(stream, CompressionMode.Decompress)
        use memoryStream = new MemoryStream()
        gzipStream.CopyTo(memoryStream)
        memoryStream.ToArray()

    let private readObjectLoose (rootDir: string) (objectId: Sha1) =
        let id1, id2 = Hash.split objectId
        let objectPath = Path.Combine(rootDir, ".git", "objects", id1, id2)
        let fileStream = File.OpenRead(objectPath)
        (fileStream, Deflated)
        
    // no support for packfiles for now
    let readObject (rootDir: string) (objectId: Sha1) (*(format: ObjectFormat)*) = 
        let (fileStream, format) = readObjectLoose rootDir objectId
        let content = readDecompressed fileStream
        { Format = format; Content = content }

    let readIndex (rootDir: string) =
        use fileStream = Path.Combine(rootDir, ".git", "index") |> File.OpenRead
        use memoryStream = new MemoryStream()
        fileStream.CopyTo(memoryStream)
        memoryStream.ToArray() |> GitIndexes.parse

    let writeObjectContent (rootDir: string) (objectId: Sha1) (content: byte array) : unit =
        let id1, id2 = Hash.split objectId
        let path = Path.Combine(rootDir, ".git", "objects", id1, id2)

        use memoryStream = new MemoryStream(content)
        use gzipStream = new ZlibStream(memoryStream, CompressionMode.Compress)

        let fileInfo = new FileInfo(path)
        fileInfo.Directory.Create()
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)
