namespace GitLib

open System
open System.IO
open System.Text  


type IndexEntryMode = 
    | Mode100644 // normal file
    | Mode100755 // executable
    | Mode120000 // symbolic link

module IndexEntryModes =
    let parse = function
        | "100644" -> Ok Mode100644
        | "100755" -> Ok Mode100755
        | "120000" -> Ok Mode120000
        | _ -> Error "unsupported mode"

    let toStr = function
        | Mode100644 -> "100644"
        | Mode100755 -> "100755"
        | Mode120000 -> "120000"


type MergeStage = uint16

type IndexEntryFlags = {
    AssumeValid: bool
    Extended: bool
    Stage: MergeStage
    NameLength: int
}

type GitIndexEntry = {
    Ctime: byte[]
    Mtime: byte[]
    Device: uint32
    Inode: uint32
    Mode: IndexEntryMode
    UserId: uint32
    GroupId: uint32
    FileSize: uint32
    Hash: Sha1
    Flags: IndexEntryFlags
    RelativeFilePath: string
}
with 
    static member Create (fileInfo: FileInfo) mode objectId path = 
        let dateToBytes (date: DateTime) = 
            date.ToBinary() |> BitConverter.GetBytes
        {
            Ctime = fileInfo.CreationTimeUtc |> dateToBytes
            Mtime = fileInfo.LastAccessTimeUtc |> dateToBytes
            Device = 0u
            Inode = 0u
            Mode = mode // todo validate
            UserId = 0u
            GroupId = 0u
            FileSize = fileInfo.Length |> uint32
            Hash = objectId
            Flags = { 
                AssumeValid = false
                Extended = false
                Stage = 0us
                NameLength = fileInfo.Name |> Encoding.UTF8.GetByteCount
            }
            RelativeFilePath = path
        }

type GitIndex = {
    Entries: GitIndexEntry list
}
with 
    static member Empty = { Entries = [] }

(* Git index entry layout
 - - - - - - - - - - - - - - - - 
|    ctime      |    mtime      |
 - - - - - - - - - - - - - - - - 
|  dev  | inode |  mode |  uid  |
 - - - - - - - - - - - - - - - - 
| gid   | size  |     sha1      |
 - - - - - - - - - - - - - - - - 
|        sha1       |fl | path\0|
 - - - - - - - - - - - - - - - - 
*)

module GitIndexes = 
    
    let private parseFlags (arr: byte[]): IndexEntryFlags =
        let bits = (arr.[0] |> uint16) + (arr.[1] |> uint16)
        let assumeValid = 
            bits &&& 0b1000000000000000us
        let extended = 
            bits &&& 0b0100000000000000us
        let stage = 
            bits &&& 0b0011000000000000us
        let nameLength =
            bits &&& 0b0000111111111111us
        {
            AssumeValid = assumeValid <> 0us
            Extended = extended <> 0us
            Stage = ((stage >>> 12) |> uint16)
            NameLength = (nameLength |> int)
        }

    let private serializeFlags (entry: GitIndexEntry): byte[] =
        let { 
            Flags = { AssumeValid = assumeValid; Stage = stage }
            RelativeFilePath = path
            } = entry
        let length = min (Encoding.UTF8.GetByteCount(path)) 0xFFF |> uint16
        
        [|
            (if assumeValid then 0b1000000000000000us else 0us)
            0us // extended is 0 in version 2
            stage &&& 0b0011000000000000us
            length &&& 0b0000111111111111us
        |] 
        |> Array.reduce (|||) 
        |> BitConverter.GetBytes
        
    let private readUInt32LE (bytes: byte[]): uint32 =
        let reversed = bytes |> Array.rev
        BitConverter.ToUInt32(reversed, 0)

    let private serializeUInt32LE (item: uint32): byte[] =
        BitConverter.GetBytes(item) |> Array.rev

    let private readFileName (reader: BinaryReader): Result<string, string> =
        let mutable arr = []
        let mutable c: byte = reader.ReadByte()

        while c <> 0uy do
            arr <- c :: arr
            c <- reader.ReadByte()
        
        // read null bytes used for padding (padded to multiply of 8)
        // ugly and hacky solution
        let mutable paddingSize = 0
        while c = 0uy do
            c <- reader.ReadByte()
            paddingSize <- paddingSize + 1
        reader.BaseStream.Position <- reader.BaseStream.Position - 1L;

        let totalLength = 62 + arr.Length + paddingSize
        if totalLength % 8 = 0 then
            arr |> Array.ofList |> Array.rev |> Encoding.UTF8.GetString |> Ok
        else 
            Error "index entry has to be padded to the length of multiply of 8"

    let private serializeFileName (name: string) = 
        Array.append (name |> Encoding.UTF8.GetBytes) [| 0uy |]
    
    let private parseMode (bytes: byte[]) = 
        match (readUInt32LE bytes) with 
        | 33188u -> Ok Mode100644
        | 33261u -> Ok Mode100755
        | 40960u -> Ok Mode120000
        | _ -> Error "unsupported file mode"

    let private serializeMode mode =
        match mode with
        | Mode100644 -> 33188u
        | Mode100755 -> 33261u
        | Mode120000 -> 40960u
        |> serializeUInt32LE

    let private parseEntry (reader: BinaryReader): Result<GitIndexEntry, string> = 
        let ctime = reader.ReadBytes(8)
        let mtime = reader.ReadBytes(8)
        let device = reader.ReadBytes(4) |> readUInt32LE
        let inode = reader.ReadBytes(4) |> readUInt32LE
        let modeResult = reader.ReadBytes(4) |> parseMode
        let uid = reader.ReadBytes(4) |> readUInt32LE
        let gid = reader.ReadBytes(4) |> readUInt32LE
        let fileSize = reader.ReadBytes(4) |> readUInt32LE
        let hash = reader.ReadBytes(20) |> Hash.fromByteArray
        let flags = reader.ReadBytes(2) |> parseFlags
        let pathResult = readFileName reader

        match modeResult, pathResult with 
        | Ok mode, Ok path -> 
            Ok {
                Ctime = ctime
                Mtime = mtime
                Device = device
                Inode = inode
                Mode = mode
                UserId = uid
                GroupId = gid
                FileSize = fileSize
                Hash = hash
                Flags = flags
                // possibly 2 more bytes here in version 3
                RelativeFilePath = path
            }
        | Error reason, _ -> Error reason
        | _, Error reason -> Error reason

    let private serializeEntry (entry: GitIndexEntry): byte[] = 
        let entryLength = 62 + entry.RelativeFilePath.Length + 1
        let nullPaddingLength = 8 - (entryLength % 8)
        [|
            entry.Ctime
            entry.Mtime
            entry.Device |> serializeUInt32LE
            entry.Inode |> serializeUInt32LE
            entry.Mode |> serializeMode
            entry.UserId |> serializeUInt32LE
            entry.GroupId |> serializeUInt32LE
            entry.FileSize |> serializeUInt32LE
            entry.Hash |> Hash.toByteArray
            entry |> serializeFlags
            entry.RelativeFilePath |> serializeFileName
            Array.create nullPaddingLength 0uy
        |] |> Array.concat

    let parse (input: byte[]): Result<GitIndex, string> = 
        let parseSignature (reader: BinaryReader): Result<string, string> = 
            let signature = reader.ReadBytes(4) |> Encoding.UTF8.GetString
            match signature with
            | "DIRC" -> Ok signature
            | _ -> Error "index file has to start with DIRC signature"
        let parseVersion (reader: BinaryReader): Result<uint32, string> = 
            let version = reader.ReadBytes(4) |> readUInt32LE
            match version with
            | 2u -> Ok version
            | _ -> Error "the only supported version is 2"
        let parseEntriesCount (reader: BinaryReader): int = 
            reader.ReadBytes(4) |> readUInt32LE |> int

        use reader = new BinaryReader(new MemoryStream(input))

        (parseSignature reader)
        |> Result.bind (fun _ -> parseVersion reader)
        |> Result.map (fun _ -> parseEntriesCount reader)
        |> Result.bind (fun entriesCount -> 
            [
                for _ in 0..(entriesCount - 1) do
                    yield (parseEntry reader)
            ] |> Utils.traverse)
        |> (function 
            | Ok entries -> Ok { Entries = entries } 
            | Error reason -> Error reason)        

    let serialize ({ Entries = entries }: GitIndex) : byte[] =
        use outputStream = new MemoryStream()
        use writer = new BinaryWriter(outputStream)

        seq {
            yield ("DIRC" |> Encoding.UTF8.GetBytes)
            yield (serializeUInt32LE 2u)
            yield (entries.Length |> uint32 |> serializeUInt32LE)
            yield! (entries|> Seq.ofList |> Seq.map serializeEntry)
        } |> Seq.iter writer.Write

        outputStream.ToArray()
