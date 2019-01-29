namespace GitLib

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
    open System
    open System.IO
    open System.Text
    
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

        let totalLength = 62 + arr.Length + 1 + paddingSize
        if totalLength % 8 = 0 then
            arr |> Array.ofList |> Array.rev |> Encoding.UTF8.GetString |> Ok
        else 
            Error "index entry has to be padded to the length of multiply of 8"

    let private serializeFileName (name: string) = 
        Array.append (name |> Encoding.UTF8.GetBytes) [| 0uy |]

    let private parseEntry (reader: BinaryReader): Result<GitIndexEntry, string> = 
        let entry = {
            Ctime = reader.ReadBytes(8)
            Mtime = reader.ReadBytes(8)
            Device = reader.ReadBytes(4) |> readUInt32LE
            Inode = reader.ReadBytes(4) |> readUInt32LE
            Mode = reader.ReadBytes(4) |> readUInt32LE // TODO validate, correct is 100644 for normal blobs
            // 100755 executable, 120000 symbolic link
            UserId = reader.ReadBytes(4) |> readUInt32LE
            GroupId = reader.ReadBytes(4) |> readUInt32LE
            FileSize = reader.ReadBytes(4) |> readUInt32LE
            Hash = (reader.ReadBytes(20) |> Hash.fromByteArray)
            Flags = (reader.ReadBytes(2) |> parseFlags)
            // possibly 2 more bytes here in version 3
            RelativeFilePath = ""
        }
        let pathResult = readFileName reader
        match pathResult with 
        | Ok path -> Ok { entry with RelativeFilePath = path }
        | Error reason -> Error reason

    let private serializeEntry (entry: GitIndexEntry): byte[] = 
        let entryLength = 62 + entry.RelativeFilePath.Length + 1
        let nullPaddingLength = 8 - (entryLength % 8)
        [|
            entry.Ctime
            entry.Mtime
            entry.Device |> serializeUInt32LE
            entry.Inode |> serializeUInt32LE
            entry.Mode |> serializeUInt32LE
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
