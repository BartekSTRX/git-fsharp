﻿namespace GitLib

module Compression = 
    open System.IO
    open System.IO.Compression
    open Ionic.Zlib

    let decompressStream (stream: Stream) : byte[] =
        use gzipStream = new ZlibStream(stream, CompressionMode.Decompress)
        use resultStream = new MemoryStream()
        gzipStream.CopyTo(resultStream)
        resultStream.ToArray()

    let decompressArray (bytes: byte[]) : byte[] =
        use sourceStream = new MemoryStream(bytes)
        decompressStream sourceStream

    let compressToStream (bytes: byte[]) : Stream =
        use memoryStream = new MemoryStream(bytes)
        use gzipStream = new ZlibStream(memoryStream, CompressionMode.Compress)
        gzipStream :> Stream
    

type ObjectFormat = Deflated | Wrapped | Content

type ReadObject = {
    Format: ObjectFormat
    Content: byte[]
}

module Storage =
    open System.IO

    let private readObjectLoose (rootDir: string) (objectId: Sha1) =
        let id1, id2 = Hash.split objectId
        let objectPath = Path.Combine(rootDir, ".git", "objects", id1, id2)
        let fileStream = File.OpenRead(objectPath)
        (fileStream, Deflated)
        
    // no support for packfiles for now
    let readObject (rootDir: string) (objectId: Sha1) (*(format: ObjectFormat)*) = 
        let (fileStream, format) = readObjectLoose rootDir objectId
        let content = Compression.decompressStream fileStream
        { Format = format; Content = content }

    let writeObjectContent (rootDir: string) (objectId: Sha1) (content: byte array) : unit =
        let id1, id2 = Hash.split objectId
        let path = Path.Combine(rootDir, ".git", "objects", id1, id2)

        use gzipStream = Compression.compressToStream content

        let fileInfo = new FileInfo(path)
        fileInfo.Directory.Create()
        use fileStream = File.OpenWrite(fileInfo.FullName)
        gzipStream.CopyTo(fileStream)


    let getIndexPath rootDir = Path.Combine(rootDir, ".git", "index")

    let readIndex (indexPath: string) =
        use fileStream = indexPath |> File.OpenRead
        use memoryStream = new MemoryStream()
        fileStream.CopyTo(memoryStream)
        memoryStream.ToArray() |> GitIndexes.parseIndex

    let writeIndex (rootDir: string) (index: GitIndex) = 
        use fileStream = rootDir |> getIndexPath |> File.OpenWrite
        let bytes = index |> GitIndexes.serializeIndex
        fileStream.Write(bytes, 0, bytes.Length)
