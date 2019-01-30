module Tests

open System.Linq
open Xunit
open FsCheck
open FsCheck.Xunit
open GitLib


[<Property>]
let ``ObjectType - toStr and parse`` (object: ObjectType) = 
    let result = object |> ObjectTypes.toStr  |> ObjectTypes.fromStr

    match result with 
    | Ok t -> t = object
    | Error _ -> false


[<Property>]
let ``GitObject - wrap and unwrap`` (object: GitObject) = 
    let result = object |> GitObjects.wrap  |> GitObjects.unwrap

    match result with 
    | Ok { ObjectType = t; Object = content } -> 
        (t = object.ObjectType) && content.SequenceEqual(object.Object)
    | Error _ -> false


let generateByte = 
    Gen.choose(0, 255) |> Gen.map byte
let generateBinaryHash =
    Gen.arrayOfLength 20 generateByte
type ByteArrayArb =
    static member Array() = Arb.fromGen(generateBinaryHash)

[<Property( Arbitrary=[| typeof<ByteArrayArb> |] )>]
let ``Sha1 - deserialize and serialize`` (hash: byte[]) =
    let result = hash |> Hash.fromByteArray |> Hash.toByteArray 

    hash.SequenceEqual(result)


[<Property>]
let ``GitIndex - serialize and parse`` (index: GitIndex) = 
    let result = index |> GitIndexes.serialize |> GitIndexes.parse

    match result with
    | Ok deserializedIndex -> index = deserializedIndex
    | Error _ -> false