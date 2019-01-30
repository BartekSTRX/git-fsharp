module Utils
    let rec traverse<'a, 'e> (results: Result<'a, 'e> list): Result<'a list, 'e> = 
        match results with
        | (Ok item) :: tail -> 
            let tailResult = traverse tail
            match tailResult with
            | Ok items -> Ok (item::items)
            | Error reason -> Error reason
        | (Error reason) :: tail -> Error reason
        | [] -> Ok []


    type ResultBuilder() =
        member __.Bind(a, binder) = Result.bind binder a

        member __.Return(a) = Ok a

    let result = new ResultBuilder()