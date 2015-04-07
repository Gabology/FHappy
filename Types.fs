module Types
open System

type MaybeBuilder() =
    member x.Bind(m,f) = Option.bind f m
    member x.Return v = Some v
    member x.ReturnFrom o = o
    member x.Zero() = None

let maybe = MaybeBuilder()

type System.String with
    member x.ToKeyValuePair(seperator) =
        let parts = x.Split(seperator)
        if parts.Length < 2 then ("","")
        else (parts.[0],parts.[1])

type System.IO.BufferedStream with
    member x.ReadLine() =
        let rec readLine chars =
            let next = x.ReadByte()
            match next with
            | 10      -> String(Array.ofList chars)
            | 13 | -1 -> readLine chars
            | _       -> readLine (chars @ [char next])
        readLine []