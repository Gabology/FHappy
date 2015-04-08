module Http
open System
open System.IO
open System.Threading
open System.Net
open System.Net.Sockets
open Microsoft.FSharp.Reflection
open System.Globalization
open System.Text
open System.Text.RegularExpressions
open Types

[<Literal>]
let serverProtocol = "HTTP/1.1"

type StatusCode = 
    | OK = 200                 | NoContent = 204
    | MovedPermanently = 302   | NotModified = 304
    | Forbidden = 403          | NotFound  = 404
    | MethodNotAllowed = 405   | ServerError = 500
    | ServiceUnavailable = 503 | HttpVersionNotSupported = 505

type HttpMethod =
    | GET   | POST
    | HEAD  | OPTIONS
    | PUT   | DELETE
    | TRACE | CONNECT
    static member Parse(str:string) =
        let methods = FSharpType.GetUnionCases(typeof<HttpMethod>)
        methods |> Array.tryFind (fun c -> c.Name = str)
                |> Option.map (fun c -> FSharpValue.MakeUnion(c, [||]) :?> HttpMethod)

type HttpBody = 
    | HttpBody of string
    | Empty

let UnsupportedMethods = [PUT; DELETE; CONNECT; TRACE] 

let formatDate (dt:DateTime) =
    dt.ToUniversalTime().ToString("r")

let headerToRaw header =
    let header = header |> string
    let rep = Regex.Replace(header.[1..],  @"(\p{Lu})", "-$1")
    (header.[0] |> string) + rep

type HttpRequest =
    { Method  : HttpMethod
      Path    : Uri
      Headers : Map<HttpRequestHeader, string>
      Body    : HttpBody }

type HttpRequestParseError =
    | MissingHostHeader
    | UnsupportedMethod
    | UnsupportedProtocol
    | BadFormatting
    | BindingError

let statusCodeRaw =
    let table = [204, "No Content"
                 302, "Moved Permanently"
                 304, "Not Modified"
                 404, "Not Found"
                 405, "Method Not Allowed"
                 500, "Server Error"
                 503, "Service Unavailable"
                 505, "HTTP Version not supported"] |> Map.ofList
    (fun (code: StatusCode) -> 
        let code = int code
        if table.ContainsKey(code) then table.[code] else string code)

let (|RequestLine|Header|Error|) (str:string) =
    match str.Split(' ') with
    | [| m; p; v |] when p.Contains("/") ->
        match HttpMethod.Parse(m) with
        | None -> Error BadFormatting
        | Some m ->
            if UnsupportedMethods |> List.exists((=)m) then Error UnsupportedMethod
            else
                match Uri.TryCreate(p, UriKind.Relative) with
                | false, _ -> Error BadFormatting
                | true, p -> 
                    if v <> serverProtocol then Error UnsupportedProtocol
                    else RequestLine (m,p,v)
    | _ -> 
        let (k,v) = str.ToKeyValuePair([|':'|])
        let textInfo = CultureInfo("en-US", false)
        let textInfo = textInfo.TextInfo
        let k = textInfo.ToTitleCase(k)
        let k = k.Replace("-", "")
        match Enum.TryParse<HttpRequestHeader>(k) with
        | true, k  -> Header (k,v.Trim())
        | _        -> Error BadFormatting

type HttpRequestParseResult =
    | Success of HttpRequest
    | Failure of HttpRequestParseError

type HttpResponse =
    { Status  : StatusCode
      Headers : Map<HttpResponseHeader, string>
      Body    : Option<string> }
      member x.Raw =
        let str = sprintf "%s %s %s\n" 
                    serverProtocol (x.Status |> int |> string) (x.Status |> statusCodeRaw) 
        let sb = StringBuilder(str)
        let headers = 
            x.Headers 
            |> Map.fold (fun st k v -> sb.AppendLine((k |> headerToRaw) + ": " + v)) sb
        do headers.AppendLine() |> ignore
        let body = x.Body |> Option.toList
                          |> List.fold (fun (acc:StringBuilder) line -> acc.AppendLine(line)) 
                                (StringBuilder(headers |> string))
                          |> string
        if body.Length > headers.Length then body.[..(body.Length - 3)] 
        else body
 
let defaultRespHeaders() =
    [ HttpResponseHeader.Server,     "FHappy"
      HttpResponseHeader.Connection, "keep-alive"
      HttpResponseHeader.Date,       DateTime.Now |> formatDate ] 
      |> Map.ofList

let defaultResponse = { Status  = StatusCode.OK 
                        Headers = defaultRespHeaders()
                        Body    = Some "Served by FHappy written in F#!" }

let defaultErrorResponse = { Status  = StatusCode.ServerError 
                             Headers = defaultRespHeaders() 
                             Body    = Some "500 Internal Server Error" }
            
let parseHttpRequest (sr:BufferedStream) =
    let parseHeaders headers =
        let rec parseHeaders' headers =
            let line = sr.ReadLine()
            match line with
            | "" -> 
                let headers = headers |> Map.ofList
                if headers.ContainsKey(HttpRequestHeader.Host) then 
                    Some headers else None
            | Header kvp -> parseHeaders' <| kvp::headers
            | Error _ -> None
            | _ -> failwith "Unhandled case..." 
        parseHeaders' headers
    let rec parseBody() = 
           raise <| NotImplementedException()
    match sr.ReadLine() with
    | RequestLine (m,p,v) ->
        match parseHeaders [] with
        | None -> Failure BadFormatting
        | Some h -> Success { Method = m; Path = p; Headers = h; Body = Empty  }
    | Error e -> Failure e
    | _ -> failwith "Unhandled case" 

/////////////////////////////////////////////////////////////////


let routeRequest req =
    defaultResponse
    // Determine if it's static or dynamic content
    //match req with

let processRequestAsync (client: TcpClient)  = 
    async {  do printfn "Incoming HTTP request..."
             let stream = client.GetStream()
             let sr = new BufferedStream(stream)
             let content = parseHttpRequest sr
             match content with
             | Success v -> 
                do printfn "Successfully parsed request:\n%A" v
                return routeRequest v
             | Failure e -> 
                do printfn "Error: %s\nInvalid request sending 500" <| string e
                return HttpResponse.DefaultErrorResponse }

let sendResponseAsync (resp: HttpResponse) (client: TcpClient) =
    async { use stream = client.GetStream()
            use sw = new StreamWriter(stream)
            do printfn "Sending response...\n%A" resp
            // TODO: Validate http response before sending
            do sw.Write(resp.Raw) }