open System
open System.Net
open System.Net.Sockets
open Http

let ip = IPAddress.Parse("127.0.0.1")
let listener = TcpListener(ip, 80)
let startServer = 
    async { do listener.Start() 
            do printfn "Starting server..."
            while true do 
                // Accept incoming connection context
                let! context = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                // Send it to another async workflow without blocking the thread..
                let! response = processRequestAsync context
                do! sendResponseAsync response context  }
                //TODO: The request must be parsed as a HTTP request
                // The route and headers must be deduced and further behaviors
                // May need to be applied depending on the headers and HTTP method

Async.RunSynchronously startServer