module TcpServer

open System
open System.Threading
open System.Net
open System.Net.Sockets
open TcpSocket
open Monads
open SocketUtils

[<Literal>]
let MaxBacklog = System.Int32.MaxValue

let utcNow() = DateTimeOffset.UtcNow
let numberOfClients = ref 0

type StartedData = 
    { startCalledUtc: DateTimeOffset
      socketBoundUtc: DateTimeOffset option
      binding: SocketBinding }

    override x.ToString() = 
        sprintf "%.3f ms with binding %O:%d"
            ((x.socketBoundUtc |> Option.fold (fun _ t -> t) x.startCalledUtc) - x.startCalledUtc).TotalMilliseconds
            x.binding.ip x.binding.port

let closeSocket (s:Sockets.Socket) =
    try
        if isNull s then
            if s.Connected || s.IsBound then
                s.Disconnect true
    with _ -> ()

let shutdownSocket (s:Sockets.Socket) =
    try
        if isNull s then
            try
                s.Shutdown(Sockets.SocketShutdown.Both)
            with _ -> ()
            s.Close()
            s.Dispose()
    with _ -> ()

let stopTcp (socet:Sockets.Socket) = 
    try
        socet.Close()
    with _ -> ()

let createPools maxOps bufferSize = 
    
    let acceptAsyncArgsPool = ConcurrentPool<SocketAsyncEventArgs>()
    let readAsyncArgsPool = ConcurrentPool<SocketAsyncEventArgs>()
    let writeAsyncArgsPool = ConcurrentPool<SocketAsyncEventArgs>()

    let bufferManager = new BufferManager(bufferSize * (maxOps + 1), bufferSize)
    bufferManager.Init()

    for _ in 0 .. maxOps - 1 do
        let readEventArg = new SocketAsyncEventArgs()
        let userToken = AsyncUserToken()
        readEventArg.UserToken <- userToken
        readEventArg.add_Completed(fun _ b -> userToken.Continuation b)

        readAsyncArgsPool.Push readEventArg

        let writeEventArg = new Sockets.SocketAsyncEventArgs()
        let userToken = AsyncUserToken()
        writeEventArg.UserToken <- userToken
        writeEventArg.add_Completed(fun _ b -> userToken.Continuation b)

        writeAsyncArgsPool.Push writeEventArg

        let acceptArg = new Sockets.SocketAsyncEventArgs()
        let userToken = AsyncUserToken()
        acceptArg.UserToken <- userToken
        acceptArg.add_Completed(fun _ b -> userToken.Continuation b)

        acceptAsyncArgsPool.Push acceptArg
    (acceptAsyncArgsPool,readAsyncArgsPool,writeAsyncArgsPool,bufferManager)

let private aFewTimes f = 
    let s ms = System.Threading.Thread.Sleep (ms: int)
    let rec run = function
        | 0us | 1us -> f()
        | n -> try f() with _ -> s 10; run (n - 1us)
    run 3us

let job (serveClient:TcpWorker<unit>) binding (transport:ITransport) (bufferManager:BufferManager) = 
    async {
        Interlocked.Increment numberOfClients |> ignore
        let connection = { Connection.empty with socketBinding = binding
                                                 transport = transport
                                                 bufferManager = bufferManager
                                                 lineBuffer = bufferManager.PopBuffer()
                                                 segments = [] }
        try
            use! __ = Async.OnCancel (fun () -> Async.RunSynchronously (transport.shutdown()))
            do! serveClient connection
        with
            | :? System.IO.EndOfStreamException -> ignore()
            | _ -> ignore()
        bufferManager.FreeBuffer(connection.lineBuffer)
        do! transport.shutdown()
        Interlocked.Decrement numberOfClients |> ignore
    }

type TcpServer = StartedData -> AsyncResultCell<StartedData> -> TcpWorker<unit> -> Async<unit>

let runServer maxConcurrentOps bufferSize (binding:SocketBinding) startData (acceptingConnections:AsyncResultCell<StartedData>) serveClient = 
    async {
        try
            let a,b,c,bufferManager = createPools maxConcurrentOps bufferSize

            let listenSocket = new Socket(binding.endpoint.AddressFamily, SocketType.Stream, ProtocolType.Tcp)
            aFewTimes (fun () -> listenSocket.Bind binding.endpoint)
            listenSocket.Listen MaxBacklog

            use! cncl = Async.OnCancel(fun () -> stopTcp listenSocket)
            let! token = Async.CancellationToken

            let startData = { startData with socketBoundUtc = Some (utcNow())}
            acceptingConnections.Complete startData |> ignore

            while not (token.IsCancellationRequested) do 
                try
                    let acceptArgs = a.Pop()
                    let! r = accept listenSocket acceptArgs
                    match r with
                    | Choice1Of2 _ ->
                        let socket = acceptArgs.AcceptSocket
                        let remoteBinding = 
                            let rep = socket.RemoteEndPoint :?> IPEndPoint
                            { ip = rep.Address; port = uint16 rep.Port}
                        let transport = new ServerTransport(acceptArgs, a, b, c)
                        Async.Start(job serveClient remoteBinding transport bufferManager, token)
                    | Choice2Of2 e -> failwithf "Socket failed to accept client, error: %A" e
                with _ -> ()
            return ()
        with ex -> return raise ex }

let startTcpIpServerAsync (serveClient: TcpWorker<unit>) (binding: SocketBinding) (runServer: TcpServer) = 
    let acceptingConnections = AsyncResultCell<StartedData>()

    let startData = 
        { startCalledUtc = DateTimeOffset.UtcNow
          socketBoundUtc = None
          binding = binding }
    acceptingConnections.AwaitResult(),runServer startData acceptingConnections serveClient