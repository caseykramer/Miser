module TcpSocket

open System
open System.Net
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
type Port = uint16

/// Helper to just invoke the three 'funcs' once.
let internal invokeOnce funcs =
  let counter = ref 0
  let invokeOnce' f x =
    if (Interlocked.CompareExchange (counter, 1, 0) = 0) then
      f x
  let (a, b, c) = funcs
  (invokeOnce' a, invokeOnce' b, invokeOnce' c)

let inline internal isNull x = 
    match x with
    | null -> true
    | _ -> false

type Async with
    static member WithTimeout(timeout: TimeSpan,computation:Async<'T>):Async<'T> = 
        let callback (success,error,cancellation) = 
            let (success,error,_) = invokeOnce (success,error,cancellation)
            let fetchResult = async {
                try
                    let! result = computation
                    success result
                with ex -> error ex }
            let timeoutExpired = async {
                do! Async.Sleep(int timeout.TotalMilliseconds)
                let ex = new TimeoutException("Timeout expired") :> Exception
                error ex }
            Async.StartImmediate fetchResult
            Async.StartImmediate timeoutExpired

        Async.FromContinuations callback

    static member AsyncRaise ( e:exn ) = 
        Async.FromContinuations (fun (_,econt,_) -> econt e)

    static member AwaitTask (t: Task) = 
        let flattenExns (e:AggregateException) = e.Flatten().InnerExceptions.[0]
        let rewarpAsyncExns (it:Async<unit>) = 
            async { try do! it with :? AggregateException as ae -> do! Async.AsyncRaise (flattenExns ae) }
        let tcs = Threading.Tasks.TaskCompletionSource<unit>(TaskCreationOptions.None)
        t.ContinueWith((fun t ->
            if t.IsFaulted then tcs.SetException(t.Exception |> flattenExns)
            elif t.IsCanceled then tcs.SetCanceled()
            else tcs.SetResult(())), TaskContinuationOptions.ExecuteSynchronously)
        |> ignore
        tcs.Task |> Async.AwaitTask |> rewarpAsyncExns

type AsyncBuilder with
    member x.Bind(t:Task<'T>, f:'T -> Async<'R>):Async<'R> = async.Bind(Async.AwaitTask t,f)
    member x.Bind(t:Task,f:unit -> Async<'R>):Async<'R> = async.Bind(Async.AwaitTask t,f)

type AsyncResultCell<'T>() = 
    let source = TaskCompletionSource<'T>()

    member __.Complete result = source.TrySetResult result

    member __.AwaitResult(?timeout: TimeSpan) = 
        async {
            match timeout with
            | None -> 
                let! res = source.Task
                return Some res
            | Some time ->
                try
                    let! res = Async.WithTimeout(time,Async.AwaitTask(source.Task))
                    return Some res
                with
                | :? TimeoutException as e -> return None }


type SocketBinding = 
    { ip: IPAddress
      port: Port }
    
    member x.endpoint = new IPEndPoint(x.ip,int x.port)

    override x.ToString() = 
        let isv6 = (x.ip.AddressFamily = Sockets.AddressFamily.InterNetworkV6)
        if isv6 then
            String.Concat [ "["; x.ip.ToString(); "]"; x.port.ToString() ]
        else
            String.Concat [ x.ip.ToString(); x.port.ToString() ]

    static member ip_ = ((fun x -> x.ip),(fun v x -> { x with ip = v }))
    static member port_ = ((fun x -> x.port),(fun v x -> { x with port = v }))

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketBinding = 
    let make ip port = { ip = ip; port = port }

[<AllowNullLiteral>]
type BufferManager(totalBytes,bufferSize) = 
    let buffer = Array.zeroCreate totalBytes
    let freeOffsets = Stack<int>()

    member __.PopBuffer():ArraySegment<byte> = 
        let offset,_ = lock freeOffsets (fun _ ->
                freeOffsets.Pop(),freeOffsets.Count)
        ArraySegment(buffer,offset,bufferSize)
    member __.Init() = 
        lock freeOffsets (fun _ ->
            let mutable runningOffset = 0
            while runningOffset < totalBytes - bufferSize do
                freeOffsets.Push runningOffset
                runningOffset <- runningOffset + bufferSize)
    member __.FreeBuffer(args:ArraySegment<_>) = 
        lock freeOffsets (fun _ ->
            if freeOffsets.Contains args.Offset then failwithf "double free buffer %d" args.Offset
            freeOffsets.Push args.Offset)

type BufferSegment = 
    { buffer: ArraySegment<byte>
      offset: int
      length: int }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BufferSegment = 
    let inline make buffer offset length = 
        if length < 0 then failwithf "BufferSegment.make: length = %d < 0" length
        { buffer = buffer; offset = offset; length = length }

type private PoolMsg<'T> = 
    | Push of 'T
    | Pop of AsyncReplyChannel<'T>

type IConcurrentPool<'T> = 
    abstract Push:'T -> unit
    abstract Pop:unit -> 'T

type ConcurrentPool<'T>() = 
    let pool = Stack<'T>()
    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop() = 
            async {
                let! msg = inbox.Receive()
                match msg with
                | Push t -> pool.Push(t)
                | Pop rply -> rply.Reply(pool.Pop())
                return! loop()
            }
        loop() )

    member __.Push t = agent.Post(Push t)
    member __.Pop() = agent.PostAndReply(Pop)

    interface IConcurrentPool<'T> with
        member x.Push t = x.Push t
        member x.Pop() = x.Pop()

type GrowingConcurrentPool<'T>(factory:unit -> 'T) = 
    let pool = Stack<'T>()
    let agent = MailboxProcessor.Start(fun inbox ->
        let rec loop() = 
            async {
                let! msg = inbox.Receive()
                match msg with
                | Push t -> pool.Push(t)
                | Pop rply ->
                    if pool.Count = 0 then
                        rply.Reply(factory())
                    else rply.Reply(pool.Pop())
                return! loop()
            }
        loop() )

    
    member __.Push t = agent.Post(Push t)
    member __.Pop() = agent.PostAndReply(Pop)

    interface IConcurrentPool<'T> with
        member x.Push t = x.Push t
        member x.Pop() = x.Pop()

type AsyncUserToken(?socket:Sockets.Socket) = 
    let mutable _socket = match socket with Some x -> x | None -> null
    let mutable _continuation : Sockets.SocketAsyncEventArgs -> unit = ignore
    member __.Socket
        with get() = _socket
        and set v = _socket <- v
    member __.Continuation
        with get() = _continuation
        and set v = _continuation <- v
        
type SystemSocketError = Sockets.SocketError

type Error = 
    | SocketError of SystemSocketError
    | InputDataError of string
    | ConnectionError of string
    
type ByteSegment = System.ArraySegment<byte>

type SocketOp<'a> = Async<Choice<'a,Error>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketOp = 
    open System.Threading.Tasks
    
    let mreturn (x:'T): SocketOp<'T> = async.Return (Choice1Of2 x) 
    let abort (x:Error): SocketOp<_> = async.Return (Choice2Of2 x)

    let orInputError errorMsg: _ -> SocketOp<_> = function
        | Choice1Of2 x -> async.Return(Choice1Of2 x)
        | Choice2Of2 (y:string) -> async.Return(Choice2Of2 (InputDataError (errorMsg y)))
    
    let bind (fCont:_ -> SocketOp<_>) (value:SocketOp<_>): SocketOp<_> = 
        async {
            let! x = value
            match x with
            | Choice1Of2 x -> return! fCont x
            | Choice2Of2 (error:Error) -> return Choice2Of2 error }

    let bindError (fCont:_ -> SocketOp<_>) (value:SocketOp<_>): SocketOp<_> = 
        async {
            let! x = value
            match x with
            | Choice2Of2 err -> return! fCont err
            | _ -> return x }

    let map f (value:SocketOp<_>) : SocketOp<_> = 
        async {
            let! x = value
            match x with
            | Choice1Of2 x -> return Choice1Of2 (f x)
            | _ -> return x }

    let mapError f (value:SocketOp<_>): SocketOp<_> = 
        async {
            let! x = value
            match x with
            | Choice2Of2 err -> return Choice2Of2 (f err)
            | _ -> return x }

    let ofAsync (a:Async<'a>): SocketOp<'a> = 
        async {
            let! s = a
            return Choice1Of2 s }

    let ofTask (a:Task):SocketOp<_> = 
        async {
            let! s = a
            return Choice1Of2 s
        }

module SocketUtils = 
    open System.Net.Sockets

    let asyncDo (op:SocketAsyncEventArgs -> bool)
                (prepare:SocketAsyncEventArgs -> unit)
                (select:SocketAsyncEventArgs -> 'T)
                (args:SocketAsyncEventArgs) = 
        Async.FromContinuations <| fun (ok,_,_) ->
            prepare args

            let k (args:SocketAsyncEventArgs) = 
                match args.SocketError with
                | SocketError.Success ->
                    let result = select args
                    ok (Choice1Of2 result)
                | e -> ok (Choice2Of2 (Error.SocketError e))

            (args.UserToken :?> AsyncUserToken).Continuation <- k

            if not (op args) then
                k args

    let setBuffer (buf:ByteSegment) (args:SocketAsyncEventArgs) = 
        args.SetBuffer(buf.Array,buf.Offset,buf.Count)

    let accept (socket:Socket) evArgs = 
        asyncDo socket.AcceptAsync ignore (fun a -> a.AcceptSocket) evArgs

    let trans (a:SocketAsyncEventArgs) =
        ArraySegment<_>(a.Buffer,a.Offset,a.BytesTransferred)

open SocketUtils

[<AllowNullLiteral>]
type ITransport = 
    abstract member read: ByteSegment -> SocketOp<int>
    abstract member write: ByteSegment -> SocketOp<unit>
    abstract member shutdown:unit -> Async<unit>

type IClientTransport = 
    inherit ITransport
    abstract member connect : SocketBinding -> SocketOp<unit>

type IServerTransport =
    inherit ITransport
    abstract member listen: SocketBinding -> SocketOp<unit>

type ClientTransport(socket:Sockets.Socket,argPool:IConcurrentPool<Sockets.SocketAsyncEventArgs>,?remoteEndpoint:SocketBinding) = 
    let mutable socket = socket
    let args = argPool.Pop() 
    let shutdownSocket _ = 
        try
            if not (isNull socket) then
                try socket.Shutdown(Sockets.SocketShutdown.Both)
                with _ -> ()
        with _ -> ()
    let ensureSocket() = 
        if socket.Connected then
            ignore()
        else 
            match remoteEndpoint with
            | None -> failwith "Cannot perform actions when socket is closed"
            | Some endpoint ->
                socket.Connect(endpoint.ip,int endpoint.port)
    interface IClientTransport with
        member __.connect remoteBinding = 
            async {
                let! connectSocket = asyncDo socket.ConnectAsync (fun a -> a.RemoteEndPoint <- IPEndPoint(remoteBinding.ip,int remoteBinding.port)) (fun a -> a.ConnectSocket) args
                match connectSocket with
                | Choice2Of2 e -> return! SocketOp.abort e
                | Choice1Of2 s ->
                    socket <- s
                    return Choice1Of2()
            }
        member __.read(buf:ByteSegment) = 
            ensureSocket()
            asyncDo socket.ReceiveAsync (setBuffer buf) (fun a -> a.BytesTransferred) args
        member __.write(buf:ByteSegment) = 
            ensureSocket()
            asyncDo socket.SendAsync (setBuffer buf) ignore args
        member __.shutdown() = 
            async {
                shutdownSocket()
                args.AcceptSocket <- null
                argPool.Push(args)
                return () }

type ServerTransport(acceptArgs:Sockets.SocketAsyncEventArgs,
                     acceptArgsPool: IConcurrentPool<Sockets.SocketAsyncEventArgs>,
                     readArgsPool: IConcurrentPool<Sockets.SocketAsyncEventArgs>,
                     writeArgsPool:IConcurrentPool<Sockets.SocketAsyncEventArgs>) = 
    let socket = acceptArgs.AcceptSocket
    let readArgs = readArgsPool.Pop()
    let writeArgs = writeArgsPool.Pop()
    let shutdownSocket _ = 
        try
            if not (isNull socket) then
                try socket.Shutdown(Sockets.SocketShutdown.Both)
                with _ -> ()

                socket.Dispose()
        with _ -> ()

    interface IServerTransport with
        member __.listen (binding:SocketBinding) = 
            if not socket.IsBound then
                socket.Bind(IPEndPoint(binding.ip,int binding.port))
            asyncDo socket.AcceptAsync ignore ignore acceptArgs
        member __.read (buf:ByteSegment) = 
            asyncDo socket.ReceiveAsync (setBuffer buf) (fun a -> a.BytesTransferred) readArgs
        member __.write (buf:ByteSegment) = 
            asyncDo socket.SendAsync (setBuffer buf) ignore writeArgs
        member __.shutdown() = 
            async {
                shutdownSocket()
                acceptArgs.AcceptSocket <- null
                acceptArgsPool.Push acceptArgs
                readArgsPool.Push readArgs
                writeArgsPool.Push writeArgs
                return () }

type Connection = 
    { socketBinding:SocketBinding
      transport:ITransport
      bufferManager:BufferManager
      lineBuffer:ArraySegment<byte>
      segments:BufferSegment list
      expiresUTC:DateTime option }

    member x.ipAddr:IPAddress = 
        x.socketBinding.ip
    member x.port:Port = 
        x.socketBinding.port

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Connection = 
    let empty: Connection = 
        { socketBinding = SocketBinding.make IPAddress.IPv6Loopback 8083us
          transport = null
          bufferManager = null
          lineBuffer = ArraySegment<byte>()
          segments = []
          expiresUTC = None }

    let inline connect (cn:Connection) = 
        match cn.transport with
        | :? IClientTransport as client ->
            client.connect cn.socketBinding
        | _ -> async.Return(Choice2Of2 (Error.ConnectionError <| sprintf "can only connect to remot end points with a CLient Transport. This connections is configured with '%A'" cn.transport))

    let inline accept (cn:Connection) = 
        match cn.transport with
        | :? IServerTransport as server ->
            server.listen cn.socketBinding
        | _ -> async.Return(Choice2Of2 (Error.ConnectionError <| sprintf "can only listen for connections with a Server Transport. This connection is configured with: '%A'" cn.transport))

    let inline receive (cn:Connection) (buf:ByteSegment) = 
        cn.transport.read buf

    let inline send (cn:Connection) (buf:ByteSegment) =
        cn.transport.write buf

    let transport_ = 
        (fun x -> x.transport),
        fun v x -> { x with transport = v }
    let bufferManager_ = 
        (fun x -> x.bufferManager),
        fun v x -> { x with bufferManager = v }
    let lineBuffer_ = 
        (fun x -> x.lineBuffer),
        fun v x -> { x with lineBuffer = v }
    let segments_ = 
        (fun x -> x.segments),
        fun v x -> { x with segments = v }
