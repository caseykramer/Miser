module Monads

open System
open TcpSocket
open System.Collections.Generic 

type SocketMonad() = 
    member __.Return(x:'a):SocketOp<'a> = SocketOp.mreturn x
    member __.Zero():SocketOp<unit> = SocketOp.mreturn ()
    member __.ReturnFrom(x) = x
    member __.Delay(f:unit -> SocketOp<'a>) = async { return! f() }
    member __.Bind(x,f) = 
        async {
            let! result = x
            match result with
            | Choice1Of2 a -> return! f a
            | Choice2Of2 b -> return Choice2Of2 b }
    member x.Conbine(v,f) = x.Bind (v,fun () -> f)
    member x.While(guard,body:SocketOp<unit>):SocketOp<unit> = 
        async {
            if guard() then
                let! result = body
                match result with
                | Choice1Of2 _ ->
                    return! x.While(guard,body)
                | Choice2Of2 _ ->
                    return result
            else return! x.Zero()
        }
    member __.TryWith(body,handler) =
        async {
            try
                return! body
            with e ->
                return! handler }
    member __.TryFinally(body,compensation) = 
        async {
            try
                return! body
            finally
                compensation()
        }
    member __.Using(disposable:#IDisposable,body) = 
        async {
            use _ = disposable
            return! body disposable }
    member x.For(sequence:seq<_>,body:'a -> SocketOp<unit>) = 
        x.Using(sequence.GetEnumerator(),fun enum -> x.While(enum.MoveNext,x.Delay(fun _ -> body enum.Current)))

[<AutoOpen;
  CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SocketMonad = 
    let socket = SocketMonad()

    type TcpWorker<'a> = Connection -> Async<'a>

    let asyncWrite (connection:Connection) (data:byte[]):SocketOp<unit> = 
        async {
            if data.Length > 0 then
                return! Connection.send connection (ArraySegment<_>(data,0,data.Length))
            else return Choice1Of2 () }
    let readStream (from:Connection) (toStream:IO.Stream):SocketOp<unit> = 
        let rec doBlock() = 
            socket {
                let buff = from.bufferManager.PopBuffer()
                try
                    let! read = Connection.receive from buff
                    if read <= 0 then
                        return ()
                    else
                        do! SocketOp.ofTask <| toStream.WriteAsync(buff.Array,buff.Offset,read)
                        return! doBlock()
                finally from.bufferManager.FreeBuffer(buff)
            }
        doBlock()
    let transferStream (toStream:Connection) (from:IO.Stream):SocketOp<unit> = 
        let buf = Array.zeroCreate<byte> 0x2000
        let rec doBlock() = socket {
            let! read = SocketOp.ofAsync <| from.AsyncRead buf
            if read <= 0 then
                return ()
            else 
                do! Connection.send toStream ((buf,0,read) |> ArraySegment<_>)
                return! doBlock() }
        doBlock ()

    let transferStreamBounded (toStream:Connection) (from:IO.Stream) len = 
        let bufSize = 0x2000
        let buf = Array.zeroCreate<byte> bufSize
        let rec doBlock left = socket {
            let! read = SocketOp.ofAsync <| from.AsyncRead(buf,0,min bufSize len)
            if read <= 0 || left - read = 0 then
                return ()
            else 
                do! Connection.send toStream ((buf,0,read) |> ArraySegment<_>)
                return! doBlock (left - read)}
        doBlock len