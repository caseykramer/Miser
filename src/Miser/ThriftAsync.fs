module Thrift.Async

open System
open System.Text

open Thrift.Protocol
open System.IO

let internal isNull x = 
    match x with
    | null -> true
    | _ -> false


[<AllowNullLiteral>]
type TAsyncTransport = 
    inherit IDisposable
    abstract IsOpen:bool with get
    abstract AsyncOpen:unit -> Async<unit>
    abstract AsyncClose:unit -> Async<unit>
    abstract AsyncRead:byte[]*int*int -> Async<int>
    abstract AsyncReadAll:byte[]*int*int -> Async<int>
    abstract AsyncWrite:byte[]*int*int -> Async<unit>
    abstract AsyncFlush:unit -> Async<unit>

open TcpSocket
open Monads
open Thrift.Transport

type TAsyncConnectionTransport(connection:Connection,?timeout) = 
    let readBuffer = new MemoryStream(1024)
    let writeBuffer = new MemoryStream(1024)
    let read() = readStream connection readBuffer

    let readAll buff offset len = 
        async {
            if readBuffer.Length - readBuffer.Position >= (int64 len) then
                let pos = readBuffer.Position
                let! result = read()
                match result with
                | Choice1Of2 _ -> readBuffer.Seek(pos,SeekOrigin.Begin) |> ignore
                | Choice2Of2 err -> failwithf "%A" err
            else ignore()
            return! readBuffer.AsyncRead(buff,offset,len) }

    let writeAll buff offset len = 
        async {
            let _ = writeBuffer.AsyncWrite(buff,offset,len)
            let! result = transferStream connection writeBuffer
            match result with
            | Choice1Of2 _ -> ignore()
            | Choice2Of2 err -> failwithf "%A" err }

    interface TAsyncTransport with
        member __.IsOpen with get() = true
        member __.AsyncOpen() = async.Return ()
        member __.AsyncClose() = async.Return ()
        member __.AsyncRead(buf,offset,len) = readAll buf offset len
        member __.AsyncReadAll(buff,offset,len) = readAll buff offset len
        member __.AsyncFlush() = 
            async {
                if writeBuffer.Length - writeBuffer.Position > 0L then
                    do! transferStream connection writeBuffer |> Async.Ignore
                return () }
        member __.AsyncWrite(buff,offset,len) = writeAll buff offset len

        member __.Dispose() = 
            readBuffer.Dispose()
            writeBuffer.Dispose()

type TAsyncFramedTramsport(transport:TAsyncTransport) = 
    let readStream = new MemoryStream(1024)
    let writeStream = new MemoryStream(1024)
    let mutable disposed = false
    let headerSize = 4
    let headerBuf = Array.zeroCreate<byte> headerSize
    let notDisposed() = 
        if not disposed then 
            raise (ObjectDisposedException("TAsyncFramedTransport")) 
        else ignore()

    let encodeFrameSize (size:int) (buf:byte[]) = 
        buf.[0] <- byte (0xff &&& (size >>> 24))
        buf.[1] <- byte (0xff &&& (size >>> 16))
        buf.[2] <- byte (0xff &&& (size >>> 8))
        buf.[3] <- byte (0xff &&& (size))
    let decodeFrameSize (buf:byte[]) =
        int <|
            (int (buf.[0] &&& 0xffuy) <<< 24) |||
            (int (buf.[1] &&& 0xffuy) <<< 16) |||
            (int (buf.[2] &&& 0xffuy) <<< 8 ) |||
            (int (buf.[3] &&& 0xffuy))
    let readFrame() =
        async {
            let! _ = transport.AsyncReadAll(headerBuf,0,headerSize)
            let frameSize = decodeFrameSize headerBuf
            readStream.SetLength(int64 frameSize)
            readStream.Seek(0L,SeekOrigin.Begin) |> ignore
            let buf = readStream.GetBuffer()
            return! transport.AsyncReadAll(buf,0,frameSize) |> Async.Ignore
        }
    let initWriteBuffer() = 
        writeStream.SetLength(int64 headerSize)
        writeStream.Seek(0L,SeekOrigin.End) |> ignore
    do initWriteBuffer()

    interface TAsyncTransport with
        member __.AsyncFlush() = 
            async {
                notDisposed()
                let buffer = writeStream.GetBuffer()
                let len = writeStream.Length
                let data_len = int (len - (int64 headerSize))
                if data_len < 0 then raise (InvalidOperationException())
                encodeFrameSize data_len buffer
                do! transport.AsyncWrite(buffer,0,int len)
                initWriteBuffer()
                do! transport.AsyncFlush()
            }
        member x.AsyncWrite(buf,offset,len) =
            async {
                notDisposed()
                if writeStream.Length + (int64 len) > (int64 Int32.MaxValue) then
                    do! (x :> TAsyncTransport).AsyncFlush()
                do! writeStream.AsyncWrite(buf,offset,len)
            }
        member __.AsyncRead(buf,offset,len) =
            async {
                notDisposed()
                let! got = readStream.AsyncRead(buf,offset,len)
                if got > 0 then
                    return got
                else
                    do! readFrame()
                    return! readStream.AsyncRead(buf,offset,len) }
        member x.AsyncReadAll(buf,offset,len) =
            let rec doRead got = 
                async {
                    if got < len then
                        let! ret = (x :> TAsyncTransport).AsyncRead(buf,offset + got,len - got)
                        if ret <= 0 then
                            raise (TTransportException(TTransportException.ExceptionType.EndOfFile,"Cannot read. Remote side has closed"))
                        return! doRead (got + ret)
                    else
                        return got } 
            doRead 0
        member __.AsyncClose() = async.Return ()
        member __.AsyncOpen() = async.Return ()
        member __.IsOpen 
            with get() = not disposed
        member __.Dispose() = 
            readStream.Dispose()
            writeStream.Dispose()
            transport.Dispose()
            

type TAsyncWriteProtocol = 
    abstract AsyncWriteMessageBegin:TMessage -> Async<unit>
    abstract AsyncWriteMessageEnd:unit -> Async<unit>
    abstract AsyncWriteStructBegin:TStruct -> Async<unit>
    abstract AsyncWriteStructEnd:unit -> Async<unit>
    abstract AsyncWriteFieldBegin:TField -> Async<unit>
    abstract AsyncWriteFieldEnd:unit -> Async<unit>
    abstract AsyncWriteFieldStop:unit -> Async<unit>
    abstract AsyncWriteMapBegin:TMap -> Async<unit>
    abstract AsyncWriteMapEnd:unit -> Async<unit>
    abstract AsyncWriteListBegin:TList -> Async<unit>
    abstract AsyncWriteListEnd:unit -> Async<unit>
    abstract AsyncWriteSetBeing:TSet -> Async<unit>
    abstract AsyncWriteSetEnd:unit -> Async<unit>
    abstract AsyncWriteBool:bool -> Async<unit>
    abstract AsyncWriteByte:byte -> Async<unit>
    abstract AsyncWriteI16:int16 -> Async<unit>
    abstract AsyncWriteI32:int -> Async<unit>
    abstract AsyncWriteI64:int64 -> Async<unit>
    abstract AsyncWriteDouble:float -> Async<unit>
    abstract AsyncWriteString:string -> Async<unit>
    abstract AsyncWriteBinary:byte[] -> Async<unit>

type TAsyncReadProtocol = 
    abstract AsyncReadMessageBegin:unit -> Async<TMessage>
    abstract AsyncReadMessageEnd:unit -> Async<unit>
    abstract AsyncReadStructBegin:unit -> Async<TStruct>
    abstract AsyncReadStructEnd:unit -> Async<unit>
    abstract AsyncReadFieldBegin:unit -> Async<TField>
    abstract AsyncReadFieldEnd:unit -> Async<unit>
    abstract AsyncReadMapBegin:unit -> Async<TMap>
    abstract AsyncReadMapEnd:unit -> Async<unit>
    abstract AsyncReadListBegin:unit -> Async<TList>
    abstract AsyncReadListEnd:unit -> Async<unit>
    abstract AsyncReadSetBegin:unit -> Async<TSet>
    abstract AsyncReadSetEnd:unit -> Async<unit>
    abstract AsyncReadBool:unit -> Async<bool>
    abstract AsyncReadByte:unit -> Async<byte>
    abstract AsyncReadI16:unit -> Async<int16>
    abstract AsyncReadI32:unit -> Async<int>
    abstract AsyncReadI64:unit -> Async<int64>
    abstract AsyncReadDouble:unit -> Async<float>
    abstract AsyncReadString:unit -> Async<string>
    abstract AsyncReadBinary:unit -> Async<byte[]>

type TAsyncBinaryProtocol(transport:TAsyncTransport,?strictRed,?strictWrite) = 
    let versionMask = 0xffff0000
    let version1 = 0x80010000
    let strictRead = defaultArg strictRed false
    let strictWrite = defaultArg strictWrite true

    let readStringBody size = 
        async {
            let b = Array.zeroCreate size
            let _ = transport.AsyncReadAll(b,0,size)
            return Encoding.UTF8.GetString(b,0,b.Length)
        }
    
    member __.Transport
        with get() = transport
    
    interface IDisposable with
        member __.Dispose() =
            if not (isNull transport) then transport.Dispose()

    interface TAsyncReadProtocol with
        member x.AsyncReadBinary() = 
            async {
                let! size = (x :> TAsyncReadProtocol).AsyncReadI32()
                let b = Array.zeroCreate size
                let! _ = transport.AsyncReadAll(b,0,size)
                return b
            }
        member x.AsyncReadBool() = 
            async {
                let! v = (x :> TAsyncReadProtocol).AsyncReadByte()
                return v = 1uy
            }
        member __.AsyncReadByte() = 
            async {
                let b = Array.zeroCreate 1
                let! _ = transport.AsyncReadAll(b,0,1)
                match b with
                | [|v|] ->return v
                | _ -> return 0uy
            }
        member x.AsyncReadDouble() = 
            async {
                let! v = (x :> TAsyncReadProtocol).AsyncReadI64()
                let bytes = BitConverter.GetBytes(v)
                return BitConverter.ToDouble(bytes,0)
            }
        member x.AsyncReadFieldBegin() = 
            async {
                let mutable field = TField()
                let! ftype = (x :> TAsyncReadProtocol).AsyncReadByte() in field.Type <- (LanguagePrimitives.EnumOfValue ftype)
                if field.Type <> TType.Stop then
                    let! fid = (x :> TAsyncReadProtocol).AsyncReadI16() in field.ID <- fid
                return field
            }
        member __.AsyncReadFieldEnd() = async.Return ()
        member __.AsyncReadI16() = 
            async {
                let b = Array.zeroCreate 2
                let! _ = transport.AsyncReadAll(b,0,2)
                return int16 <|
                        ((b.[0] &&& 0xffuy) <<< 8) |||
                        (int16 (b.[1] &&& 0xffuy))
            }
        member __.AsyncReadI32() = 
            async {
                let b = Array.zeroCreate 4
                let! _ = transport.AsyncReadAll(b,0,4)
                return int32 <|
                        (int (b.[0] &&& 0xffuy) <<< 24) |||
                        (int (b.[1] &&& 0xffuy) <<< 16) |||
                        (int (b.[2] &&& 0xffuy) <<< 8)  |||
                        (int (b.[3] &&& 0xffuy))
            }
        member __.AsyncReadI64() = 
            async {
                let b = Array.zeroCreate 8
                let! _ = transport.AsyncReadAll(b,0,8)
                return int64 <|
                        (int64 (b.[0] &&& 0xffuy) <<< 56) |||
                        (int64 (b.[1] &&& 0xffuy) <<< 48) |||
                        (int64 (b.[2] &&& 0xffuy) <<< 40) |||
                        (int64 (b.[3] &&& 0xffuy) <<< 32) |||
                        (int64 (b.[4] &&& 0xffuy) <<< 24) |||
                        (int64 (b.[5] &&& 0xffuy) <<< 16) |||
                        (int64 (b.[6] &&& 0xffuy) <<< 8) |||
                        (int64 (b.[7] &&& 0xffuy))
            }
        member x.AsyncReadListBegin() = 
            async {
                let mutable list = TList()
                let! listType = (x :> TAsyncReadProtocol).AsyncReadByte() in list.ElementType <- (LanguagePrimitives.EnumOfValue listType)
                let! count = (x :> TAsyncReadProtocol).AsyncReadI32() in list.Count <- count
                return list
            }
        member __.AsyncReadListEnd() = async.Return ()
        member x.AsyncReadMapBegin() = 
            async {
                let mutable map = TMap()
                let! keyType = (x :> TAsyncReadProtocol).AsyncReadByte() in map.KeyType <- (LanguagePrimitives.EnumOfValue keyType)
                let! valueType = (x :> TAsyncReadProtocol).AsyncReadByte() in map.ValueType <- (LanguagePrimitives.EnumOfValue valueType)
                let! count = (x :> TAsyncReadProtocol).AsyncReadI32() in map.Count <- count

                return map
            }
        member __.AsyncReadMapEnd() = async.Return()
        member x.AsyncReadMessageBegin() =
            async {
                let mutable message = TMessage()
                let! size = (x :> TAsyncReadProtocol).AsyncReadI32()
                if size < 0 then
                    let version = (uint32)size &&& (uint32 versionMask)
                    if version <> uint32 version1 then
                        raise <| TProtocolException(TProtocolException.BAD_VERSION,"Bad version in ReadMessageBegin: " + string version)
                        return message
                    else
                        message.Type <- (enum (size &&& 0x000000ff))
                        let! name = (x :> TAsyncReadProtocol).AsyncReadString() in message.Name <- name
                        let! seqId = (x :> TAsyncReadProtocol).AsyncReadI32() in message.SeqID <- seqId
                        return message
                else
                    if strictRead then
                        raise <| TProtocolException(TProtocolException.BAD_VERSION,"Missing version in readMessageBegin, old client?")
                        return message
                    else
                        let! name = readStringBody size in message.Name <- name
                        let! t = (x :> TAsyncReadProtocol).AsyncReadByte() in message.Type <- (enum (int t))
                        let! seqId = (x :> TAsyncReadProtocol).AsyncReadI32() in message.SeqID <- seqId
                        return message
            }
        member __.AsyncReadMessageEnd() = async.Return ()
        member x.AsyncReadSetBegin() = 
            async {
                let mutable set = TSet()
                let! stype = (x :> TAsyncReadProtocol).AsyncReadByte() in set.ElementType <- (LanguagePrimitives.EnumOfValue stype)
                let! count = (x :> TAsyncReadProtocol).AsyncReadI32() in set.Count <- count
                return set
            }
        member __.AsyncReadSetEnd() = async.Return ()
        member x.AsyncReadString() = 
            async {
                let! b = (x :> TAsyncReadProtocol).AsyncReadBinary()
                return Encoding.UTF8.GetString(b)
            }
        member __.AsyncReadStructBegin() = async.Return (TStruct())
        member __.AsyncReadStructEnd() = async.Return ()
        
    interface TAsyncWriteProtocol with
        member __.AsyncWriteBinary(arg1) = 
            async {
                let _ = transport.AsyncWrite(arg1,0,arg1.Length)
                do! transport.AsyncFlush()
            }
        member x.AsyncWriteBool(arg1) = 
            async {
                if arg1 then 
                    do! (x :> TAsyncWriteProtocol).AsyncWriteByte(1uy) 
                else do! (x :> TAsyncWriteProtocol).AsyncWriteByte(0uy)
            }
            
        member __.AsyncWriteByte(arg1) = 
            async {
                let data = [|arg1|]
                do! transport.AsyncWrite(data,0,1) |> Async.Ignore
            }
        member x.AsyncWriteDouble(arg1) = 
            async {
                let bytes = BitConverter.GetBytes arg1
                do! (x :> TAsyncWriteProtocol).AsyncWriteI64 (BitConverter.ToInt64(bytes,0))
            }
        member x.AsyncWriteFieldBegin(arg1:TField) = 
            async {
                do! (x :> TAsyncWriteProtocol).AsyncWriteByte (byte arg1.Type)
                do! (x :> TAsyncWriteProtocol).AsyncWriteI32 (int arg1.ID)
            }
        member __.AsyncWriteFieldEnd() = async.Return ()
        member x.AsyncWriteFieldStop() = (x :> TAsyncWriteProtocol).AsyncWriteByte (byte TType.Stop)
        member __.AsyncWriteI16(arg1:int16) = 
            async {
                let data = [| (byte (0xffs &&& arg1 >>> 8));(byte (0xffs &&& arg1)) |]
                do! transport.AsyncWrite(data,0,2)
            }
        member __.AsyncWriteI32(arg1) = 
            async {
                let data = [| (byte (0xff &&& arg1 >>> 24))
                              (byte (0xff &&& arg1 >>> 16))
                              (byte (0xff &&& arg1 >>> 8))
                              (byte (0xdd &&& arg1)) |]
                do! transport.AsyncWrite(data,0,4)
            }
        member __.AsyncWriteI64(arg1) = 
            async {
                let data = [| (byte (0xffL &&& arg1 >>> 56))
                              (byte (0xffL &&& arg1 >>> 48))
                              (byte (0xffL &&& arg1 >>> 32))
                              (byte (0xffL &&& arg1 >>> 24))
                              (byte (0xffL &&& arg1 >>> 16))
                              (byte (0xffL &&& arg1 >>> 8))
                              (byte (0xffL &&& arg1)) |]
                do! transport.AsyncWrite(data,0,8) |> Async.Ignore
            }
        member x.AsyncWriteListBegin(arg1) = 
            async {
                do! (x :> TAsyncWriteProtocol).AsyncWriteByte(byte arg1.ElementType)
                do! (x :> TAsyncWriteProtocol).AsyncWriteI32(arg1.Count)
            }
        member __.AsyncWriteListEnd() = async.Return ()
        member x.AsyncWriteMapBegin(arg1) = 
            async {
                do! (x :> TAsyncWriteProtocol).AsyncWriteByte (byte arg1.KeyType)
                do! (x :> TAsyncWriteProtocol).AsyncWriteByte (byte arg1.ValueType)
                do! (x :> TAsyncWriteProtocol).AsyncWriteI32 arg1.Count
            }
        member __.AsyncWriteMapEnd() = async.Return ()
        member x.AsyncWriteMessageBegin(arg1:TMessage) = 
            async {
                if strictWrite then
                    let version = version1 ||| (int arg1.Type)
                    do! (x :> TAsyncWriteProtocol).AsyncWriteI32 version
                    do! (x :> TAsyncWriteProtocol).AsyncWriteString arg1.Name
                    do! (x :> TAsyncWriteProtocol).AsyncWriteI32 arg1.SeqID
                else
                    do! (x :> TAsyncWriteProtocol).AsyncWriteString arg1.Name
                    do! (x :> TAsyncWriteProtocol).AsyncWriteByte (byte arg1.Type)
                    do! (x :> TAsyncWriteProtocol).AsyncWriteI32 arg1.SeqID
            }
        member __.AsyncWriteMessageEnd() = async.Return ()
        member x.AsyncWriteSetBeing(arg1) = 
            async {
                do! (x :> TAsyncWriteProtocol).AsyncWriteByte (byte arg1.ElementType)
                do! (x :> TAsyncWriteProtocol).AsyncWriteI32 arg1.Count
            }
        member __.AsyncWriteSetEnd() = async.Return ()
        member x.AsyncWriteString(arg1) = 
            async {
                let bytes = Encoding.UTF8.GetBytes(arg1)
                return! (x :> TAsyncWriteProtocol).AsyncWriteBinary(bytes)
            }
        member __.AsyncWriteStructBegin(_) = async.Return ()
        member __.AsyncWriteStructEnd() = async.Return ()
        

type TAsyncProcessor = 
    abstract AsyncProcess:#TAsyncReadProtocol*#TAsyncWriteProtocol -> Async<bool>
type TAsyncBase = 
    abstract AsyncRead:#TAsyncReadProtocol -> Async<unit>
    abstract AsyncWrite:#TAsyncWriteProtocol -> Async<unit>