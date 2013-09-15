module Thrift
open Thrift

type TAsyncProcessor<'service>(service:'service,handlers:Map<string,unit->unit>) = 
    interface TProcessor with
        member this.Process(p_in,p_out) = false

