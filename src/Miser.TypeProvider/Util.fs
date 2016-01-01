module Util

open System

module internal Option = 
    let orElse v o = match o with | None -> v | Some v -> v

module internal Expr = 
    open FSharp.Quotations
    let raw (expr:Expr<_>) = expr.Raw

module internal Naming = 
    let toPascal (s:string) = 
        match s.ToCharArray() |> List.ofArray with
        | c::rest -> Char.ToUpper c::rest |> List.toArray |> String
        | _ -> s

    let toFieldName (s:string) =
        if s.StartsWith("_") then
            s
        else sprintf "_%s" s

