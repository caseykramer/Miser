[<AutoOpen>]
module Helpers
    open NUnit.Framework

    let fail() = Assert.Fail()
    let failwith t = Printf.ksprintf (fun s -> Assert.Fail(s)) t

