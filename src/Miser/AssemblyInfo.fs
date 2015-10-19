namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Miser")>]
[<assembly: AssemblyDescriptionAttribute("Thrift TypeProvider")>]
[<assembly: AssemblyProductAttribute("Miser")>]
[<assembly: AssemblyCopyrightAttribute("Copyright © Casey Kramer 2015")>]
[<assembly: AssemblyVersionAttribute("0.0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.0.1"
