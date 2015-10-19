#I @"packages\FAKE\"
#r "FakeLib.dll"

open Fake
open Fake.AssemblyInfoFile

let project = "Wattle"
let authors = ["Casey Kramer"]
let summary = "Miser - An F# Type Provider for Thrift"
let description = "A type provider for Thrift RPC"
let version = "0.0.0.1"
let tags = "f# functional rpc thrift"
let nugetDir = @".\nuget"

let buildDir = @".\build\"
let deployDir = @".\artifacts\"
let testDir =  @".\test\"

let nunitPath = @".\packages\NUnit.Runners\tools"

let appReferences  = !! @"src\**\*.fsproj"     
let testReferences = !! @"tests\**\*.fsproj" 

Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "BuildApp" (fun _ ->
    CreateFSharpAssemblyInfo @".\src\Miser\AssemblyInfo.fs"
        [Attribute.Title "Miser"
         Attribute.Description "Thrift TypeProvider"
         Attribute.Product "Miser"
         Attribute.Copyright "Copyright © Casey Kramer 2015"
         Attribute.Version version
         Attribute.FileVersion version ]

    MSBuildRelease buildDir "Build" appReferences
        |> Log "AppBuild-Output:"
)

Target "BuildTest" (fun _ ->
    MSBuildRelease testDir "Build" testReferences
        |> Log "TestBuild-Output:"
)

Target "Test" (fun _ ->
 !! (testDir + @"\*Tests.dll") 
        |> NUnit (fun p -> 
            {p with 
                ToolPath = nunitPath; 
                OutputFile = testDir + @"TestResults.xml"})
)

  
Target "All" DoNothing

"Clean"
    ==> "BuildApp"
    ==> "BuildTest"
    ==> "Test"
    ==> "All"

Run <| getBuildParamOrDefault "target" "All"