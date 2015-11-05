namespace Miser

open Microsoft.VisualStudio.Package
open Microsoft.VisualStudio.TextManager.Interop

type private ThriftScanner(buffer:IVsTextBuffer) = 
    interface IScanner with
        member __.ScanTokenAndProvideInfoAboutIt(tokInfo,state) = 
            true
        member __.SetSource(source,offset) = ()

type private ThriftAuthoringScope() =
    inherit AuthoringScope()

    override x.GetDataTipText(line,col,span) = 
        span <- new TextSpan()
        null
    override x.GetDeclarations(view,line,col,info,reason) = null
    override x.Goto(cmd,textView,line,col,span) = 
        span <- new TextSpan()
        null
    override x.GetMethods(line,col,name) = null

type ThriftLanguageService() as this = 
    inherit LanguageService()
    let mutable scanner:IScanner option = None
    let preferences = new LanguagePreferences(this.Site,typeof<ThriftLanguageService>.GUID,this.Name)
    do preferences.Init()

    override x.Name with get() = "Thrift"
    override x.GetLanguagePreferences() = preferences
    override x.GetScanner(buffer):IScanner = 
        match scanner with
        | None ->
            scanner <- Some (upcast ThriftScanner(buffer))
            scanner |> Option.get
        | Some s -> s
    override x.ParseSource(r) = upcast ThriftAuthoringScope()
    override x.GetFormatFilterList() = ""