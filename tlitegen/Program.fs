open TypeLite
open TypeLite.Net4
open TypeLite.TsModels
open System.Reflection
open System
open YamlDotNet.Serialization
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections
open YamlDotNet.Core

type ModifyRules() = 
    member val strip = ResizeArray<string>() with get, set
    member val translate = ResizeArray<ResizeArray<string>>() with get, set

type TypeSelector() = 
    member val typename = ResizeArray<string>() with get, set
    member val namespacename = ResizeArray<string>() with get, set

type TypegenConfig() =
    member val dll = ResizeArray<string>() with get, set
    member val types = TypeSelector() with get, set
    member val modulenames = ModifyRules() with get, set
    member val membertypes = ModifyRules() with get, set

let getReader fname =
    let file = new FileStream(fname, FileMode.Open, FileAccess.Read)
    new StreamReader(file) 

let readConfig fname =
    let rd = new Deserializer()
    use reader = getReader fname
    try 
        let res = rd.Deserialize<TypegenConfig> reader
        Some(res)
    with
    | :? YamlException as e ->
        printfn "Yaml parse error:\n%s" e.Message
        None

        

let camelize (s: string) = (Char.ToLower s.[0]).ToString() + s.Substring(1)
    
let strip needle (s: string) = s.Replace(needle, "")
let stripMany (needles: string seq) (s: string) = Seq.fold (fun state el -> strip el state) s needles

let wraps (s:string) wrapper = wrapper + s + wrapper

let unionstrings names =
    String.Join(" | ", [ for n in names -> wraps n "\""])

let configureTs (ts: TypeScriptFluent) (config: TypegenConfig) =    
    let simplifyModuleName = stripMany config.modulenames.strip
    let asTuple (rzarr: ResizeArray<_>) = rzarr.[0], rzarr.[1]
    let typeMapTries = Seq.map asTuple config.membertypes.translate
    let typeFormatFunc (prop: TsProperty) (s:string) =
        let typ = prop.PropertyType.Type
        let found  = RegexUt.reMatchTries typeMapTries s
        match found with 
            | Some (_, newname) -> newname
            | None -> 
                if typ.IsEnum then
                    let values = Enum.GetNames(typ)
                    unionstrings values
                else
                    s

                    

    // nice "fluent" api...
    ts.WithMemberFormatter (fun t -> camelize t.Name) |> ignore
    ts.WithModuleNameFormatter (fun t -> simplifyModuleName t.Name) |> ignore
    ts.WithMemberTypeFormatter(
        new TsMemberTypeFormatter(typeFormatFunc)) |> ignore
    

let filterTypes (config: TypegenConfig) =     
    Seq.filter (fun (t: Type) -> 
        (t.IsClass) && 
        ((RegexUt.anyReMatch config.types.typename t.Name) || (RegexUt.anyReMatch config.types.namespacename t.Namespace)))


type TLGen(config: TypegenConfig) =
    let ts = TypeScript.Definitions()
    do configureTs ts config
    member x.FeedAssembly fname =
        let assy = Assembly.LoadFrom fname
        let types = assy.GetTypes() |> filterTypes config
        for t in types do
            try
                ts.For t |> ignore
            with
            | :? ArgumentException -> 
                eprintfn "Bad type: %s" t.Name
            | :? Exception ->
                eprintfn "Unknown exception for type: %s" t.Name
                 
    member x.Generate() = ts.Generate()

[<EntryPoint>]
let main argv =
    
    let arg = [|"../../../typegen.yaml"|]
    //let arg = [|"TypeLite.dll"|]
   
    //let arg = argv
    match arg with
        | [||] -> 
            eprintfn "Specify dll or configuration .yaml file"
            0
        | [|fname|] when fname.ToLower().EndsWith(".yaml") -> 
            let config = readConfig fname
            match config with
            | Some config -> 
                let tlg = TLGen(config)
                for fname in config.dll do
                    tlg.FeedAssembly fname
                let out = tlg.Generate()
                printfn "%s" out
                0
            | None -> 1
        | [|fname|] when fname.ToLower().EndsWith(".dll") ->
            let config = new TypegenConfig( dll = ResizeArray(argv), types= TypeSelector( typename = ResizeArray([".*"])))
            let tlg = TLGen(config)
            tlg.FeedAssembly(fname)
            let out = tlg.Generate()
            printfn "%s" out
            0
        | _ ->
            printfn "Nothing to do. Specify either .dll or .yaml file"
            1

       