// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open TypeLite
open TypeLite.Net4
open TypeLite.TsModels
open System.Reflection
open System
open YamlDotNet.Serialization
open System.IO
open System.Text.RegularExpressions

type ModifyRules() = 
    member val strip = ResizeArray<string>() with get, set
    member val translate = ResizeArray<ResizeArray<string>>() with get, set

type TypegenConfig() =
    member val dll = ResizeArray<string>() with get, set
    member val types = ResizeArray<string>() with get, set
    member val modulenames = ModifyRules() with get, set
    member val membertypes = ModifyRules() with get, set

let getReader fname =
    let file = new FileStream(fname, FileMode.Open, FileAccess.Read)
    new StreamReader(file) 

let readConfig fname =
    let rd = new Deserializer()
    use reader = getReader fname
    let res = rd.Deserialize<TypegenConfig> reader
    res

let camelize (s: string) = (Char.ToLower s.[0]).ToString() + s.Substring(1)
    
let strip needle (s: string) = s.Replace(needle, "")
let stripMany (needles: string seq) (s: string) = Seq.fold (fun state el -> strip el state) s needles

let configureTs (ts: TypeScriptFluent) (config: TypegenConfig) =    
    let simplifyModuleName = stripMany config.modulenames.strip
    let asTuple (rzarr: ResizeArray<'t>) = rzarr.[0], rzarr.[1]
    let typeMapTries = Seq.map asTuple config.membertypes.translate
    let typeFormatFunc (prop: TsProperty) (s:string) =
        let found  = RegexUt.reMatchTries typeMapTries s
        match found with 
            | None -> s
            | Some (_, newname) -> newname

    // nice "fluent" api...
    ts.WithMemberFormatter (fun t -> camelize t.Name) |> ignore
    ts.WithModuleNameFormatter (fun t -> simplifyModuleName t.Name) |> ignore
    ts.WithMemberTypeFormatter(
        new TsMemberTypeFormatter(typeFormatFunc)) |> ignore
    
    

let filterTypes (config: TypegenConfig) = 
    let globpats = config.types
    Seq.filter (fun (t: Type) -> RegexUt.anyReMatch globpats t.Name)    



let parseAssembly (fname: string) (config: TypegenConfig) = 
    let assy = Assembly.LoadFrom(fname)
    let mutable ts = TypeScript.Definitions()
    configureTs ts config

    let types = assy.GetTypes() |> filterTypes config
    for t in types do
        ts <- ts.For t

    let o = ts.Generate()
    ()



[<EntryPoint>]
let main argv =
    let a = 1
    let config = readConfig "../../../typegen.yaml" 
    parseAssembly config.dll.[0] config 
    0 // return an integer exit code
   