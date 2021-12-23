namespace Jumble.Export

module MapFile = 
    open Jumble.Utils.JsonConverters
    open Newtonsoft.Json
    
// stringOptConverter
    let converters = [| classNameConverter;  |]

    let createMapFile (r:RenameMap) : byte[] = 
        let s = JsonConvert.SerializeObject(r, Formatting.Indented, converters)
        System.Text.Encoding.UTF8.GetBytes(s)
 
    let loadFrom (path:string) = 
        let s = System.IO.File.ReadAllText(path)
        JsonConvert.DeserializeObject<RenameMap>(s, converters)
    
    