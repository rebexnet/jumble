namespace Jumble.Utils

open Newtonsoft.Json
open System

module JsonConverters =
    type CConverter<'T>(write:JsonWriter -> JsonSerializer -> 'T -> unit, read) = 
        inherit JsonConverter<'T>() 
        override this.WriteJson ((writer:JsonWriter), (value:'T), (serializer:JsonSerializer)) = 
            write writer serializer value
        override this.ReadJson(reader:JsonReader, objectType: Type, existingValue: 'T, hasExistingValue: bool, serializer: JsonSerializer) = 
            read reader objectType existingValue hasExistingValue serializer

    let createConverter<'T> read write = CConverter<'T>(write, read)
    let createConverterSimple<'T> (r:JsonReader -> 'T) w = 
        createConverter 
            (fun reader _ _ _ _ -> r reader)
            (fun writer _ value -> w writer value)
        :> JsonConverter

    let stringOptConverter = createConverterSimple<string option>
                                (fun reader -> match reader.Value :?> string with null -> None | s -> Some s)
                                (fun writer v -> match v with None -> writer.WriteNull() | Some s -> writer.WriteValue(s))