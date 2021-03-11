namespace Jumble.Utils

open Jumble
open Jumble.Utils.JsonConverters

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module JsonConverters = 
    let classNameConverter = createConverterSimple<ClassName>
                                (fun reader -> reader.Value :?> string |> ClassName.fromFullname)
                                (fun writer v -> writer.WriteValue(ClassName.toString v))
