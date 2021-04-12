---
Title: Configuration file syntax 
CanonicalUrl: https://www.rebex.net/doc/configuration
---
# Jumble configuration file syntax

```json
// config.json example

// file paths are relative to the location of the configuration file
{
  // Sets which .NET framework should be used to resolve system dependencies
  // The framework identifiers are the same as in https://fsprojects.github.io/Paket/dependencies-file.html#Framework-restrictions
  "Framework": "netcoreapp2.1",

  // Input DLLs. Dependencies will be searched for in input dll directories and SearchPaths section below
  "Input": [
    {
      "File": "Test.dll",
      
      // Obfuscation level - supported modes are:
      // - 'untouchable', 'default' or not specified: do not modify anything
      // - 'onlyNecessary' or 'testLib': no types will be renamed; type members will only be renamed if necessary. Use this for test libraries, etc.
      // - 'privateOnly': only private types and members will be renamed
      // - 'privateAndPublic': both private and public members will be renamed   
      "ObfuscationLevel": "onlyNecessary",
      
      // Path to assembly signing key (optional)
      "SigningKey": "key.snk"
    }, 
    // ...
  ],
  
  // Output directory
  "Output": "../out",
  
  // Log directory
  "LogDir": "../log",
  
  // Types and members to be excluded from renaming. The syntax is still in development.
  "Exclude": ["Rebex.Foo.Bar"],
  
  // Method and type name generator (optional). Supported values (with few exceptions) are:
  // - 'default' or not specified: random name generator
  // - 'id': keep as is
  // - 'test': _JUMBLE suffix will be appended to all renamed type and member names
  // - 'order': (only applicable for parameters) - parameters will be named by their order of appearance (p0, p1, p2...)
  "MethodNameGenerator": "test",
  "GenericParameterNameGenerator": "order",
  "ParameterNameGenerator": "order",
  "TypeNameGenerator": "test",

  // Additional reference search paths (optional)
  "SearchPaths": ["../references"],

  // Additional options (optional)
  "AdditionalOptions": {
    // Disables whitelisting of enums on which ToString() is called
    "DisableEnumToStringFiltering": true
  }
}
```

The configuration file can be generated using `Rebex.Jumble.Lib` package:

```fsharp
open Jumble

let jumbleConfig = {
  ConfigurationModel.defaultConfig with
    Framework = "netcoreapp3.1"
    Input = [...]
    ...
}
      
ConfigurationModel.save "jumble-config.json" jumbleConfig
```