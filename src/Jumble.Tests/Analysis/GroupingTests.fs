module GroupingTests

open FSharp.Core.Fluent
open Mono.Cecil
open NUnit.Framework

open Jumble
open Jumble.Analysis
open Jumble.Tests

open LibA

type private GroupingTestSetup = {
    Assemblies: AssemblyCache
    Tree: TypeTree
    Groups: GroupingResult[][]
} with 
    member this.FindGroupByMember (m:IMemberDefinition) =
        this.Groups.find(fun g -> g.exists(fun gres -> gres.Member = m))

    interface System.IDisposable with 
        member this.Dispose () = 
            this.Assemblies.Dispose()


type GroupingTests () = 
    let mutable (s:GroupingTestSetup option) = None
    
    member private _.FMethod<'T, 'U> expr = 
        snd <| TypeSearch.findTypeMethod<'T, 'U> s.Value.Tree.AllTypes expr
    
    [<OneTimeSetUp>]
    member _.OneTimeSetup () = 
        let asmCache = AssemblyCache.FromPaths testFramework [libADllPath; libBDllPath] []
        let tree = TypeTree(asmCache)
        let groups = Grouping.groupMembers tree
        s <- Some { Assemblies = asmCache; Tree = tree; Groups = groups }

    [<OneTimeTearDown>]
    member _.OneTimeTeardown () = 
        (s.Value :> System.IDisposable).Dispose()

    [<Test>]
    member _.``Groups contain all members and each member is only in one group, only once`` () = 
        let s = s.Value
        let definedMembers = [libAAssemblyName; libBAssemblyName]
                             |> Seq.map(s.Assemblies.GetByName) 
                             |> Seq.collect(fun asm -> asm.Modules) 
                             |> Seq.collect(fun m -> m.Types) 
                             |> Seq.collect TypeDefinition.members

        for m in definedMembers do 
            let group = s.FindGroupByMember m
            Assert.IsTrue(group.filter(fun gm -> gm.Member = m).length = 1)

    [<Test>]
    member this.``Method implementing an interface is in one group (same dll)`` () = 
        let s = s.Value

        let iaMethod = this.FMethod<IA, _> <@ fun x -> x.MethodA() @>

        let group = s.FindGroupByMember iaMethod
        Assert.IsTrue(group.exists(fun m -> m.Member.Name = "MethodA" && m.Member.DeclaringType.FullName = "LibA.CA1_InheritsIA"))
    
    [<Test>]
    member this.``Method implementing an interface is in one group (another dll)`` () = 
        let s = s.Value

        let iaMethod = this.FMethod<IA, _> <@ fun x -> x.MethodA() @>
    
        let group = s.FindGroupByMember iaMethod
    
        Assert.IsTrue(group.exists(fun m -> m.Member.Name = "MethodA" && m.Member.DeclaringType.FullName = "LibB.CB1_InheritsIA"))

    [<Test>]
    member this.``Explicit interface implementation is NOT in same group and another method with same name is neither`` () = 
        let s = s.Value

        let iaMethod = this.FMethod<IA, _> <@ fun x -> x.MethodForExplicitImpl @>

        let group = s.FindGroupByMember iaMethod
        let methodsBelongingToCa1 = group.filter(fun m -> m.Member.DeclaringType.FullName = typeof<CA1_InheritsIA>.FullName).toList()

        Assert.IsFalse(methodsBelongingToCa1.exists(fun m -> m.Member.Name = "LibA.IA.MethodForExplicitImpl"))
        Assert.IsFalse(methodsBelongingToCa1.exists(fun m -> m.Member.Name = "MethodForExplicitImpl"))
    
    [<Test>]
    member this.``Virtual method and its override is in one group`` () =
        let s = s.Value

        let ca1Method =  this.FMethod<CA1_InheritsIA, _> <@ fun x -> x.VirtualMethod @>
        
        let group = s.FindGroupByMember ca1Method
        Assert.IsTrue(group.exists(fun m -> m.Member.Name = "VirtualMethod" && m.Member.DeclaringType.FullName = "LibB.CB2_InheritsCA1_IB"))

    [<Test>]
    member this.``Inner classes are considered when grouping`` () = 
        let s = s.Value
        let method = this.FMethod<IGeneric<_>, _> <@ fun i -> i.MethodA @>

        let group = s.FindGroupByMember method
        Assert.IsTrue(group.exists(fun m -> m.Member.Name = method.Name && m.Member.DeclaringType.FullName = "LibA.Outer/Inner`1"))

    member this.``Static interface method is NOT in the same group as class instance method with same name`` () =
        let s = s.Value
        let ifaceType = s.Tree.AllTypes |> Seq.find (fun t -> t.Name.Name = typeof<IWithStaticMember>.Name)
        let method = ifaceType.Members |> Seq.find (fun m -> m.Name = "StaticMethod")
        let group = s.FindGroupByMember method
        Assert.IsFalse(group.exists(fun m -> m.Member.DeclaringType.Name = typeof<CImplementingIWithStaticMember>.Name))