namespace Jumble.Tests

open NUnit.Framework

open Jumble

[<TestFixture>]
type VersionTests() =
    
    [<Test>]
    [<TestCaseSource("CanParseSource")>]
    member _.``Can parse`` s expected =
        let v = Version.parse s
        Assert.That(v, Is.EqualTo expected)
    
    static member CanParseSource() =
        let mk s vnum vpre = TestCaseData(s, Version.create vnum vpre).SetName(s)
        [
            mk "1" (1,0,0,0) None
            mk "0.11.22.33" (0,11,22,33) None
            mk "1.5-pre1" (1,5,0,0) (Some "pre1")
        ]

    [<Test>]
    [<TestCaseSource("CanNotParseSource")>]
    member _.``Can not parse`` s =
        let v = Version.tryParse s
        Assert.That(v.IsNone)
        
    static member CanNotParseSource() =
        [
            ""
            "1.2.3.4.5"
            "1e5.2.3"
            "1.2.-3"
            "1..3"
        ]
        
    [<Test>]
    [<TestCaseSource("FirstGreaterThanSecondSource")>]
    member _.``First >= second`` s1 s2 =
        let v1, v2 = (Version.parse s1, Version.parse s2)
        Assert.That(v1, Is.GreaterThanOrEqualTo v2)

    static member FirstGreaterThanSecondSource() =
        let mk v1 v2 = TestCaseData(v1, v2).SetName $"%s{v1} >= %s{v2}"
        [
            mk "1.1.0" "1.0.999"
            mk "1.1.0" "1.1.0-rc1"
            mk "1.1.0-rc2" "1.1.0-rc1"
            mk "1.11.0" "1.9.0"
            mk "9" "8.9.9"
        ]