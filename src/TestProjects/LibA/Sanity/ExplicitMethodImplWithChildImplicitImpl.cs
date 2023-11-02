// ReSharper disable once CheckNamespace
namespace LibA.Sanity.ExplicitMethodImplWithChildImplicitImpl;

public interface I
{
    string Method();
}

public class C1 : I
{
    string I.Method() => "C1";
}

public class C2I : C1, I
{
    public string Method() => "C2I";
}

public class C2 : C1
{
    public string Method() => "C2";
}