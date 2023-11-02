// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.ParentExplicit;

public interface I
{
    void Method();
}

public class C1 : I
{
    void I.Method() { }
}

public class C2 : C1, I
{ }