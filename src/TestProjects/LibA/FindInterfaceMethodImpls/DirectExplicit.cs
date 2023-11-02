// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.DirectExplicit;


public interface I
{
    void Method();
}

public class C : I
{
    void I.Method() { }
}