// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.ParentImplicit;

public interface I
{
    void Method();
}

public class C1
{
    public void Method() { }
}

public class C2 : C1, I
{ }