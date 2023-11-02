// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.DirectImplicit;

public interface I
{
    void Method();
}

public class C : I
{
    public void Method() { }

    // trap
    public static void Method(params object[] args) { }
}