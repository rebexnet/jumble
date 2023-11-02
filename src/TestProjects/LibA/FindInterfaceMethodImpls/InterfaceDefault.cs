// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.InterfaceDefault;

public interface I1
{
    void Method();
}

public interface I2 : I1
{
    void I1.Method() { }
}