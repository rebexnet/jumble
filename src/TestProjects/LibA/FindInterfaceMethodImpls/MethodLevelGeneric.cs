// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.MethodLevelGeneric;

public interface I
{
    T Method<T>();
}

public class CImpl : I
{
    public T Method<T>() => default;

    // trap
    public int Method() => default;
}

public class CExpl : I
{
    T I.Method<T>() => default;

    // trap
    public T Method<T>() => default;
}