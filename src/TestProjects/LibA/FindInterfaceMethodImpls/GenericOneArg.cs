// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.GenericOneArg;

public interface I<T>
{
    T Method(T value);
}

public class CImpl : I<int>
{
    public int Method(int value) => default;
}

public class CExpl : I<int>
{
    int I<int>.Method(int value) => default;
}

public class CGenImpl<T> : I<T>
{
    public T Method(T value) => default;
}

public class CGenExpl<T> : I<T>
{
    T I<T>.Method(T value) => default;
}