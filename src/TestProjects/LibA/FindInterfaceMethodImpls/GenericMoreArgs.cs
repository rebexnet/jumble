// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.GenericMoreArgs;


public interface I<T, U>
{
    T Method(U value);
}

public class CImpl : I<int, string>
{
    public int Method(string value) => default;
}

public class CExpl : I<int, string>
{
    int I<int, string>.Method(string value) => default;
}

public class CGenImpl<T1, T2> : I<T1, T2>
{
    public T1 Method(T2 value) => default;
}

public class CGenExpl<T1, T2> : I<T1, T2>
{
    T1 I<T1, T2>.Method(T2 value) => default;
}