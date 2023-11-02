// ReSharper disable once CheckNamespace
namespace LibA.FindInterfaceMethodImpls.MultipleImplementation;

public interface I<T>
{
    void Method(T value);
}

public class CImplImpl : I<int>, I<string>
{
    public void Method(int value) {}
    public void Method(string value) { }

    // trap
    public void Method(char value) { }
}


public class CImplExpl : I<int>, I<string>
{
    public void Method(int value) {}
    void I<string>.Method(string value) { }

    // trap
    public void Method(float value) { }
}

public class CExplExpl : I<int>, I<string>
{
    void I<int>.Method(int value) { }
    void I<string>.Method(string value) { }

    // trap
    public void Method(int value) { }
}