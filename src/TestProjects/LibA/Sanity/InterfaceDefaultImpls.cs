// ReSharper disable once CheckNamespace

namespace LibA.Sanity.InterfaceDefaultImpls;

public interface I1
{
    string Method() => "I1";
}

public interface I2 : I1
{
    string I1.Method() => "I2";
}

public interface I3 : I2
{
    string I1.Method() => "I3";
}

public class C12 : I1, I2
{
    public string TestAsI1() => ((I1)this).Method();
    public string TestAsI2() => ((I2)this).Method();
}

public class C2 : I2
{
    public string TestAsI1() => ((I1)this).Method();
    public string TestAsI2() => ((I2)this).Method();
}

public class C123 : I1, I2, I3
{
    public string TestAsI1() => ((I1)this).Method();
    public string TestAsI2() => ((I2)this).Method();
    public string TestAsI3() => ((I3)this).Method();
}