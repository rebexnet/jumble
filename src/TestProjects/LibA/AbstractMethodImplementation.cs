using System.ComponentModel;
using System.Reflection;

namespace LibA.AbstractMethodImplementation;

[Obfuscation]
public abstract class AbstractClassNoObfuscation
{
    public abstract void Method();
}

[Obfuscation(Exclude = false)]
public class C : AbstractClassNoObfuscation
{
    public override void Method()
    {
    }

    public void OtherMethod()
    {
    }
}

[Obfuscation(Exclude = false)]
public class COuter
{
    [Description("LibA.AbstractMethodImplementation.COuter.CInner")]
    internal sealed class CInner : AbstractClassNoObfuscation
    {
        public override void Method()
        {
        }

        public void OtherMethod()
        {
        }
    }
}