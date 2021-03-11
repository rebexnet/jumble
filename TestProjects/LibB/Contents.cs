// ReSharper disable All

using System;
using System.Collections.Generic;
using LibA;

namespace LibB
{
    public interface IB : IA
    {
        string ValGetterB { get; }
    }

    public class CB1_InheritsIA : IA
    {
        public string ValGetterA => "A";
        public string ValSetterA { set { } }
        public string ValGetterSetterA { get; set; }
        public string MethodA() => "A";
        public string MethodA(string _) => null;
        public string GenericMethod<T>() => null;
        void IA.MethodForExplicitImpl() { }
        public IEnumerable<ICollection<string>> MethodComplexParameter(IEnumerable<ICollection<string>> _) => null;
    }

    public class CB2_InheritsCA1_IB : CA1_InheritsIA, IB
    {
        public string ValGetterB => "B";
        public override void VirtualMethod() => throw new NotSupportedException();
    }

    public class CB3_Contains_CA1
    {
        public CB3_Contains_CA1()
        {
            Console.WriteLine(new CA1_InheritsIA().MethodA());
            Console.WriteLine(new CA1_InheritsIA().ValGetterA);
        }
    }

    public class EnumTest
    {
        // Mono.Cecil 0.11+ has a bug which causes export to crash on enum parameter with default value 
        public void FooWithOptional(SomeEnum e = SomeEnum.ValueA)
        {
        }

        public void FooWithRegular(SomeEnum e)
        {
        }
    }

    #region Custom attribute
    public enum CustomAttributeEnum
    {
        EnumValue
    }

    public abstract class CustomBaseAttribute : Attribute
    {
        public int BaseIntProperty { get; set; }
    }

    public class CustomAttribute : CustomBaseAttribute
    {
        public CustomAttribute(CustomAttributeEnum val)
        {
        }

        public int IntProperty { get; set; }
    }

    [Custom(CustomAttributeEnum.EnumValue)]
    public class ClassWithCustomAttribute
    {
        [Custom(CustomAttributeEnum.EnumValue)]
        public void Method()
        {
            
        }
    }
    #endregion

    public class CUsesLibAInternal
    {
        public void Foo() {
            LibA.CInternal.Foo();
        }
    }
}