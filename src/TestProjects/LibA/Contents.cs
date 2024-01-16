// ReSharper disable All

using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Runtime.CompilerServices;

[assembly:InternalsVisibleTo("LibB")]

namespace LibA
{
    public interface IA
    {
        string ValGetterA { get; }
        string MethodA();
        string MethodA(string _);
        
        string GenericMethod<T>();

        string ValSetterA { set; }
        string ValGetterSetterA { get; set; }

        void MethodForExplicitImpl();

        IEnumerable<ICollection<string>> MethodComplexParameter(IEnumerable<ICollection<string>> _);

        void DefaultImplMethod() {}
    }

    [Description("LibA.IGeneric<TValue>")]
    public interface IGeneric<TValue>
    {
        void MethodA(TValue value);
    }

    public class CA1_InheritsIA : IA
    {
        public string ValGetterA => "A";

        public string MethodA() => "A";
        public string MethodB() => "B";

        public string MethodA(string _) => null;

        public string GenericMethod<T>() => null;

        public string ValSetterA { set { } }

        public string ValGetterSetterA { get; set; }

        [Description("Explicit")]
        void IA.MethodForExplicitImpl() { }

        public IEnumerable<ICollection<string>> MethodComplexParameter(IEnumerable<ICollection<string>> _) => null;
        
        [Description("New")]
        public void MethodForExplicitImpl() { }

        public virtual void VirtualMethod() => throw new NotSupportedException();

        public void DefaultImplMethod() { }
    }

    public class CGeneric<T> : IGeneric<T>
    {
        public void MethodA(T value) => throw new Exception();
        public void MethodA(int value) => throw new Exception();
    }

    public class CGenericSpec : IGeneric<int>
    {
        public void MethodA(int value) => throw new Exception();
        public void MethodA(string value) => throw new Exception();
    }

    public class CGenericExplImpl<T> : IGeneric<T>
    {
        // MethodDefinition.Name = LibA.IGeneric<T>.MethodA
        // MethodDefinition.Overrides works
        void IGeneric<T>.MethodA(T value) => throw new NotImplementedException();
    }

    public class CGenericSpecExplImpl : IGeneric<int>
    {
        void IGeneric<int>.MethodA(int value) => throw new NotImplementedException();
    }

    public class Dict : Dictionary<string, string>
    {

    }

    public class Outer
    {
        public class Inner<T> : IGeneric<T>
        {
            public void MethodA(T value) => throw new NotImplementedException();
        }
    }

    public enum SomeEnum
    {
        ValueA,
        ValueB,
        ValueC
    }

    public class CInheritsPublicInterfaceExplicitImpl : IEnumerable
    {
        IEnumerator IEnumerable.GetEnumerator() {
            throw new NotImplementedException();
        }
    }

    [Description("LibA.IWithStaticMember")]
    public interface IWithStaticMember
    {
        public static int StaticField;
        public static void StaticMethod() { }
    }

    public class CImplementingIWithStaticMember : IWithStaticMember
    {
        public void StaticMethod() { }
    }

    public class CNestedParent
    {
        public class CNestedChild
        { }
    }

    internal class CInternal
    {
        public static void Foo() { }
    }

    [Description("LibA.CWithConstructors")]
    public class CWithConstructors
    {
        public CWithConstructors() { }
        public CWithConstructors(int value) { }
    }
}

[Description("NamespacelessClass")]
public class NamespacelessClass
{ }