// ReSharper disable All

using System;
using System.ComponentModel;

#pragma warning disable 169
#pragma warning disable 67

namespace LibC
{
    public enum PublicEnum
    {
        Value1,
        Value2
    }
    
    public class CWithDifferentVisibilities
    {
        private event EventHandler PrivateEvent; 
        protected event EventHandler ProtectedEvent;
        public event EventHandler PublicEvent;

        private string PrivateField;
        protected string ProtectedField;
        public string PublicField;

        private void PrivateMethod(string parameter1) { }
        protected void ProtectedMethod(string parameter1) { }
        public void PublicMethod(string parameter1) { }

        private int PrivateProperty { get; set; }
        protected int ProtectedProperty { get; set; }
        public int PublicProperty { get; set; }

        protected internal void ProtectedInternalMethod(string parameter1) { }
        protected internal int ProtectedInternalProperty { get; set; }

    }

    public class COuterClass
    {
        public class CInnerClass
        {
        }
    }

    internal class COuterInternalClass
    {
        public class CInnerClass
        {
        }
    }
    
    [Description("LibC.IWithStaticMember")]
    public interface IWithStaticMember
    {
        public static int StaticField;
        public static void StaticMethod() { }
    }

    public class CImplementingIWithStaticMember : IWithStaticMember
    {
        public void StaticMethod() { }
    }
}