// ReSharper disable once CheckNamespace
// ReSharper disable UnusedMember.Local
// ReSharper disable UnusedMember.Global
// ReSharper disable UnusedType.Global
// ReSharper disable UnusedType.Local
#pragma warning disable 169
// ReSharper disable once CheckNamespace
namespace LibA.Annotated
{
    using System.Reflection;

    /// With default [Obfuscation] - Features = all, Exclude = true, ApplyToMembers = true. Should whitelist everything.
    [Obfuscation]
    public class CDefault
    {
        private int privateField;
        private void PrivateMethod() { }
        public void PublicMethod() { }

        public class CNestedPublic
        {
        }

        private class CNestedPrivate
        {
        }
    }

    [Obfuscation(ApplyToMembers = false)]
    public class CApplyToMembersFalse
    {
        private int privateField;
        private void PrivateMethod() { }
        public void PublicMethod() { }

        public class CNestedPublic
        {
        }

        private class CNestedPrivate
        {
        }
    }

    [Obfuscation(Exclude = false)]
    public class CExcludeFalse
    {
        private int privateField;
        private void PrivateMethod() { }
        public void PublicMethod() { }

        public class CNestedPublic
        {
        }

        private class CNestedPrivate
        {
        }
    }


    [Obfuscation(Feature = "applyToPrivate")]
    public class CApplyToPrivate
    {
        private int privateField;
        private void PrivateMethod() { }
        public void PublicMethod() { }

        public class CNestedPublic
        {
        }

        private class CNestedPrivate
        {
        }
    }

    [Obfuscation(Feature = "applyToChildren")]
    public class CApplyToChildren
    {
    }

    public class CApplyToChildrenChild : CApplyToChildren
    {
        private int privateField;
        private void PrivateMethod() { }
        public void PublicMethod() { }

        public class CNestedPublic
        {
        }

        private class CNestedPrivate
        {
        }
    }
}