// ReSharper disable All

using System;
using System.Collections.Generic;
using System.Globalization;

namespace LibA.Complex
{
    public interface IComplexGeneric<T, U>
    {
        U MethodSimple(T tval);
        U MethodComplex(T tval, IDictionary<T, U> dict);
        X GenMethod<X>(X val);
    }

    public interface IMiddle<V, W> : IComplexGeneric<V, W>
    {
    }

    public class CComplexGeneric<TT> : IMiddle<TT, IEnumerable<TT>>
    {
        public IEnumerable<TT> MethodSimple(TT tval) => throw new Exception();
        public IEnumerable<TT> MethodComplex(TT tval, IDictionary<TT, IEnumerable<TT>> dict) => throw new Exception();
        public X GenMethod<X>(X val) => throw new Exception();
    }
    
    public class CComplexGenericDouble : IMiddle<int, float>, IMiddle<string, char>, IComplexGeneric<DateTime, CultureInfo>
    {
        public float MethodSimple(int tval) => throw new Exception();
        public float MethodComplex(int tval, IDictionary<int, float> dict) => throw new Exception();
        public char MethodSimple(string tval) => throw new Exception();
        public char MethodComplex(string tval, IDictionary<string, char> dict) => throw new Exception();
        CultureInfo IComplexGeneric<DateTime, CultureInfo>.MethodSimple(DateTime tval) => throw new Exception();
        public CultureInfo MethodComplex(DateTime tval, IDictionary<DateTime, CultureInfo> dict) => throw new Exception();
        public X GenMethod<X>(X val) => throw new Exception();
        X IComplexGeneric<string, char>.GenMethod<X>(X val) => throw new Exception();
        X IComplexGeneric<int, float>.GenMethod<X>(X val) => throw new Exception();

        // this does NOT implement any interface - an extra
        public CultureInfo MethodSimple(DateTime tval) => default;
    }

    public class CComplexSimpleExplImpl : IComplexGeneric<int, float>
    {
        float IComplexGeneric<int, float>.MethodSimple(int tval) => 0f;
        float IComplexGeneric<int, float>.MethodComplex(int tval, IDictionary<int, float> dict) => 0f;
        X IComplexGeneric<int, float>.GenMethod<X>(X val) => default;
    }

    public class CComplexSimple2 : IComplexGeneric<DateTime, CultureInfo>
    {
        public CultureInfo MethodSimple(DateTime tval) => default;
        public CultureInfo MethodComplex(DateTime tval, IDictionary<DateTime, CultureInfo> dict) => default;
        public X GenMethod<X>(X val) => default;
    }
}
