// ReSharper disable All

using System;

namespace LibA.Test
{
    public interface III<T>
    {
        void Foo(T value);
    }


    // ref III<III<U>>, gp = III<U>
    class DDD<U> : III<III<U>>
    {
        public void Foo(III<U> value)
        {
            throw new NotImplementedException();
        }
    }
}
