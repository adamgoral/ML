namespace ML.Data
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class Mapper<T1, T2> : IMapper<T1, T2>
    {
        protected readonly IDictionary<T1, T2> lookup;

        public Mapper(IDictionary<T1, T2> lookup)
        {
            this.lookup = lookup;
        }

        public virtual T2 Map(T1 source)
        {
            return this.lookup[source];
        }

        public object Map(object source)
        {
            return this.Map((T1)source);
        }

        public IMapper<T2, T1> CreateDecoder()
        {
            var reverseLookup = lookup.ToDictionary(kvp => kvp.Value, kvp => kvp.Key);
            return new Mapper<T2, T1>(reverseLookup);
        }
    }
}
