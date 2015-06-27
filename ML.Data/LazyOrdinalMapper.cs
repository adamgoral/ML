using ML.Common;

namespace ML.Data
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class LazyOrdinalMapper<T> : Mapper<T, int>
    {
        private int currentCode = -1;

        public LazyOrdinalMapper()
            : base(new Dictionary<T, int>())
        {
        }

        public override int Map(T source)
        {
            source.ThrowIfNull("source");
            int result;
            if(this.lookup.TryGetValue(source, out result))
            {
                return result;
            }

            currentCode++;
            this.lookup.Add(source, currentCode);
            return currentCode;
        }
    }
}
