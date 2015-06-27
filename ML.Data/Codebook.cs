using ML.Common;

namespace ML.Data
{
    using System;
    using System.Collections.Generic;
    using Deedle;

    public class Codebook<TKey> : ICodebook<TKey>
    {
        public Codebook(IDictionary<TKey, IMapper> mappers)
        {
            this.Mappers = mappers;
        }

        public IDictionary<TKey, IMapper> Mappers { get; private set; }

        public Frame<TRowKey, TKey> Encode<TRowKey>(Frame<TRowKey, TKey> source)
        {
            source.ThrowIfNull("source");
            IMapper mapper;
            foreach (var columnKey in source.ColumnKeys)
            {
                if(!this.Mappers.TryGetValue(columnKey, out mapper))
                {
                    throw new InvalidOperationException(string.Format("no mapper could be located for column key {0}", columnKey));
                }
            }

            throw new NotImplementedException();
        }

        public Frame<TRowKey, TKey> Decode<TRowKey>(Frame<TRowKey, TKey> source)
        {
            throw new NotImplementedException();
        }

        public Series<TRowKey, V> Encode<TRowKey, V>(TKey mapperKey, Series<TRowKey, V> source)
        {
            throw new NotImplementedException();
        }

        public Series<TRowKey, V> Decode<TRowKey, V>(TKey mapperKey, Series<TRowKey, V> source)
        {
            throw new NotImplementedException();
        }
    }
}
