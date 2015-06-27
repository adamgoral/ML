namespace ML.Data
{
    using Deedle;
    using System.Collections.Generic;

    public interface ICodebook<TKey>
    {
        IDictionary<TKey, IMapper> Mappers { get; }

        Frame<TRowKey, TKey> Encode<TRowKey>(Frame<TRowKey, TKey> source);

        Frame<TRowKey, TKey> Decode<TRowKey>(Frame<TRowKey, TKey> source);

        Series<TRowKey, V> Encode<TRowKey, V>(TKey mapperKey, Series<TRowKey, V> source);

        Series<TRowKey, V> Decode<TRowKey, V>(TKey mapperKey, Series<TRowKey, V> source);
    }

    public interface IMapper
    {
        object Map(object source);
    }

    public interface IMapper<T1, T2>: IMapper
    {
        T2 Map(T1 source);
    }
}
