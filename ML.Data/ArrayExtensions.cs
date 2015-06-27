using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ML.Common;

namespace ML.Data
{
    public static class ArrayExtensions
    {
        public static T[] GetColumn<T>(this T[,] source, int columnIndex)
        {
            source.ThrowIfNull("source");
            if (columnIndex < 0 || columnIndex >= source.GetLength(1))
            {
                throw new ArgumentOutOfRangeException("columnIndex", "must be greater or equal to 0 and lower than dimension 1 length of the source array");
            }

            var result = new T[source.GetLength(0)];
            for (var i = 0; i < source.GetLength(0); i++)
            {
                result[i] = source[i, columnIndex];
            }

            return result;
        }
    }
}
