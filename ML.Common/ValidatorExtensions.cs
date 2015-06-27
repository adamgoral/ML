using System;

namespace ML.Common
{
    public static class ValidatorExtensions
    {
        public static void ThrowIfNull(this object obj, string paramName)
        {
            if (obj == null)
            {
                throw new ArgumentNullException(paramName);
            }
        }

        public static void ThrowIfNullOrWhitespace(this string text, string paramName)
        {
            text.ThrowIfNull(paramName);

            if (string.IsNullOrWhiteSpace(text))
            {
                throw new ArgumentException("Cannot be empty or whitespace", paramName);
            }
        }

        public static void ThrowIfZeroLength<T>(this T[] array, string paramName)
        {
            if (array.Length == 0)
            {
                throw new ArgumentOutOfRangeException(paramName, "array lenght must be larger than 0");
            }
        }

        public static void ThrowIfZeroLength<T>(this T[,] array, string paramName)
        {
            for (var i = 0; i < 2; i++)
            {
                if (array.GetLength(i) == 0)
                {
                    throw new ArgumentOutOfRangeException(paramName, string.Format("dimension {0} length must be larger than 0", i));
                }
            }
        }
    }
}
