using System;
using Deedle;

namespace R.Interop
{
    public interface IRSession : IDisposable
    {
        void Set(IFrame data, string name);
        void Set(double[,] data, string name);
        void Set(double[] data, string name);
        void Set(double data, string name);
        T Eval<T>(string expression) where T : class;
        void Eval(string expression);
        void LoadPackages(params string[] packages);
    }
}