using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Deedle;
using Deedle.RPlugin;
using ML.Common;
using RDotNet;
using RDotNet.Internals;
using RProvider;

namespace R.Interop
{
    public class SessionRNet : IRSession
    {
        private readonly REngine engine;

        public SessionRNet()
        {
            this.engine = REngine.GetInstance();
        }

        public void Dispose()
        {
            this.Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (!disposing)
            {
                return;
            }

            this.engine.Dispose();
        }

        //public class SymbolicExpressionBuilder : IFrameOperation<SymbolicExpression>
        //{
        //    private readonly REngine engine;

        //    public SymbolicExpressionBuilder(REngine engine)
        //    {
        //        this.engine = engine;
        //    }

        //    public SymbolicExpression Invoke<TR, TC>(Frame<TR, TC> frame)
        //    {
        //        var columnNames = frame.ColumnKeys.Cast<string>().ToArray();
        //        var columns = frame.GetAllColumns<double>().Select(c => c.Value.ValuesAll.ToArray()).ToArray();
        //        return this.engine.CreateDataFrame(columns, columnNames);
        //    }
        //}
        public void Set(IFrame data, string name)
        {
            data.ThrowIfNull("data");
            name.ThrowIfNullOrWhitespace("name");
            var builder = new DataFrameToR() as IConvertToR<IFrame>;
            var frame = builder.Convert(this.engine, data);
            this.engine.SetSymbol(name, frame);
        }

        public void Set(double[,] data, string name)
        {
            data.ThrowIfNull("data");
            name.ThrowIfNullOrWhitespace("name");
            this.engine.SetSymbol(name, this.engine.CreateNumericMatrix(data));
        }

        public void Set(double[] data, string name)
        {
            data.ThrowIfNull("data");
            name.ThrowIfNullOrWhitespace("name");
            this.engine.SetSymbol(name, this.engine.CreateNumericVector(data));
        }

        public void Set(double data, string name)
        {
            data.ThrowIfNull("data");
            name.ThrowIfNullOrWhitespace("name");
            this.engine.SetSymbol(name, this.engine.CreateNumeric(data));
        }

        public T Eval<T>(string expression) where T : class
        {
            expression.ThrowIfNullOrWhitespace("expression");
            var result = this.engine.Evaluate(expression);
            var resultType = typeof (T);
            if (resultType == typeof(double[]))
            {
                return result.AsNumeric().ToArray() as T;
            }
            else if (resultType == typeof(double[,]))
            {
                return result.AsNumericMatrix().ToArray() as T;
            }
            else if (resultType == typeof(string[]))
            {
                return result.AsCharacter().ToArray() as T;
            }

            throw new NotSupportedException(string.Format("return type {0} is not supported", resultType));
        }

        public void Eval(string expression)
        {
            expression.ThrowIfNullOrWhitespace("expression");
            this.engine.Evaluate(expression);
        }

        public void LoadPackages(params string[] packages)
        {
            packages.ThrowIfNull("packages");
            foreach (var package in packages)
            {
                this.Eval(string.Format("require({0})", package));
            }
        }
    }
}
