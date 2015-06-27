using System.Linq;
using Deedle;
using ML.Common;

namespace R.Interop
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using RserveCLI2;
    
    public class SessionRServe : IRSession
    {
        private readonly Process rServeProcess;
        private readonly RConnection connection;

        public SessionRServe(string rDirectory, string rServePath, int portOffset)
        {
            rDirectory.ThrowIfNullOrWhitespace("rDirectory");
            rServePath.ThrowIfNullOrWhitespace("rServePath");
            if (portOffset < 0)
            {
                throw new ArgumentException("must be greater or equal to 0", "portOffset");
            }

            portOffset = 6312 + portOffset;
            this.rServeProcess = StartR(rDirectory, rServePath, portOffset);
            this.connection = new RConnection(System.Net.IPAddress.Loopback, port: portOffset);
        }

        public void LoadPackages(params string[] packages)
        {
            packages.ThrowIfNull("packages");
            foreach (var package in packages)
            {
                connection.VoidEval(string.Format("require({0})", package));
            }
        }

        private static Process StartR(string rDirectory, string rServePath, int port)
        {
            var startInfo = new ProcessStartInfo(rServePath, "--RS-port " + port)
            {
                WorkingDirectory = rDirectory,
                CreateNoWindow = true,
                UseShellExecute = false
            };
            var rProcess = Process.Start(startInfo);
            return rProcess;
        }

        public class SexpListBuilder : IFrameOperation<object>
        {
            public SexpList list;

            public object Invoke<TR, TC>(Frame<TR, TC> frame)
            {
                this.list = Sexp.MakeDataFrame();
                throw new NotImplementedException();
                return null;
            }
        }

        public void Set(IFrame frame, string name)
        {
            frame.ThrowIfNull("frame");
            name.ThrowIfNullOrWhitespace("name");
            var sexpBuilder = new SexpListBuilder();
            frame.Apply(sexpBuilder);
            this.connection[name] = sexpBuilder.list;
        }

        public void Set(double[,] data, string name)
        {
            data.ThrowIfNull("data");
            name.ThrowIfNullOrWhitespace("name");
            this.connection[name] = Sexp.Make(data);
        }

        public void Set(double[] data, string name)
        {
            data.ThrowIfNull("data");
            name.ThrowIfNullOrWhitespace("name");
            this.connection[name] = Sexp.Make(data);
        }

        public void Set(double data, string name)
        {
            data.ThrowIfNull("data");
            name.ThrowIfNullOrWhitespace("name");
            this.connection[name] = Sexp.Make(data);
        }

        public T Eval<T>(string expression) where T : class
        {
            expression.ThrowIfNullOrWhitespace("expression");

            var result = this.connection.Eval(expression);
            var resultType = typeof (T);
            if (resultType == typeof(double[]))
            {
                return result.AsDoubles as T;
            }
            else if (resultType == typeof(double[,]))
            {
                return result.As2DArrayDouble as T;
            }
            else if (resultType == typeof(string[]))
            {
                return result.AsStrings as T;
            }

            throw new NotSupportedException(string.Format("return type {0} is not supported", resultType));
        }

        public void Eval(string expression)
        {
            expression.ThrowIfNullOrWhitespace("expression");
            this.connection.Eval(expression);
        }

        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (!disposing) return;
            this.rServeProcess.Kill();
            this.rServeProcess.Dispose();
            this.connection.Dispose();
        }
    }
}