using Deedle;
using ML.Data;
using ML.DecisionTrees;
using R.Interop;

namespace ML.Excel.AddIn
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Linq;
    using System.Runtime.InteropServices;
    using ExcelDna.Integration;
    using ExcelDna.Integration.Extensibility;
    using Microsoft.Office.Interop.Excel;

    public static class Program
    {
        public static IRSession CreateRSession()
        {
            return CreateRSession(new string[0], true);
        }

        public static IRSession CreateRSession(IEnumerable<string> packages, bool inprocess)
        {
            IRSession result = inprocess ? new SessionRNet() as IRSession
                                        : new SessionRServe(Properties.Settings.Default.RPath, Properties.Settings.Default.RServePath, 1) as IRSession;
            result.LoadPackages(packages.ToArray());
            return result;
        }

        static void Main(string[] args)
        {
        }

        private static Application GetApplication()
        {
            var outlookProcesses = Process.GetProcessesByName("EXCEL");
            if (outlookProcesses.Any())
            {
                return Marshal.GetActiveObject("Excel.Application") as Application;
            }

            throw new InvalidOperationException("No excel running");
        }

        [ExcelFunction(Category = "test", Name = "addUp")]
        public static double[,] AddUp(double a, double[,] b)
        {
            using (var session = CreateRSession())
            {
                session.Set(b, "x");
                session.Set(a, "y");
                var result = session.Eval<double[,]>("x+y");
                return result;
            }
        }

        [ExcelFunction(Category = "ML", Name = "ML.InferLabel.NaiveBayes")]
        public static double[,] NBInferLabel(double[,] features, double[] labels)
        {
            using (var session = CreateRSession(new[] {"e1071"}, false))
            {
                session.Set(features, "features");
                session.Set(labels, "labels");
                var model = session.Eval<double[,]>("model<-naiveBayes(features,labels)");
                return model;
            }
        }

        [ExcelFunction(Category = "ML", Name = "ML.InferLabel.DecisionTree")]
        public static double[] DTInferLabel(double[,] features, double[] labels)
        {
            using (var session = CreateRSession())
            {
                var frame = Deedle.Frame.CreateEmpty<int, string>();
                for (var c = 0; c < features.GetLength(1); c++)
                {
                    frame.AddColumn<double>(string.Format("c{0}", c), features.GetColumn(c));
                }

                var decisionTree = new RDecisionTree<double>(session, frame, labels);
                return decisionTree.Predict(frame);
            }
        }
    }
}