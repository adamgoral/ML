using System;
using System.Collections.Generic;
using System.Linq;
using Deedle;
using ML.Common;
using R.Interop;

namespace ML.DecisionTrees
{
    public class RDecisionTree<T> : IDecisionTree<T>
    {
        private readonly IRSession irSession;

        private readonly int featureColumns;

        public RDecisionTree(IRSession irSession, Frame<int, string> features, T[] labels)
        {
            irSession.ThrowIfNull("irSession");
            features.ThrowIfNull("features");
            labels.ThrowIfNull("labels");
            
            labels.ThrowIfZeroLength("labels");

            if (features.RowCount != labels.Length)
            {
                throw new ArgumentOutOfRangeException("features", "dimension 0 size must be equal to labels array length");
            }

            this.irSession = irSession;
            this.featureColumns = features.ColumnCount;
            this.Grow(features, labels);
        }

        private void Grow(Frame<int, string> features, IEnumerable<T> labels)
        {
            this.irSession.LoadPackages("party");
            features = features.Clone();
            var columnLabels = features.ColumnKeys.ToArray();
            features.AddColumn("label", labels);
            var formula = string.Format("label~{0}", string.Join("+", columnLabels));
            this.irSession.Set(features, "RDTTrainingSet");
            this.irSession.Eval(string.Format("RDT<-ctree({0}, RDTTrainingSet)", formula));
        }

        private static double[,] AddColumn(double[,] data, double[] column)
        {
            var newSize = data.GetLength(1) + 1;
            var result = new double[data.GetLength(0), newSize];
            for (var i = 0; i < data.GetLength(0); i++)
            {
                for (var j = 0; j < data.GetLength(1); j++)
                {
                    result[i, j] = data[i, j];
                }
            }

            for (var i = 0; i < column.Length; i++)
            {
                result[i, newSize - 1] = column[i];
            }

            return result;
        }

        public T[] Predict(Frame<int, string> features)
        {
            features.ThrowIfNull("features");
            if (features.ColumnCount != this.featureColumns)
            {
                throw new ArgumentOutOfRangeException("features", string.Format("dimension 1 must be exactly {0} in length", this.featureColumns));
            }

            this.irSession.Set(features, "RDTPredictSet");
            var result = this.irSession.Eval<T[]>("predict(RDT, newdata = RDTPredictSet)");
            return result;
        }
    }
}
