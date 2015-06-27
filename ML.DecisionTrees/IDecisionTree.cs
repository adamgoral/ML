using Deedle;

namespace ML.DecisionTrees
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Threading.Tasks;
    
    public interface IDecisionTree<T>
    {
        T[] Predict(Frame<int, string> features);
    }
}
