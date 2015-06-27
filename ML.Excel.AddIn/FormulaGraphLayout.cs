using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using GraphSharp.Controls;
using QuickGraph;

namespace ML.Excel.AddIn
{
    public class FormulaGraphLayout : GraphLayout<FVertex, FEdge, FGraph>
    {
    }

    public class FGraph : BidirectionalGraph<FVertex, FEdge>
    {
    }

    public class FEdge : Edge<FVertex>
    {
        public FEdge(FVertex source, FVertex target) : base(source, target)
        {
        }
    }

    public class FVertex
    {
        public string Name { get; set; }
    }
}
