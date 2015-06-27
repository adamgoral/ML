using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Remoting.Messaging;
using System.Windows;
using System.Windows.Controls;
using ExcelFormulaParser;

namespace ML.Excel.AddIn
{
    using Microsoft.Office.Interop.Excel;

    /// <summary>
    /// Interaction logic for FormulaExplorer.xaml
    /// </summary>
    public partial class FormulaGraph
    {
        private readonly Application application;
        private string selectedCellRef = null;

        public FormulaGraph(Application application)
        {
            this.application = application;
            this.Graph = new FGraph();
            application.SheetSelectionChange += application_SheetSelectionChange;
            var worksheet = application.ActiveSheet as Worksheet;
            if (worksheet != null)
            {
                var selection = application.Selection as Range;
                if (selection != null)
                {
                    application_SheetSelectionChange(worksheet, selection);                    
                }
            }

            InitializeComponent();
        }

        private void application_SheetSelectionChange(object sheet, Range targetRange)
        {
            var cellRef = GetRangeAddress(targetRange);
            if (selectedCellRef == cellRef)
            {
                return;
            }

            selectedCellRef = cellRef;
            string formula = null;
            if (targetRange.HasArray)
            {
                formula = targetRange.FormulaArray as string;
                Debug.WriteLine(string.Format("Array formula: {0}", formula)); 
            }
            else if (targetRange.HasFormula)
            {
                formula = targetRange.Formula as string;
                Debug.WriteLine(string.Format("Formula: {0}", formula));
            }
            else
            {
                formula = targetRange.Value2 as string ?? string.Empty;
                Debug.WriteLine(string.Format("Value: {0}", formula));
            }
            if (formula != null)
            {
                ShowFormula(cellRef, formula);
            }
        }

        private string GetRangeAddress(Range range)
        {
            if (range.HasArray)
            {
                return range.CurrentArray.AddressLocal;
            }
            else
            {
                return range.AddressLocal;
            }
        }

        public FGraph Graph { get; set; }

        private void ShowFormula(string cellRef, string formula)
        {
            var formulaObject = new ExcelFormulaParser.ExcelFormula(formula);
            System.Action action = () =>
            {
                this.Graph = new FGraph();
                var rootItem = new FVertex() { Name = cellRef };
                Graph.AddVertex(rootItem);
                ShowFormula(new Queue<ExcelFormulaToken>(formulaObject), rootItem);
                graphLayout.Graph = this.Graph;
            };

            Dispatcher.BeginInvoke(action);
        }

        private void ShowFormula(Queue<ExcelFormulaToken> tokens, FVertex parent)
        {
            while (tokens.Any())
            {
                var token = tokens.Dequeue();
                if (token.Type == ExcelFormulaTokenType.Argument && token.Value == ",")
                {
                    continue;
                }

                if (token.Type == ExcelFormulaTokenType.Function && token.Subtype == ExcelFormulaTokenSubtype.Stop)
                {
                    return;
                }

                var child = new FVertex {Name = token.Value};
                this.Graph.AddVertex(child);
                this.Graph.AddEdge(new FEdge(parent, child));
                if (token.Type == ExcelFormulaTokenType.Function && token.Subtype == ExcelFormulaTokenSubtype.Start)
                {
                    ShowFormula(tokens, child);
                }
            }
        }
    }
}
