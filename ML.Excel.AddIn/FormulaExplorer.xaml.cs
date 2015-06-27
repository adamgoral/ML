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
    public partial class FormulaExplorer
    {
        private readonly Application application;
        private string selectedCellRef = null;

        public FormulaExplorer(Application application)
        {
            this.application = application;
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

        private void ShowFormula(string cellRef, string formula)
        {
            var formulaObject = new ExcelFormulaParser.ExcelFormula(formula);
            System.Action action = () =>
            {
                FormulaTreeView.Items.Clear();
                var rootItem = new TreeViewItem {Header = cellRef};
                FormulaTreeView.Items.Add(rootItem);
                ShowFormula(new Queue<ExcelFormulaToken>(formulaObject), rootItem.Items);
                rootItem.ExpandSubtree();
            };

            Dispatcher.BeginInvoke(action);
        }

        private void ShowFormula(Queue<ExcelFormulaToken> tokens, ItemCollection treeItems)
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

                var treeItem = new TreeViewItem {Header = token.Value, Tag = token};
                treeItems.Add(treeItem);
                if (token.Type == ExcelFormulaTokenType.Function && token.Subtype == ExcelFormulaTokenSubtype.Start)
                {
                    ShowFormula(tokens, treeItem.Items);
                }
            }
        }

        private void FormulaTreeView_OnSelectedItemChanged(object sender, RoutedPropertyChangedEventArgs<object> e)
        {
            var selection = FormulaTreeView.SelectedItem as TreeViewItem;
            if (selection != null)
            {
                var token = selection.Tag as ExcelFormulaToken;
                if (token != null)
                {
                    if (token.Subtype == ExcelFormulaTokenSubtype.Range)
                    {
                        var range = this.application.Range[token.Value];
                        range.Worksheet.Activate();
                        range.Select();
                    }
                }
            }
        }
    }
}
