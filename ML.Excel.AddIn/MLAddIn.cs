using System;
using System.Windows.Controls;
using ExcelDna.Integration;
using ExcelDna.Integration.CustomUI;
using Microsoft.Office.Interop.Excel;

namespace ML.Excel.AddIn
{
    public class MLAddIn : IExcelAddIn
    {
        internal static MLAddIn Instance;

        private ICustomTaskPane customTaskPane;

        private static ICustomTaskPane CreateCustomTaskPane(string title, UserControl control)
        {
            var result = CustomTaskPaneFactory.CreateCustomTaskPane(typeof(WinFormElementHost), title);
            var winformHost = result.ContentControl as WinFormElementHost;
            if (winformHost == null)
            {
                throw new InvalidOperationException("Custom task pane failed to create WinFormElementHost");
            }

            winformHost.Control = control;
            return result;
        }

        public void AutoOpen()
        {
            Instance = this;
        }

        public void AutoClose()
        {
            if (this.customTaskPane != null)
            {
                this.customTaskPane.Delete();
            }
        }

        public void ShowFormulaExplorer()
        {
            if (this.customTaskPane == null)
            {
                var app = ExcelDnaUtil.Application as Application;
                this.customTaskPane = CreateCustomTaskPane("Formula explorer", new FormulaGraph(app));                
            }

            this.customTaskPane.DockPosition = MsoCTPDockPosition.msoCTPDockPositionLeft;
            this.customTaskPane.Visible = true;
        }
    }
}