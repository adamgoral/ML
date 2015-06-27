using System.Runtime.InteropServices;
using ExcelDna.Integration.CustomUI;

namespace ML.Excel.AddIn
{
    [ComVisible(true)]
    public class MLRibbon : ExcelRibbon
    {
        public void ShowFormulaExplorer(IRibbonControl control)
        {
            MLAddIn.Instance.ShowFormulaExplorer();
        }
    }
}