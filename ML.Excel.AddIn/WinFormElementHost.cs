namespace ML.Excel.AddIn
{
    using System.Runtime.InteropServices;
    using System.Windows.Forms;
    using System.Windows.Forms.Integration;
    using WPFControls = System.Windows.Controls;

    [ComVisible(true)]
    public partial class WinFormElementHost : UserControl
    {
        public WinFormElementHost()
        {
            InitializeComponent();
            var elementHost = new ElementHost {Dock = DockStyle.Fill};
            this.Controls.Add(elementHost);
        }

        public WPFControls.UserControl Control
        {
            get { return ((ElementHost)this.Controls[0]).Child as WPFControls.UserControl; }
            set { ((ElementHost) this.Controls[0]).Child = value; }
        }
    }
}
