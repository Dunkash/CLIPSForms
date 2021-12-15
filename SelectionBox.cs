using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace CLIPSForms
{


    public partial class SelectionBox : Form
    {
        public List<String> Result { get; set; } = new List<string>();
        public SelectionBox(List<string> options)
        {
            InitializeComponent();
            InitializeSelection(options);
        }

        private void InitializeSelection(List<string> options)
        {
            Selection.Items.Clear();
            foreach (var option in options)
            {
                Selection.Items.Add(option);
            }
        }



        private string MakeAssertion(string assertion, bool owns)
        {
            return owns ? $"({assertion} yes)" : $"({assertion} no)";
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void SelectionBox_FormClosing(object sender, FormClosingEventArgs e)
        {
            for (var index = 0; index < Selection.Items.Count; index++)
            {
                if (Selection.SelectedIndices.Contains(index))
                    Result.Add(MakeAssertion(Selection.Items[index].ToString(), true));
                else
                    Result.Add(MakeAssertion(Selection.Items[index].ToString(), false));
            }
        }
    }
}
