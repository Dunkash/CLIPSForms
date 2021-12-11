using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.IO;

using CLIPSNET;


namespace CLIPSForms
{
    public partial class CLIPSForms : Form
    {
        public CLIPSForms()
        {
            InitializeComponent();
            LoadInitial();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            ProcessRules();
        }

        private void nextBtn_Click(object sender, EventArgs e)
        {
        }

        private void resetBtn_Click(object sender, EventArgs e)
        {
        }

        private void openFile_Click(object sender, EventArgs e)
        {
        }

        private void fontSelect_Click(object sender, EventArgs e)
        {
            if (fontDialog1.ShowDialog() == DialogResult.OK)
            {
                codeBox.Font = fontDialog1.Font;
                outputBox.Font = fontDialog1.Font;
            }
        }

        private void saveAsButton_Click(object sender, EventArgs e)
        {
            clipsSaveFileDialog.FileName = clipsOpenFileDialog.FileName;
            if (clipsSaveFileDialog.ShowDialog() == DialogResult.OK)
            {
                System.IO.File.WriteAllText(clipsSaveFileDialog.FileName, codeBox.Text);
            }
        }

    }
}
