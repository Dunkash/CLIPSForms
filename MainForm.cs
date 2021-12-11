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
            SetInputOptions();
            SendMessage(GetMessage());
        }


        /// <summary>
        /// Sends message in a string format to console.
        /// </summary>
        /// <param name="message">Message to send</param>
        private void SendMessage(string message)
        {
            OutputConsole.Text += "\n";
            OutputConsole.Text += message;
        }


        /// <summary>
        /// Sets possible values in input field according to evaluated responses.
        /// </summary>
        private void SetInputOptions()
        {
            InputField.Items.Clear();
            InputField.Items.AddRange(GetPossibleAnswer());
            if (InputField.Items.Count!=0)
                InputField.SelectedIndex = 0;
        }

        private void NextBtn_Click(object sender, EventArgs e)
        {
        }

        private void ResetBtn_Click(object sender, EventArgs e)
        {
        }

        private void OpenFile_Click(object sender, EventArgs e)
        {
        }

        private void FontSelect_Click(object sender, EventArgs e)
        {
            /*
            if (fontDialog1.ShowDialog() == DialogResult.OK)
            {
                codeBox.Font = fontDialog1.Font;
                outputBox.Font = fontDialog1.Font;
            }*/
        }

        private void SaveAsButton_Click(object sender, EventArgs e)
        {
            /*
            clipsSaveFileDialog.FileName = clipsOpenFileDialog.FileName;
            if (clipsSaveFileDialog.ShowDialog() == DialogResult.OK)
            {
                System.IO.File.WriteAllText(clipsSaveFileDialog.FileName, codeBox.Text);
            }
            */
        }

        private void InputField_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter)
            {
                SendMessage(InputField.Text);

                if (InputHandler(InputField.Text))
                {
                    SetInputOptions();
                    SendMessage(GetMessage());
                }
                else
                    SendMessage("Incorrect input");
            }
        }
    }
}
