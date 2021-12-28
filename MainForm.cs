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
            //InputField.Items.Clear();
            //InputField.Items.AddRange(GetPossibleAnswer());
            Nonterminals.Items.Clear();
            Nonterminals.Items.AddRange(GetPossibleAnswer());
            if (InputField.Items.Count!=0)
                InputField.SelectedIndex = 0;
        }

        private void StartButton_Click(object sender, EventArgs e)
        {
            SendMessage("Output reset");
            SendMessage($"Selected {Nonterminals.SelectedItems.Count} initial facts");
            Result.Items.Clear();
            AddAssertions(Nonterminals.SelectedItems);
            EvaluateLoop();
        }


    }
}
