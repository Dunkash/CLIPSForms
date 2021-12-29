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
            if (currentAssertion != "" && !chancedAssertions.ContainsKey(currentAssertion))
                chancedAssertions.Add(currentAssertion, trackBar1.Value * 0.05);
            SendMessage("Output reset");
            SendMessage($"Selected {Nonterminals.SelectedItems.Count} initial facts");
            Result.Items.Clear();
            //AddAssertions(Nonterminals.SelectedItems);
            AddAssertions();
            EvaluateLoop();
            SendMessage("\n");
        }

        private void Nonterminals_SelectedIndexChanged(object sender, EventArgs e)
        {
            var temp = ToList(Nonterminals.SelectedItems);
            if (temp.Except(previous).Count()==0)
            {
                var t = previous.Except(temp).First();
                if (currentAssertion != "" && !chancedAssertions.ContainsKey(currentAssertion))
                {
                    chancedAssertions.Add(currentAssertion, trackBar1.Value * 0.05);
                }
                currentAssertion = "";
                chancedAssertions.Remove(t);
            }
            else
            {
                var t = (temp.Except(previous).First());
                if (currentAssertion != "" && !chancedAssertions.ContainsKey(currentAssertion))
                {
                    chancedAssertions.Add(currentAssertion, trackBar1.Value * 0.05);
                }
                currentAssertion = t;
            }

            previous = temp;
            trackBar1.Value = 18;
        }
    }
}
