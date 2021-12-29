﻿namespace CLIPSForms
{
    partial class CLIPSForms
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CLIPSForms));
            this.clipsOpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.fontDialog1 = new System.Windows.Forms.FontDialog();
            this.clipsSaveFileDialog = new System.Windows.Forms.SaveFileDialog();
            this.InputField = new System.Windows.Forms.ComboBox();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.splitContainer2 = new System.Windows.Forms.SplitContainer();
            this.splitContainer3 = new System.Windows.Forms.SplitContainer();
            this.splitContainer4 = new System.Windows.Forms.SplitContainer();
            this.Nonterminals = new System.Windows.Forms.ListBox();
            this.trackBar1 = new System.Windows.Forms.TrackBar();
            this.StartButton = new System.Windows.Forms.Button();
            this.Result = new System.Windows.Forms.ListBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.OutputConsole = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).BeginInit();
            this.splitContainer2.Panel1.SuspendLayout();
            this.splitContainer2.Panel2.SuspendLayout();
            this.splitContainer2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer3)).BeginInit();
            this.splitContainer3.Panel1.SuspendLayout();
            this.splitContainer3.Panel2.SuspendLayout();
            this.splitContainer3.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer4)).BeginInit();
            this.splitContainer4.Panel1.SuspendLayout();
            this.splitContainer4.Panel2.SuspendLayout();
            this.splitContainer4.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).BeginInit();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // clipsOpenFileDialog
            // 
            this.clipsOpenFileDialog.Filter = "CLIPS files|*.clp|All files|*.*";
            this.clipsOpenFileDialog.Title = "Открыть файл кода CLIPS";
            // 
            // clipsSaveFileDialog
            // 
            this.clipsSaveFileDialog.Filter = "CLIPS files|*.clp|All files|*.*";
            this.clipsSaveFileDialog.Title = "Созранить файл как...";
            // 
            // InputField
            // 
            this.InputField.Location = new System.Drawing.Point(0, 0);
            this.InputField.Name = "InputField";
            this.InputField.Size = new System.Drawing.Size(121, 24);
            this.InputField.TabIndex = 4;
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(0, 0);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.splitContainer2);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.panel1);
            this.splitContainer1.Size = new System.Drawing.Size(1317, 812);
            this.splitContainer1.SplitterDistance = 439;
            this.splitContainer1.TabIndex = 3;
            // 
            // splitContainer2
            // 
            this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer2.IsSplitterFixed = true;
            this.splitContainer2.Location = new System.Drawing.Point(0, 0);
            this.splitContainer2.Name = "splitContainer2";
            this.splitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer2.Panel1
            // 
            this.splitContainer2.Panel1.Controls.Add(this.splitContainer3);
            // 
            // splitContainer2.Panel2
            // 
            this.splitContainer2.Panel2.Controls.Add(this.Result);
            this.splitContainer2.Size = new System.Drawing.Size(439, 812);
            this.splitContainer2.SplitterDistance = 405;
            this.splitContainer2.TabIndex = 0;
            // 
            // splitContainer3
            // 
            this.splitContainer3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer3.IsSplitterFixed = true;
            this.splitContainer3.Location = new System.Drawing.Point(0, 0);
            this.splitContainer3.Name = "splitContainer3";
            this.splitContainer3.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer3.Panel1
            // 
            this.splitContainer3.Panel1.Controls.Add(this.splitContainer4);
            // 
            // splitContainer3.Panel2
            // 
            this.splitContainer3.Panel2.Controls.Add(this.StartButton);
            this.splitContainer3.Size = new System.Drawing.Size(439, 405);
            this.splitContainer3.SplitterDistance = 373;
            this.splitContainer3.TabIndex = 0;
            // 
            // splitContainer4
            // 
            this.splitContainer4.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer4.IsSplitterFixed = true;
            this.splitContainer4.Location = new System.Drawing.Point(0, 0);
            this.splitContainer4.Name = "splitContainer4";
            this.splitContainer4.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // splitContainer4.Panel1
            // 
            this.splitContainer4.Panel1.Controls.Add(this.Nonterminals);
            this.splitContainer4.Panel1MinSize = 15;
            // 
            // splitContainer4.Panel2
            // 
            this.splitContainer4.Panel2.Controls.Add(this.trackBar1);
            this.splitContainer4.Panel2MinSize = 20;
            this.splitContainer4.Size = new System.Drawing.Size(439, 373);
            this.splitContainer4.SplitterDistance = 344;
            this.splitContainer4.TabIndex = 0;
            // 
            // Nonterminals
            // 
            this.Nonterminals.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.Nonterminals.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Nonterminals.FormattingEnabled = true;
            this.Nonterminals.ItemHeight = 16;
            this.Nonterminals.Location = new System.Drawing.Point(0, 0);
            this.Nonterminals.Name = "Nonterminals";
            this.Nonterminals.SelectionMode = System.Windows.Forms.SelectionMode.MultiSimple;
            this.Nonterminals.Size = new System.Drawing.Size(439, 344);
            this.Nonterminals.TabIndex = 1;
            this.Nonterminals.SelectedIndexChanged += new System.EventHandler(this.Nonterminals_SelectedIndexChanged);
            // 
            // trackBar1
            // 
            this.trackBar1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.trackBar1.Location = new System.Drawing.Point(0, 0);
            this.trackBar1.Maximum = 20;
            this.trackBar1.Name = "trackBar1";
            this.trackBar1.Size = new System.Drawing.Size(439, 25);
            this.trackBar1.TabIndex = 0;
            this.trackBar1.Value = 18;
            // 
            // StartButton
            // 
            this.StartButton.Dock = System.Windows.Forms.DockStyle.Fill;
            this.StartButton.Location = new System.Drawing.Point(0, 0);
            this.StartButton.Name = "StartButton";
            this.StartButton.Size = new System.Drawing.Size(439, 28);
            this.StartButton.TabIndex = 0;
            this.StartButton.Text = "Start";
            this.StartButton.UseVisualStyleBackColor = true;
            this.StartButton.Click += new System.EventHandler(this.StartButton_Click);
            // 
            // Result
            // 
            this.Result.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Result.FormattingEnabled = true;
            this.Result.ItemHeight = 16;
            this.Result.Location = new System.Drawing.Point(0, 0);
            this.Result.Name = "Result";
            this.Result.Size = new System.Drawing.Size(439, 403);
            this.Result.TabIndex = 0;
            // 
            // panel1
            // 
            this.panel1.AutoScroll = true;
            this.panel1.Controls.Add(this.OutputConsole);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(874, 812);
            this.panel1.TabIndex = 0;
            // 
            // OutputConsole
            // 
            this.OutputConsole.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.OutputConsole.Dock = System.Windows.Forms.DockStyle.Fill;
            this.OutputConsole.Font = new System.Drawing.Font("Times New Roman", 11F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.OutputConsole.Location = new System.Drawing.Point(0, 0);
            this.OutputConsole.Name = "OutputConsole";
            this.OutputConsole.Size = new System.Drawing.Size(874, 812);
            this.OutputConsole.TabIndex = 3;
            this.OutputConsole.Text = "\r\n";
            this.OutputConsole.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // CLIPSForms
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1317, 812);
            this.Controls.Add(this.splitContainer1);
            this.Controls.Add(this.InputField);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Margin = new System.Windows.Forms.Padding(4);
            this.MinimumSize = new System.Drawing.Size(874, 358);
            this.Name = "CLIPSForms";
            this.Text = "Экспертная система \"Form 1\"";
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.splitContainer2.Panel1.ResumeLayout(false);
            this.splitContainer2.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).EndInit();
            this.splitContainer2.ResumeLayout(false);
            this.splitContainer3.Panel1.ResumeLayout(false);
            this.splitContainer3.Panel2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer3)).EndInit();
            this.splitContainer3.ResumeLayout(false);
            this.splitContainer4.Panel1.ResumeLayout(false);
            this.splitContainer4.Panel2.ResumeLayout(false);
            this.splitContainer4.Panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer4)).EndInit();
            this.splitContainer4.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).EndInit();
            this.panel1.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion
    private System.Windows.Forms.OpenFileDialog clipsOpenFileDialog;
    private System.Windows.Forms.FontDialog fontDialog1;
    private System.Windows.Forms.SaveFileDialog clipsSaveFileDialog;
        private System.Windows.Forms.ComboBox InputField;
        private System.Windows.Forms.SplitContainer splitContainer1;
        private System.Windows.Forms.SplitContainer splitContainer2;
        private System.Windows.Forms.SplitContainer splitContainer3;
        private System.Windows.Forms.Button StartButton;
        private System.Windows.Forms.ListBox Result;
        private System.Windows.Forms.SplitContainer splitContainer4;
        private System.Windows.Forms.ListBox Nonterminals;
        private System.Windows.Forms.TrackBar trackBar1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label OutputConsole;
    }
}

