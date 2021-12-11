namespace CLIPSForms
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
            this.OutputConsole = new System.Windows.Forms.Label();
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
            this.InputField.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.InputField.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.InputField.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.InputField.Font = new System.Drawing.Font("Times New Roman", 10.8F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.InputField.FormattingEnabled = true;
            this.InputField.Location = new System.Drawing.Point(0, 784);
            this.InputField.Name = "InputField";
            this.InputField.Size = new System.Drawing.Size(1317, 28);
            this.InputField.TabIndex = 1;
            this.InputField.KeyDown += new System.Windows.Forms.KeyEventHandler(this.InputField_KeyDown);
            // 
            // OutputConsole
            // 
            this.OutputConsole.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.OutputConsole.Dock = System.Windows.Forms.DockStyle.Fill;
            this.OutputConsole.Font = new System.Drawing.Font("Times New Roman", 11F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.OutputConsole.Location = new System.Drawing.Point(0, 0);
            this.OutputConsole.Name = "OutputConsole";
            this.OutputConsole.Size = new System.Drawing.Size(1317, 784);
            this.OutputConsole.TabIndex = 2;
            this.OutputConsole.Text = "\r\n";
            this.OutputConsole.TextAlign = System.Drawing.ContentAlignment.BottomLeft;
            // 
            // CLIPSForms
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1317, 812);
            this.Controls.Add(this.OutputConsole);
            this.Controls.Add(this.InputField);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Margin = new System.Windows.Forms.Padding(4);
            this.MinimumSize = new System.Drawing.Size(874, 358);
            this.Name = "CLIPSForms";
            this.Text = "Экспертная система \"Form 1\"";
            this.ResumeLayout(false);

        }

        #endregion
    private System.Windows.Forms.OpenFileDialog clipsOpenFileDialog;
    private System.Windows.Forms.FontDialog fontDialog1;
    private System.Windows.Forms.SaveFileDialog clipsSaveFileDialog;
        private System.Windows.Forms.Label OutputConsole;
        private System.Windows.Forms.ComboBox InputField;
    }
}

