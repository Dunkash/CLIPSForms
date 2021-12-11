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
    partial class CLIPSForms : Form
    {
        /// <summary>
        /// Possible states are:
        /// <list type="bullet">
        /// <item>
        /// <description>Greeting. Initial state, no evaluations made yet.</description>
        /// </item>
        /// <item>
        /// <description>Interview. System asks questions, and retreives answers. Evaluations are processed.</description>
        /// </item>
        ///  <item>
        /// <description>Conclusion. Return result of evaluations.</description>
        /// </item>
        /// </list>
        /// </summary>
        private enum InterviewState { GREETING, INTERVIEW, CONCLUSION };

        private CLIPSNET.Environment clips = new CLIPSNET.Environment();
        private String lastAnswer = null;
        private String relationAsserted = null;
        private List<string> variableAsserts = new List<string>();
        private List<string> priorAnswers = new List<string>();

        /// <summary>
        /// Current state of the envinronment.
        /// </summary>
        private InterviewState currentState;


        ///<summary> 
        ///Loads initial CLIPS files, required for basic functions, then resets envinronment to insert files into CLIPS envinronment. 
        ///</summary>
        void LoadInitial()
        {
            clips.Load(@"auto.clp");
            clips.Load(@"auto_en.clp");
            clips.Reset();
        }

        /// <summary>
        /// Initial rules processing, setting initial facts, then running and processing result.
        /// </summary>
        private void ProcessRules()
        {
            clips.Reset();
            foreach (String factString in variableAsserts)
            {
                String assertCommand = "(assert " + factString + ")";
                clips.Eval(assertCommand);
            }
            clips.Run();
            HandleResponse();
        }

        private void HandleResponse()
        {
            /*===========================*/
            /* Get the current UI state. */
            /*===========================*/

            String evalStr = "(find-fact ((?f UI-state)) TRUE)";
            FactAddressValue fv = (FactAddressValue)((MultifieldValue)clips.Eval(evalStr))[0];

            /*========================================*/
            /* Determine the Next/Prev button states. */
            /*========================================*/

            if (fv["state"].ToString().Equals("conclusion"))
            {

            }
            else if (fv["state"].ToString().Equals("greeting"))
            {

            }
            else
            {

            }

            /*=====================*/
            /* Set up the choices. */
            /*=====================*/

            MultifieldValue damf = (MultifieldValue)fv["display-answers"];
            MultifieldValue vamf = (MultifieldValue)fv["valid-answers"];

            String selected = fv["response"].ToString();

            for (int i = 0; i < damf.Count; i++)
            {
                LexemeValue da = (LexemeValue)damf[i];
                LexemeValue va = (LexemeValue)vamf[i];
            }


            /*====================================*/
            /* Set the label to the display text. */
            /*====================================*/


            /*====================================*/
            /* Set the label to the display text. */
            /*====================================*/

        }

    }
}