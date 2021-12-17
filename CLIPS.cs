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
        private int SucessfulRules { get; set; } = 0;
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

        /// <summary>
        /// Current state of the envinronment.
        /// </summary>
        private InterviewState currentState;

        /// <summary>
        /// Current clips envinronment
        /// </summary>
        private readonly CLIPSNET.Environment clips = new CLIPSNET.Environment();

        //private String lastAnswer = null;
        private String relationAsserted = null;
        private readonly List<string> variableAsserts = new List<string>();
        private readonly List<string> priorAnswers = new List<string>();

        private readonly List<string> results = new List<string>();

        private readonly List<string> checkOwns = new List<string>();

        private readonly HashSet<string> pastChecks = new HashSet<string>();


        /// <summary>
        /// List of currently possible answers, as well as CLIPS commands, connected to them
        /// </summary>
        private Dictionary<string, string> answers = null;

        private string currentMessage = "";

        public List<string> CheckOwns
        {
            get => checkOwns;
        }

        ///<summary> 
        ///Loads initial CLIPS files, required for basic functions, then resets envinronment to insert files into CLIPS envinronment. 
        ///</summary>
        void LoadInitial()
        {
            clips.Load(@"crafting.clp");
            clips.Load(@"crafting_en.clp");
            clips.Reset();
        }

        /// <summary>
        /// Generates array of possible answers for current state
        /// </summary>
        /// <returns>Array of string answer possibilities</returns>
        private string[] GetPossibleAnswer()
        {
            return answers.Keys.ToArray();
        }

        private bool NeedCheck()
        {
            return checkOwns.Count() != 0;
        }

        private void RefreshChecks()
        {
            foreach (var value in checkOwns)
                pastChecks.Add(value);
            checkOwns.Clear();
        }

        private void AddAssertions(List<string> assertions)
        {
            foreach (var assertion in assertions)
                variableAsserts.Add(answers[assertion]);
        }

        private void AddAssertions(ListBox.SelectedObjectCollection assertions)
        {
            foreach (var assertion in assertions)
                variableAsserts.Add($"( {answers[assertion.ToString()]} )");
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

        /// <summary>
        /// Pushes the asserts, located in the "Additional asserts" multislot of UI class of CLIPS file, into the application.
        /// </summary>
        /// <param name="fv">UI class adress</param>
        private void AdditionalAsserts(FactAddressValue fv)
        {
            MultifieldValue damf = (MultifieldValue)fv["additional-asserts"];
            foreach (var assertion in damf)
            {
                var assTrimmed = assertion.ToString().Trim(new char[] {'\\','"' });
                if (assTrimmed.Contains("?"))
                {
                    var value = assTrimmed.Split()[0].Substring(1);
                    if (!pastChecks.Contains(value))
                        checkOwns.Add(value);
                }
                else
                    variableAsserts.Add(assTrimmed);
            }
        }

        private void GetResults(FactAddressValue fv)
        {
            MultifieldValue damf = (MultifieldValue)fv["additional-asserts"];
            foreach (var assertion in damf)
            {
                var assTrimmed = assertion.ToString().Trim(new char[] { '\\', '"' });
                variableAsserts.Add(assTrimmed);
                if (!Result.Items.Contains((((StringValue)fv["display"]).Value)))
                Result.Items.Add(((StringValue)fv["display"]).Value);
            }
        }

        /// <summary>
        /// Handles the CLIPS response from CLIPS to NET proxy channel.
        /// Sets response message from CLIPS to UI, sets list of possible answers and connections between answers and facts.
        /// </summary>
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

            SetCurrentState(fv);

            /*=====================*/
            /* Set up the choices. */
            /*=====================*/

            MultifieldValue damf = (MultifieldValue)fv["display-answers"];
            MultifieldValue vamf = (MultifieldValue)fv["valid-answers"];

            //String selected = fv["response"].ToString();

            answers = new Dictionary<string, string>();

            for (int i = 0; i < damf.Count; i++)
            {
                LexemeValue da = (LexemeValue)damf[i];
                LexemeValue va = (LexemeValue)vamf[i];

                answers.Add(da.Value, va.Value);
            }
            //AdditionalAsserts(fv);
            GetResults(fv);
            relationAsserted = ((LexemeValue)fv["relation-asserted"]).Value;

            currentMessage = ((StringValue)fv["display"]).Value;
        }


        /// <summary>
        /// Gets UI response for current situation
        /// </summary>
        /// <returns>Message for message box</returns>
        private string GetMessage()
        {
            return currentMessage;
        }

        /// <summary>
        /// Sets current system state based.
        /// </summary>
        /// <param name="fv">CLIPS to NET proxy adress</param>
        private void SetCurrentState(FactAddressValue fv)
        {
            if (fv["state"].ToString().Equals("conclusion"))
            {
                currentState = InterviewState.CONCLUSION;
            }
            else if (fv["state"].ToString().Equals("greeting"))
            {
                currentState = InterviewState.GREETING;
            }
            else
            {
                currentState = InterviewState.INTERVIEW;
            }
        }

        /// <summary>
        /// Handles inputs from "console".
        /// </summary>
        /// <param name="response">User input for current state</param>
        private bool InputHandler(string response)
        {
            string theString;

            switch (currentState)
            {
                case InterviewState.GREETING:
                {
                        theString = "(" + relationAsserted + " " + response + ")";
                        variableAsserts.Add(theString);
                        priorAnswers.Add(response);
                        break;
                }
                case InterviewState.INTERVIEW:
                {
                        variableAsserts.Add($"({relationAsserted})");
                        break;
                }
                case InterviewState.CONCLUSION:
                    { 
                        variableAsserts.Clear();
                        priorAnswers.Clear();
                        checkOwns.Clear();
                        pastChecks.Clear();
                        SucessfulRules = 0;
                        answers.Clear();

                        break;
                    }
            }

            ProcessRules();
            return true;
        }

    }
}