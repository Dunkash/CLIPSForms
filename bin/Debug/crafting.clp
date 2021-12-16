;;;*****************
;;;* Configuration *
;;;*****************
   
(defglobal ?*target* = gui) ; console, cgi, or gui

;;; ***************************
;;; * DEFTEMPLATES & DEFFACTS *
;;; ***************************

(deftemplate MAIN::text-for-id
   (slot id)
   (slot text))

(deftemplate UI-state
   (slot id (default-dynamic (gensym*)))
   (slot display)
   (slot relation-asserted (default none))
   (slot response (default none))
   (multislot valid-answers)
   (multislot display-answers)
   (slot state (default middle))
   (multislot additional-asserts))
   
;;;***************************
;;;* DEFFUNCTION DEFINITIONS *
;;;***************************

(deffunction MAIN::find-text-for-id (?id)
   ;; Search for the text-for-id fact
   ;; with the same id as ?id
   (bind ?fact
      (find-fact ((?f text-for-id))
                  (eq ?f:id ?id)))
   (if ?fact
      then
      (fact-slot-value (nth$ 1 ?fact) text)
      else
      ?id))
      
(deffunction MAIN::translate-av (?values)
   ;; Create the return value
   (bind ?result (create$))
   ;; Iterate over each of the allowed-values
   (progn$ (?v ?values)
      ;; Find the associated text-for-id fact
      (bind ?nv
         (find-text-for-id ?v))
      ;; Add the text to the return value
      (bind ?result (create$ ?result ?nv)))
   ;; Return the return value
   ?result)

(deffunction MAIN::replace-spaces (?str)
   (bind ?len (str-length ?str))
   (bind ?i (str-index " " ?str))
   (while (neq ?i FALSE)
      (bind ?str (str-cat (sub-string 1 (- ?i 1) ?str) "-" (sub-string (+ ?i 1) ?len ?str)))
      (bind ?i (str-index " " ?str)))
   ?str)

(deffunction MAIN::sym-cat-multifield (?values)
   (bind ?rv (create$))
   (progn$ (?v ?values)
      (bind ?rv (create$ ?rv (sym-cat (replace-spaces ?v)))))
   ?rv)

(deffunction MAIN::multifield-to-delimited-string (?mv ?delimiter)
   (bind ?rv "")
   (bind ?first TRUE)
   (progn$ (?v ?mv)
      (if ?first
         then
         (bind ?first FALSE)
         (bind ?rv (str-cat ?v))
         else
         (bind ?rv (str-cat ?rv ?delimiter ?v))))
   ?rv)

;;;*****************
;;;* STATE METHODS *
;;;*****************
      
;;; Console target
   
(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target console))
                         (?display LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (printout t ?display crlf)
   (str-assert (str-cat "(" ?relation-asserted " " yes ")")))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target console))
                         (?question LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE) ; default
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (bind ?display-answers (sym-cat-multifield ?display-answers))
   (format t "%s " ?question)
   (printout t ?display-answers " ")
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?display-answers)) do
      (format t "%s " ?question)
      (printout t ?display-answers " ")
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   (bind ?pos (member$ ?answer ?display-answers))
   (bind ?answer (nth$ ?pos ?valid-answers))
   (str-assert (str-cat "(" ?relation-asserted " " ?answer ")")))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target console))
                         (?display LEXEME))
   (assert (conclusion))
   (printout t ?display crlf)
   (halt))

;;; CGI target

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target cgi))
                         (?display LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (printout t "state=greeting" crlf)
   (printout t "display=" ?display crlf)
   (printout t "variable=greeting" crlf)
   (printout t "validAnswers=yes" crlf)
   (printout t "displayAnswers=yes" crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target cgi))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (printout t "state=interview" crlf)
   (printout t "display=" ?message crlf)  
   (printout t "variable=" ?relation-asserted crlf)
   (printout t "validAnswers=" (multifield-to-delimited-string ?valid-answers ":") crlf)
   (printout t "displayAnswers=" (multifield-to-delimited-string ?display-answers ":") crlf) 
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target cgi))
                         (?display LEXEME))
   (printout t "state=conclusion" crlf)
   (printout t "display=" ?display crlf)
   (printout t "prevLabel=" (find-text-for-id Prev) crlf)
   (printout t "nextLabel=" (find-text-for-id Next) crlf)
   (printout t "restartLabel=" (find-text-for-id Restart) crlf)
   (printout t "autoDemoLabel=" (find-text-for-id AutoDemo) crlf)
   (assert (conclusion))
   (halt))

;;; GUI target (iOS and JNI)

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?valid-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted greeting)
                     (state ?state)
                     (valid-answers proceed)
                     (display-answers proceed)))
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD)
                         (?additional-asserts MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (response ?response)
                     (valid-answers ?valid-answers)
                     (display-answers ?display-answers)
                     (additional-asserts ?additional-asserts)))
   (halt))

   (defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?response PRIMITIVE)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (response ?response)
                     (valid-answers ?valid-answers)
                     (display-answers ?display-answers)))
   (halt))
 
(defmethod handle-state ((?state SYMBOL (eq ?state conclusion))
                         (?target SYMBOL (eq ?target gui))
                         (?display LEXEME))
   (assert (UI-state (display ?display)
                     (state ?state)
                     (valid-answers)
                     (display-answers)))
   (assert (conclusion))
   (halt))

;;;****************
;;;* STARTUP RULE *
;;;****************

(defrule system-banner ""
  (not (greeting proceed))
  =>
  (handle-state greeting
                ?*target*
                (find-text-for-id WelcomeMessage)
                greeting
                (create$)))

;;;***************
;;;* QUERY RULES *
;;;***************

(defrule initial-question ""

   (greeting proceed)
   (not (crafting ?))
   
   =>

   (bind ?answers (create$ board wood workbench stick woodpick stonepick ironpick forge diamond))
   (handle-state interview
                 ?*target*
                 (find-text-for-id InitialQuestion)
                 no
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(crafting shit)" )
                 ))

(defrule craft-board ""

   (board no)
   (not (board proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftBoard)
                 board
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(wood ?)" ))
 )


(defrule craft-wood ""

   (wood no)
   (not (wood proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftWood)
                 wood
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)))

(defrule craft-stone ""

   (stone no)
   (not (stone proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftStone)
                 stone
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(woodpick ?)")))


(defrule craft-workbench ""

   (workbench no)
   (not (workbench proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftWorkbench)
                 workbench
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(board ?)")))

(defrule craft-stick ""

   (stick no)
   (not (stick proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftStick)
                 stick
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(board ?)")))

(defrule craft-wood-pick ""

   (woodpick no)
   (not (woodpick proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftWoodPick)
                 woodpick
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(board ?)" "(stick ?)" "(workbench ?)")))

(defrule craft-stone-pick ""

   (stonepick no)
   (not (stonepick proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftStonePick)
                 stonepick
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(stone ?)" "(stick ?)" "(workbench ?)")))

(defrule craft-iron-pick ""

   (ironpick no)
   (not (ironpick proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftIronPick)
                 ironpick
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(iron ?)" "(stick ?)" "(workbench ?)")))

(defrule craft-iron ""

   (iron no)
   (not (iron proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftIron)
                 iron
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(forge ?)" "(ironore ?)" "(board ?)")))

(defrule craft-iron-ore ""

   (ironore no)
   (not (ironore proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftIronOre)
                 ironore
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(stonepick ?)")))

(defrule craft-forge ""

   (forge no)
   (not (forge proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftForge)
                 forge
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(stone ?)")))

(defrule craft-diamond ""

   (diamond no)
   (not (diamond proceed))
   =>

   (bind ?answers (create$ proceed))
   (handle-state interview
                 ?*target*
                 (find-text-for-id CraftDiamond)
                 diamond
                 (nth$ 1 ?answers)
                 ?answers
                 (translate-av ?answers)
                 (create$ "(ironpick ?)")))

;;;******************
;;;* CRAFTING RULES *
;;;******************

(defrule end ""
   (declare (salience -1))
   =>
   (handle-state conclusion ?*target* (find-text-for-id End)))
