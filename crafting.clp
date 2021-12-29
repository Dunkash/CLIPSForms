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
                         (?message LEXEME))
   (assert (UI-state (display ?message)
                     (relation-asserted greeting)
                     (state ?state)
                     (valid-answers proceed)
                     (display-answers proceed)))
   (halt))

(defmethod handle-state ((?state SYMBOL (eq ?state greeting))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?valid-answers MULTIFIELD)
                         (?display-answers MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted greeting)
                     (state ?state)
                     (valid-answers ?valid-answers)
                     (display-answers ?display-answers)))
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

   (defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL)
                         (?additional-asserts MULTIFIELD))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (valid-answers)
                     (display-answers)
                     (additional-asserts ?additional-asserts)))
   (halt))

      (defmethod handle-state ((?state SYMBOL (eq ?state interview))
                         (?target SYMBOL (eq ?target gui))
                         (?message LEXEME)
                         (?relation-asserted SYMBOL))
   (assert (UI-state (display ?message)
                     (relation-asserted ?relation-asserted)
                     (state ?state)
                     (valid-answers)
                     (display-answers)
                     (additional-asserts)))
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

;;;************
;;;* END RULE *
;;;************

(defrule end ""
   (declare (salience -1))
   =>
   (handle-state conclusion ?*target* (find-text-for-id End)))

;;;**************
;;;* WORK RULES *
;;;**************




(defrule system-banner ""
	(not (greeting proceed))
	=>
	(bind ?answers (create$ wood stone ironore goldenore diamond ancientwreckage stick sand coal redstone fire water flower obsidian honeycomb flint feather snowball gunpowder clay pumpkin slime vines bonemeal carrot wheat beans cane egg milk apple laminaria thread bamboo firerod endermanpearl dye ink glowstonedust turtleshell backpack ))
  (handle-state greeting 
	?*target* 
	(find-text-for-id WelcomeMessage) 
	?answers 
	(translate-av ?answers) ))


(defrule craft-board ""

	(backpack ?v0)
	(wood ?v1)
	(not (craftboard))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id wood) " => " (find-text-for-id board) "  " (* 100 ?v0 ?v1 ) "%")
			craftboard
			(create$ (str-cat "(board " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftboard
		)
	)	
)

(defrule craft-workbench ""

	(backpack ?v0)
	(board ?v1)
	(not (craftworkbench))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id board) " => " (find-text-for-id workbench) "  " (* 100 ?v0 ?v1 ) "%")
			craftworkbench
			(create$ (str-cat "(workbench " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftworkbench
		)
	)	
)

(defrule craft-torch ""

	(backpack ?v0)
	(stick ?v1)
	(coal ?v2)
	(not (crafttorch))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id stick) ", " (find-text-for-id coal) " => " (find-text-for-id torch) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			crafttorch
			(create$ (str-cat "(torch " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			crafttorch
		)
	)	
)

(defrule craft-redstonetorch ""

	(backpack ?v0)
	(stick ?v1)
	(redstone ?v2)
	(not (craftredstonetorch))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id stick) ", " (find-text-for-id redstone) " => " (find-text-for-id redstonetorch) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftredstonetorch
			(create$ (str-cat "(redstonetorch " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftredstonetorch
		)
	)	
)

(defrule craft-lighter ""

	(backpack ?v0)
	(flint ?v1)
	(iron ?v2)
	(not (craftlighter))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id flint) ", " (find-text-for-id iron) " => " (find-text-for-id lighter) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftlighter
			(create$ (str-cat "(lighter " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftlighter
		)
	)	
)

(defrule craft-snowblock ""

	(backpack ?v0)
	(snowball ?v1)
	(not (craftsnowblock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id snowball) " => " (find-text-for-id snowblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftsnowblock
			(create$ (str-cat "(snowblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftsnowblock
		)
	)	
)

(defrule craft-clayblock ""

	(backpack ?v0)
	(clay ?v1)
	(not (craftclayblock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id clay) " => " (find-text-for-id clayblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftclayblock
			(create$ (str-cat "(clayblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftclayblock
		)
	)	
)

(defrule craft-jackslamp ""

	(backpack ?v0)
	(pumpkin ?v1)
	(torch ?v2)
	(not (craftjackslamp))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id pumpkin) ", " (find-text-for-id torch) " => " (find-text-for-id jackslamp) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftjackslamp
			(create$ (str-cat "(jackslamp " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftjackslamp
		)
	)	
)

(defrule craft-stonebricks ""

	(backpack ?v0)
	(stone ?v1)
	(not (craftstonebricks))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id stone) " => " (find-text-for-id stonebricks) "  " (* 100 ?v0 ?v1 ) "%")
			craftstonebricks
			(create$ (str-cat "(stonebricks " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftstonebricks
		)
	)	
)

(defrule craft-mossystone ""

	(backpack ?v0)
	(stone ?v1)
	(vines ?v2)
	(not (craftmossystone))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id stone) ", " (find-text-for-id vines) " => " (find-text-for-id mossystone) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftmossystone
			(create$ (str-cat "(mossystone " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftmossystone
		)
	)	
)

(defrule craft-rodwithcarrot ""

	(backpack ?v0)
	(carrot ?v1)
	(fishingrod ?v2)
	(not (craftrodwithcarrot))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id carrot) ", " (find-text-for-id fishingrod) " => " (find-text-for-id rodwithcarrot) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftrodwithcarrot
			(create$ (str-cat "(rodwithcarrot " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftrodwithcarrot
		)
	)	
)

(defrule craft-sugar ""

	(backpack ?v0)
	(cane ?v1)
	(not (craftsugar))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id cane) " => " (find-text-for-id sugar) "  " (* 100 ?v0 ?v1 ) "%")
			craftsugar
			(create$ (str-cat "(sugar " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftsugar
		)
	)	
)

(defrule craft-wheat ""

	(backpack ?v0)
	(sheafofhay ?v1)
	(not (craftwheat))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id sheafofhay) " => " (find-text-for-id wheat) "  " (* 100 ?v0 ?v1 ) "%")
			craftwheat
			(create$ (str-cat "(wheat " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftwheat
		)
	)	
)

(defrule craft-firepowder ""

	(backpack ?v0)
	(firerod ?v1)
	(not (craftfirepowder))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id firerod) " => " (find-text-for-id firepowder) "  " (* 100 ?v0 ?v1 ) "%")
			craftfirepowder
			(create$ (str-cat "(firepowder " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftfirepowder
		)
	)	
)

(defrule craft-endermaneye ""

	(backpack ?v0)
	(endermanpearl ?v1)
	(firepowder ?v2)
	(not (craftendermaneye))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id endermanpearl) ", " (find-text-for-id firepowder) " => " (find-text-for-id endermaneye) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftendermaneye
			(create$ (str-cat "(endermaneye " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftendermaneye
		)
	)	
)

(defrule craft-lavacream ""

	(backpack ?v0)
	(slime ?v1)
	(firepowder ?v2)
	(not (craftlavacream))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id slime) ", " (find-text-for-id firepowder) " => " (find-text-for-id lavacream) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftlavacream
			(create$ (str-cat "(lavacream " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftlavacream
		)
	)	
)

(defrule craft-star ""

	(backpack ?v0)
	(gunpowder ?v1)
	(dye ?v2)
	(not (craftstar))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id gunpowder) ", " (find-text-for-id dye) " => " (find-text-for-id star) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftstar
			(create$ (str-cat "(star " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftstar
		)
	)	
)

(defrule craft-stick ""

	(backpack ?v0)
	(bamboo ?v1)
	(not (craftstick))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id bamboo) " => " (find-text-for-id stick) "  " (* 100 ?v0 ?v1 ) "%")
			craftstick
			(create$ (str-cat "(stick " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftstick
		)
	)	
)

(defrule craft-magmablock ""

	(backpack ?v0)
	(lavacream ?v1)
	(not (craftmagmablock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id lavacream) " => " (find-text-for-id magmablock) "  " (* 100 ?v0 ?v1 ) "%")
			craftmagmablock
			(create$ (str-cat "(magmablock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftmagmablock
		)
	)	
)

(defrule craft-glowstonedust ""

	(backpack ?v0)
	(glowstone ?v1)
	(not (craftglowstonedust))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id backpack) ", " (find-text-for-id glowstone) " => " (find-text-for-id glowstonedust) "  " (* 100 ?v0 ?v1 ) "%")
			craftglowstonedust
			(create$ (str-cat "(glowstonedust " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftglowstonedust
		)
	)	
)

(defrule craft-furnace ""

	(workbench ?v0)
	(stone ?v1)
	(not (craftfurnace))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id stone) " => " (find-text-for-id furnace) "  " (* 100 ?v0 ?v1 ) "%")
			craftfurnace
			(create$ (str-cat "(furnace " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftfurnace
		)
	)	
)

(defrule craft-chest ""

	(workbench ?v0)
	(wood ?v1)
	(not (craftchest))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id wood) " => " (find-text-for-id chest) "  " (* 100 ?v0 ?v1 ) "%")
			craftchest
			(create$ (str-cat "(chest " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftchest
		)
	)	
)

(defrule craft-boiler ""

	(workbench ?v0)
	(iron ?v1)
	(not (craftboiler))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) " => " (find-text-for-id boiler) "  " (* 100 ?v0 ?v1 ) "%")
			craftboiler
			(create$ (str-cat "(boiler " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftboiler
		)
	)	
)

(defrule craft-meltingfurnace ""

	(workbench ?v0)
	(iron ?v1)
	(stone ?v2)
	(furnace ?v3)
	(not (craftmeltingfurnace))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) ", " (find-text-for-id stone) ", " (find-text-for-id furnace) " => " (find-text-for-id meltingfurnace) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftmeltingfurnace
			(create$ (str-cat "(meltingfurnace " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftmeltingfurnace
		)
	)	
)

(defrule craft-smokehouse ""

	(workbench ?v0)
	(furnace ?v1)
	(wood ?v2)
	(not (craftsmokehouse))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id furnace) ", " (find-text-for-id wood) " => " (find-text-for-id smokehouse) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftsmokehouse
			(create$ (str-cat "(smokehouse " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftsmokehouse
		)
	)	
)

(defrule craft-stonecutter ""

	(workbench ?v0)
	(iron ?v1)
	(stone ?v2)
	(not (craftstonecutter))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) ", " (find-text-for-id stone) " => " (find-text-for-id stonecutter) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftstonecutter
			(create$ (str-cat "(stonecutter " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftstonecutter
		)
	)	
)

(defrule craft-anvil ""

	(workbench ?v0)
	(iron ?v1)
	(ironblock ?v2)
	(not (craftanvil))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) ", " (find-text-for-id ironblock) " => " (find-text-for-id anvil) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftanvil
			(create$ (str-cat "(anvil " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftanvil
		)
	)	
)

(defrule craft-hive ""

	(workbench ?v0)
	(wood ?v1)
	(honeycomb ?v2)
	(not (crafthive))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id wood) ", " (find-text-for-id honeycomb) " => " (find-text-for-id hive) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			crafthive
			(create$ (str-cat "(hive " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			crafthive
		)
	)	
)

(defrule craft-blacksmithstable ""

	(workbench ?v0)
	(iron ?v1)
	(wood ?v2)
	(not (craftblacksmithstable))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) ", " (find-text-for-id wood) " => " (find-text-for-id blacksmithstable) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftblacksmithstable
			(create$ (str-cat "(blacksmithstable " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftblacksmithstable
		)
	)	
)

(defrule craft-cartographerstable ""

	(workbench ?v0)
	(paper ?v1)
	(board ?v2)
	(not (craftcartographerstable))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id paper) ", " (find-text-for-id board) " => " (find-text-for-id cartographerstable) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftcartographerstable
			(create$ (str-cat "(cartographerstable " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftcartographerstable
		)
	)	
)

(defrule craft-archerstable ""

	(workbench ?v0)
	(flint ?v1)
	(board ?v2)
	(not (craftarcherstable))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id flint) ", " (find-text-for-id board) " => " (find-text-for-id archerstable) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftarcherstable
			(create$ (str-cat "(archerstable " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftarcherstable
		)
	)	
)

(defrule craft-hopper ""

	(workbench ?v0)
	(iron ?v1)
	(chest ?v2)
	(not (crafthopper))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) ", " (find-text-for-id chest) " => " (find-text-for-id hopper) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			crafthopper
			(create$ (str-cat "(hopper " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			crafthopper
		)
	)	
)

(defrule craft-torch_ ""

	(workbench ?v0)
	(stick ?v1)
	(coal ?v2)
	(not (crafttorch_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id stick) ", " (find-text-for-id coal) " => " (find-text-for-id torch) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			crafttorch_
			(create$ (str-cat "(torch " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			crafttorch_
		)
	)	
)

(defrule craft-leash ""

	(workbench ?v0)
	(thread ?v1)
	(slime ?v2)
	(not (craftleash))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id thread) ", " (find-text-for-id slime) " => " (find-text-for-id leash) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftleash
			(create$ (str-cat "(leash " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftleash
		)
	)	
)

(defrule craft-helmet ""

	(workbench ?v0)
	(turtleshell ?v1)
	(not (crafthelmet))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id turtleshell) " => " (find-text-for-id helmet) "  " (* 100 ?v0 ?v1 ) "%")
			crafthelmet
			(create$ (str-cat "(helmet " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			crafthelmet
		)
	)	
)

(defrule craft-campfire ""

	(workbench ?v0)
	(stick ?v1)
	(coal ?v2)
	(wood ?v3)
	(not (craftcampfire))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id stick) ", " (find-text-for-id coal) ", " (find-text-for-id wood) " => " (find-text-for-id campfire) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftcampfire
			(create$ (str-cat "(campfire " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftcampfire
		)
	)	
)

(defrule craft-ironblock ""

	(workbench ?v0)
	(iron ?v1)
	(not (craftironblock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) " => " (find-text-for-id ironblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftironblock
			(create$ (str-cat "(ironblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftironblock
		)
	)	
)

(defrule craft-goldenblock ""

	(workbench ?v0)
	(gold ?v1)
	(not (craftgoldenblock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id gold) " => " (find-text-for-id goldenblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftgoldenblock
			(create$ (str-cat "(goldenblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgoldenblock
		)
	)	
)

(defrule craft-diamondblock ""

	(workbench ?v0)
	(diamond ?v1)
	(not (craftdiamondblock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id diamond) " => " (find-text-for-id diamondblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftdiamondblock
			(create$ (str-cat "(diamondblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftdiamondblock
		)
	)	
)

(defrule craft-snowblock_ ""

	(workbench ?v0)
	(snowball ?v1)
	(not (craftsnowblock_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id snowball) " => " (find-text-for-id snowblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftsnowblock_
			(create$ (str-cat "(snowblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftsnowblock_
		)
	)	
)

(defrule craft-clayblock_ ""

	(workbench ?v0)
	(clay ?v1)
	(not (craftclayblock_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id clay) " => " (find-text-for-id clayblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftclayblock_
			(create$ (str-cat "(clayblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftclayblock_
		)
	)	
)

(defrule craft-boneblock ""

	(workbench ?v0)
	(bonemeal ?v1)
	(not (craftboneblock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id bonemeal) " => " (find-text-for-id boneblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftboneblock
			(create$ (str-cat "(boneblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftboneblock
		)
	)	
)

(defrule craft-iron ""

	(workbench ?v0)
	(ironblock ?v1)
	(not (craftiron))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id ironblock) " => " (find-text-for-id iron) "  " (* 100 ?v0 ?v1 ) "%")
			craftiron
			(create$ (str-cat "(iron " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftiron
		)
	)	
)

(defrule craft-gold ""

	(workbench ?v0)
	(goldenblock ?v1)
	(not (craftgold))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id goldenblock) " => " (find-text-for-id gold) "  " (* 100 ?v0 ?v1 ) "%")
			craftgold
			(create$ (str-cat "(gold " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgold
		)
	)	
)

(defrule craft-diamond ""

	(workbench ?v0)
	(diamondblock ?v1)
	(not (craftdiamond))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id diamondblock) " => " (find-text-for-id diamond) "  " (* 100 ?v0 ?v1 ) "%")
			craftdiamond
			(create$ (str-cat "(diamond " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftdiamond
		)
	)	
)

(defrule craft-pumpkinpart ""

	(workbench ?v0)
	(pumpkin ?v1)
	(not (craftpumpkinpart))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id pumpkin) " => " (find-text-for-id pumpkinpart) "  " (* 100 ?v0 ?v1 ) "%")
			craftpumpkinpart
			(create$ (str-cat "(pumpkinpart " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftpumpkinpart
		)
	)	
)

(defrule craft-pumpkinseed ""

	(workbench ?v0)
	(pumpkinpart ?v1)
	(not (craftpumpkinseed))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id pumpkinpart) " => " (find-text-for-id pumpkinseed) "  " (* 100 ?v0 ?v1 ) "%")
			craftpumpkinseed
			(create$ (str-cat "(pumpkinseed " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftpumpkinseed
		)
	)	
)

(defrule craft-snowlayer ""

	(workbench ?v0)
	(snowblock ?v1)
	(not (craftsnowlayer))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id snowblock) " => " (find-text-for-id snowlayer) "  " (* 100 ?v0 ?v1 ) "%")
			craftsnowlayer
			(create$ (str-cat "(snowlayer " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftsnowlayer
		)
	)	
)

(defrule craft-cloth ""

	(workbench ?v0)
	(thread ?v1)
	(not (craftcloth))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id thread) " => " (find-text-for-id cloth) "  " (* 100 ?v0 ?v1 ) "%")
			craftcloth
			(create$ (str-cat "(cloth " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftcloth
		)
	)	
)

(defrule craft-bed ""

	(workbench ?v0)
	(cloth ?v1)
	(board ?v2)
	(not (craftbed))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id cloth) ", " (find-text-for-id board) " => " (find-text-for-id bed) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftbed
			(create$ (str-cat "(bed " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftbed
		)
	)	
)

(defrule craft-picture ""

	(workbench ?v0)
	(cloth ?v1)
	(stick ?v2)
	(not (craftpicture))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id cloth) ", " (find-text-for-id stick) " => " (find-text-for-id picture) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftpicture
			(create$ (str-cat "(picture " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftpicture
		)
	)	
)

(defrule craft-lighter_ ""

	(workbench ?v0)
	(flint ?v1)
	(iron ?v2)
	(not (craftlighter_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id flint) ", " (find-text-for-id iron) " => " (find-text-for-id lighter) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftlighter_
			(create$ (str-cat "(lighter " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftlighter_
		)
	)	
)

(defrule craft-watch ""

	(workbench ?v0)
	(gold ?v1)
	(redstone ?v2)
	(not (craftwatch))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id gold) ", " (find-text-for-id redstone) " => " (find-text-for-id watch) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftwatch
			(create$ (str-cat "(watch " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftwatch
		)
	)	
)

(defrule craft-compass ""

	(workbench ?v0)
	(iron ?v1)
	(redstone ?v2)
	(not (craftcompass))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) ", " (find-text-for-id redstone) " => " (find-text-for-id compass) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftcompass
			(create$ (str-cat "(compass " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftcompass
		)
	)	
)

(defrule craft-map ""

	(workbench ?v0)
	(paper ?v1)
	(compass ?v2)
	(not (craftmap))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id paper) ", " (find-text-for-id compass) " => " (find-text-for-id map) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftmap
			(create$ (str-cat "(map " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftmap
		)
	)	
)

(defrule craft-fishingrod ""

	(workbench ?v0)
	(stick ?v1)
	(thread ?v2)
	(not (craftfishingrod))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id stick) ", " (find-text-for-id thread) " => " (find-text-for-id fishingrod) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftfishingrod
			(create$ (str-cat "(fishingrod " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftfishingrod
		)
	)	
)

(defrule craft-woodenpickaxe ""

	(workbench ?v0)
	(wood ?v1)
	(stick ?v2)
	(not (craftwoodenpickaxe))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id wood) ", " (find-text-for-id stick) " => " (find-text-for-id woodenpickaxe) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftwoodenpickaxe
			(create$ (str-cat "(woodenpickaxe " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftwoodenpickaxe
		)
	)	
)

(defrule craft-stonepickaxe ""

	(workbench ?v0)
	(stone ?v1)
	(stick ?v2)
	(not (craftstonepickaxe))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id stone) ", " (find-text-for-id stick) " => " (find-text-for-id stonepickaxe) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftstonepickaxe
			(create$ (str-cat "(stonepickaxe " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftstonepickaxe
		)
	)	
)

(defrule craft-ironpickaxe ""

	(workbench ?v0)
	(iron ?v1)
	(stick ?v2)
	(not (craftironpickaxe))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id iron) ", " (find-text-for-id stick) " => " (find-text-for-id ironpickaxe) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftironpickaxe
			(create$ (str-cat "(ironpickaxe " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftironpickaxe
		)
	)	
)

(defrule craft-goldenpickaxe ""

	(workbench ?v0)
	(gold ?v1)
	(stick ?v2)
	(not (craftgoldenpickaxe))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id gold) ", " (find-text-for-id stick) " => " (find-text-for-id goldenpickaxe) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftgoldenpickaxe
			(create$ (str-cat "(goldenpickaxe " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgoldenpickaxe
		)
	)	
)

(defrule craft-diamondpickaxe ""

	(workbench ?v0)
	(diamond ?v1)
	(stick ?v2)
	(not (craftdiamondpickaxe))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id diamond) ", " (find-text-for-id stick) " => " (find-text-for-id diamondpickaxe) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftdiamondpickaxe
			(create$ (str-cat "(diamondpickaxe " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftdiamondpickaxe
		)
	)	
)

(defrule craft-netherite ""

	(workbench ?v0)
	(netheritescrap ?v1)
	(gold ?v2)
	(not (craftnetherite))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id netheritescrap) ", " (find-text-for-id gold) " => " (find-text-for-id netherite) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftnetherite
			(create$ (str-cat "(netherite " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftnetherite
		)
	)	
)

(defrule craft-arrow ""

	(workbench ?v0)
	(flint ?v1)
	(stick ?v2)
	(feather ?v3)
	(not (craftarrow))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id flint) ", " (find-text-for-id stick) ", " (find-text-for-id feather) " => " (find-text-for-id arrow) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftarrow
			(create$ (str-cat "(arrow " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftarrow
		)
	)	
)

(defrule craft-ghostarrow ""

	(workbench ?v0)
	(glowstonedust ?v1)
	(arrow ?v2)
	(not (craftghostarrow))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id glowstonedust) ", " (find-text-for-id arrow) " => " (find-text-for-id ghostarrow) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftghostarrow
			(create$ (str-cat "(ghostarrow " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftghostarrow
		)
	)	
)

(defrule craft-tnt ""

	(workbench ?v0)
	(gunpowder ?v1)
	(sand ?v2)
	(not (crafttnt))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id gunpowder) ", " (find-text-for-id sand) " => " (find-text-for-id tnt) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			crafttnt
			(create$ (str-cat "(tnt " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			crafttnt
		)
	)	
)

(defrule craft-jackslamp_ ""

	(workbench ?v0)
	(pumpkin ?v1)
	(torch ?v2)
	(not (craftjackslamp_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id pumpkin) ", " (find-text-for-id torch) " => " (find-text-for-id jackslamp) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftjackslamp_
			(create$ (str-cat "(jackslamp " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftjackslamp_
		)
	)	
)

(defrule craft-slimeblock ""

	(workbench ?v0)
	(slime ?v1)
	(not (craftslimeblock))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id slime) " => " (find-text-for-id slimeblock) "  " (* 100 ?v0 ?v1 ) "%")
			craftslimeblock
			(create$ (str-cat "(slimeblock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftslimeblock
		)
	)	
)

(defrule craft-mossystone_ ""

	(workbench ?v0)
	(stone ?v1)
	(vines ?v2)
	(not (craftmossystone_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id stone) ", " (find-text-for-id vines) " => " (find-text-for-id mossystone) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftmossystone_
			(create$ (str-cat "(mossystone " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftmossystone_
		)
	)	
)

(defrule craft-redstonetorch_ ""

	(workbench ?v0)
	(stick ?v1)
	(redstone ?v2)
	(not (craftredstonetorch_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id stick) ", " (find-text-for-id redstone) " => " (find-text-for-id redstonetorch) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftredstonetorch_
			(create$ (str-cat "(redstonetorch " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftredstonetorch_
		)
	)	
)

(defrule craft-repeater ""

	(workbench ?v0)
	(redstone ?v1)
	(redstonetorch ?v2)
	(stone ?v3)
	(not (craftrepeater))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id redstone) ", " (find-text-for-id redstonetorch) ", " (find-text-for-id stone) " => " (find-text-for-id repeater) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftrepeater
			(create$ (str-cat "(repeater " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftrepeater
		)
	)	
)

(defrule craft-cookies ""

	(workbench ?v0)
	(beans ?v1)
	(wheat ?v2)
	(not (craftcookies))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id beans) ", " (find-text-for-id wheat) " => " (find-text-for-id cookies) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftcookies
			(create$ (str-cat "(cookies " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftcookies
		)
	)	
)

(defrule craft-bread ""

	(workbench ?v0)
	(wheat ?v1)
	(not (craftbread))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id wheat) " => " (find-text-for-id bread) "  " (* 100 ?v0 ?v1 ) "%")
			craftbread
			(create$ (str-cat "(bread " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftbread
		)
	)	
)

(defrule craft-sugar_ ""

	(workbench ?v0)
	(cane ?v1)
	(not (craftsugar_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id cane) " => " (find-text-for-id sugar) "  " (* 100 ?v0 ?v1 ) "%")
			craftsugar_
			(create$ (str-cat "(sugar " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftsugar_
		)
	)	
)

(defrule craft-cake ""

	(workbench ?v0)
	(milk ?v1)
	(wheat ?v2)
	(egg ?v3)
	(sugar ?v4)
	(not (craftcake))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ?v4 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id milk) ", " (find-text-for-id wheat) ", " (find-text-for-id egg) ", " (find-text-for-id sugar) " => " (find-text-for-id cake) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ?v4 ) "%")
			craftcake
			(create$ (str-cat "(cake " (* 1 ?v0 ?v1 ?v2 ?v3 ?v4 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftcake
		)
	)	
)

(defrule craft-goldenapple ""

	(workbench ?v0)
	(goldenblock ?v1)
	(apple ?v2)
	(not (craftgoldenapple))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id goldenblock) ", " (find-text-for-id apple) " => " (find-text-for-id goldenapple) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftgoldenapple
			(create$ (str-cat "(goldenapple " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgoldenapple
		)
	)	
)

(defrule craft-pumpkinpie ""

	(workbench ?v0)
	(pumpkin ?v1)
	(egg ?v2)
	(sugar ?v3)
	(not (craftpumpkinpie))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id pumpkin) ", " (find-text-for-id egg) ", " (find-text-for-id sugar) " => " (find-text-for-id pumpkinpie) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftpumpkinpie
			(create$ (str-cat "(pumpkinpie " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftpumpkinpie
		)
	)	
)

(defrule craft-sheafofhay ""

	(workbench ?v0)
	(wheat ?v1)
	(not (craftsheafofhay))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id wheat) " => " (find-text-for-id sheafofhay) "  " (* 100 ?v0 ?v1 ) "%")
			craftsheafofhay
			(create$ (str-cat "(sheafofhay " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftsheafofhay
		)
	)	
)

(defrule craft-wheat_ ""

	(workbench ?v0)
	(sheafofhay ?v1)
	(not (craftwheat_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id sheafofhay) " => " (find-text-for-id wheat) "  " (* 100 ?v0 ?v1 ) "%")
			craftwheat_
			(create$ (str-cat "(wheat " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftwheat_
		)
	)	
)

(defrule craft-paper ""

	(workbench ?v0)
	(bamboo ?v1)
	(not (craftpaper))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id bamboo) " => " (find-text-for-id paper) "  " (* 100 ?v0 ?v1 ) "%")
			craftpaper
			(create$ (str-cat "(paper " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftpaper
		)
	)	
)

(defrule craft-stage ""

	(workbench ?v0)
	(bamboo ?v1)
	(thread ?v2)
	(not (craftstage))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id bamboo) ", " (find-text-for-id thread) " => " (find-text-for-id stage) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftstage
			(create$ (str-cat "(stage " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftstage
		)
	)	
)

(defrule craft-book ""

	(workbench ?v0)
	(paper ?v1)
	(not (craftbook))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id paper) " => " (find-text-for-id book) "  " (* 100 ?v0 ?v1 ) "%")
			craftbook
			(create$ (str-cat "(book " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftbook
		)
	)	
)

(defrule craft-firepowder_ ""

	(workbench ?v0)
	(firerod ?v1)
	(not (craftfirepowder_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id firerod) " => " (find-text-for-id firepowder) "  " (* 100 ?v0 ?v1 ) "%")
			craftfirepowder_
			(create$ (str-cat "(firepowder " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftfirepowder_
		)
	)	
)

(defrule craft-endermaneye_ ""

	(workbench ?v0)
	(endermanpearl ?v1)
	(firepowder ?v2)
	(not (craftendermaneye_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id endermanpearl) ", " (find-text-for-id firepowder) " => " (find-text-for-id endermaneye) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftendermaneye_
			(create$ (str-cat "(endermaneye " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftendermaneye_
		)
	)	
)

(defrule craft-lavacream_ ""

	(workbench ?v0)
	(slime ?v1)
	(firepowder ?v2)
	(not (craftlavacream_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id slime) ", " (find-text-for-id firepowder) " => " (find-text-for-id lavacream) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftlavacream_
			(create$ (str-cat "(lavacream " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftlavacream_
		)
	)	
)

(defrule craft-fireball ""

	(workbench ?v0)
	(firepowder ?v1)
	(gunpowder ?v2)
	(coal ?v3)
	(not (craftfireball))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id firepowder) ", " (find-text-for-id gunpowder) ", " (find-text-for-id coal) " => " (find-text-for-id fireball) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftfireball
			(create$ (str-cat "(fireball " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftfireball
		)
	)	
)

(defrule craft-editablebook ""

	(workbench ?v0)
	(feather ?v1)
	(dye ?v2)
	(book ?v3)
	(not (crafteditablebook))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id feather) ", " (find-text-for-id dye) ", " (find-text-for-id book) " => " (find-text-for-id editablebook) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			crafteditablebook
			(create$ (str-cat "(editablebook " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			crafteditablebook
		)
	)	
)

(defrule craft-star_ ""

	(workbench ?v0)
	(gunpowder ?v1)
	(dye ?v2)
	(not (craftstar_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id gunpowder) ", " (find-text-for-id dye) " => " (find-text-for-id star) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftstar_
			(create$ (str-cat "(star " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftstar_
		)
	)	
)

(defrule craft-rocket ""

	(workbench ?v0)
	(star ?v1)
	(paper ?v2)
	(gunpowder ?v3)
	(not (craftrocket))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id star) ", " (find-text-for-id paper) ", " (find-text-for-id gunpowder) " => " (find-text-for-id rocket) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftrocket
			(create$ (str-cat "(rocket " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftrocket
		)
	)	
)

(defrule craft-greydye ""

	(workbench ?v0)
	(ink ?v1)
	(bonemeal ?v2)
	(not (craftgreydye))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id ink) ", " (find-text-for-id bonemeal) " => " (find-text-for-id greydye) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftgreydye
			(create$ (str-cat "(greydye " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgreydye
		)
	)	
)

(defrule craft-magmablock_ ""

	(workbench ?v0)
	(lavacream ?v1)
	(not (craftmagmablock_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id lavacream) " => " (find-text-for-id magmablock) "  " (* 100 ?v0 ?v1 ) "%")
			craftmagmablock_
			(create$ (str-cat "(magmablock " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftmagmablock_
		)
	)	
)

(defrule craft-glowstone ""

	(workbench ?v0)
	(glowstonedust ?v1)
	(not (craftglowstone))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id glowstonedust) " => " (find-text-for-id glowstone) "  " (* 100 ?v0 ?v1 ) "%")
			craftglowstone
			(create$ (str-cat "(glowstone " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftglowstone
		)
	)	
)

(defrule craft-glowstonedust_ ""

	(workbench ?v0)
	(glowstone ?v1)
	(not (craftglowstonedust_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id glowstone) " => " (find-text-for-id glowstonedust) "  " (* 100 ?v0 ?v1 ) "%")
			craftglowstonedust_
			(create$ (str-cat "(glowstonedust " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftglowstonedust_
		)
	)	
)

(defrule craft-library ""

	(workbench ?v0)
	(board ?v1)
	(book ?v2)
	(not (craftlibrary))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id board) ", " (find-text-for-id book) " => " (find-text-for-id library) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftlibrary
			(create$ (str-cat "(library " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftlibrary
		)
	)	
)

(defrule craft-cathedral ""

	(workbench ?v0)
	(board ?v1)
	(library ?v2)
	(not (craftcathedral))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id board) ", " (find-text-for-id library) " => " (find-text-for-id cathedral) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftcathedral
			(create$ (str-cat "(cathedral " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftcathedral
		)
	)	
)

(defrule craft-musicbox ""

	(workbench ?v0)
	(board ?v1)
	(diamond ?v2)
	(not (craftmusicbox))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id workbench) ", " (find-text-for-id board) ", " (find-text-for-id diamond) " => " (find-text-for-id musicbox) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftmusicbox
			(create$ (str-cat "(musicbox " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftmusicbox
		)
	)	
)

(defrule craft-glass ""

	(furnace ?v0)
	(sand ?v1)
	(fire ?v2)
	(coal ?v3)
	(not (craftglass))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id sand) ", " (find-text-for-id fire) ", " (find-text-for-id coal) " => " (find-text-for-id glass) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftglass
			(create$ (str-cat "(glass " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftglass
		)
	)	
)

(defrule craft-glass_ ""

	(furnace ?v0)
	(sand ?v1)
	(fire ?v2)
	(wood ?v3)
	(not (craftglass_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id sand) ", " (find-text-for-id fire) ", " (find-text-for-id wood) " => " (find-text-for-id glass) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftglass_
			(create$ (str-cat "(glass " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftglass_
		)
	)	
)

(defrule craft-glass__ ""

	(furnace ?v0)
	(sand ?v1)
	(fire ?v2)
	(board ?v3)
	(not (craftglass__))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id sand) ", " (find-text-for-id fire) ", " (find-text-for-id board) " => " (find-text-for-id glass) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftglass__
			(create$ (str-cat "(glass " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftglass__
		)
	)	
)

(defrule craft-iron_ ""

	(furnace ?v0)
	(ironore ?v1)
	(fire ?v2)
	(coal ?v3)
	(not (craftiron_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id ironore) ", " (find-text-for-id fire) ", " (find-text-for-id coal) " => " (find-text-for-id iron) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftiron_
			(create$ (str-cat "(iron " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftiron_
		)
	)	
)

(defrule craft-iron__ ""

	(furnace ?v0)
	(ironore ?v1)
	(fire ?v2)
	(wood ?v3)
	(not (craftiron__))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id ironore) ", " (find-text-for-id fire) ", " (find-text-for-id wood) " => " (find-text-for-id iron) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftiron__
			(create$ (str-cat "(iron " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftiron__
		)
	)	
)

(defrule craft-iron___ ""

	(furnace ?v0)
	(ironore ?v1)
	(fire ?v2)
	(board ?v3)
	(not (craftiron___))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id ironore) ", " (find-text-for-id fire) ", " (find-text-for-id board) " => " (find-text-for-id iron) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftiron___
			(create$ (str-cat "(iron " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftiron___
		)
	)	
)

(defrule craft-gold_ ""

	(furnace ?v0)
	(goldenore ?v1)
	(fire ?v2)
	(coal ?v3)
	(not (craftgold_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id goldenore) ", " (find-text-for-id fire) ", " (find-text-for-id coal) " => " (find-text-for-id gold) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftgold_
			(create$ (str-cat "(gold " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgold_
		)
	)	
)

(defrule craft-gold__ ""

	(furnace ?v0)
	(goldenore ?v1)
	(fire ?v2)
	(wood ?v3)
	(not (craftgold__))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id goldenore) ", " (find-text-for-id fire) ", " (find-text-for-id wood) " => " (find-text-for-id gold) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftgold__
			(create$ (str-cat "(gold " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgold__
		)
	)	
)

(defrule craft-gold___ ""

	(furnace ?v0)
	(goldenore ?v1)
	(fire ?v2)
	(board ?v3)
	(not (craftgold___))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id goldenore) ", " (find-text-for-id fire) ", " (find-text-for-id board) " => " (find-text-for-id gold) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftgold___
			(create$ (str-cat "(gold " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftgold___
		)
	)	
)

(defrule craft-netheritescrap ""

	(furnace ?v0)
	(ancientwreckage ?v1)
	(fire ?v2)
	(coal ?v3)
	(not (craftnetheritescrap))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id ancientwreckage) ", " (find-text-for-id fire) ", " (find-text-for-id coal) " => " (find-text-for-id netheritescrap) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftnetheritescrap
			(create$ (str-cat "(netheritescrap " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftnetheritescrap
		)
	)	
)

(defrule craft-netheritescrap_ ""

	(furnace ?v0)
	(ancientwreckage ?v1)
	(fire ?v2)
	(wood ?v3)
	(not (craftnetheritescrap_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id ancientwreckage) ", " (find-text-for-id fire) ", " (find-text-for-id wood) " => " (find-text-for-id netheritescrap) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftnetheritescrap_
			(create$ (str-cat "(netheritescrap " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftnetheritescrap_
		)
	)	
)

(defrule craft-netheritescrap__ ""

	(furnace ?v0)
	(ancientwreckage ?v1)
	(fire ?v2)
	(board ?v3)
	(not (craftnetheritescrap__))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id ancientwreckage) ", " (find-text-for-id fire) ", " (find-text-for-id board) " => " (find-text-for-id netheritescrap) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftnetheritescrap__
			(create$ (str-cat "(netheritescrap " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftnetheritescrap__
		)
	)	
)

(defrule craft-bakedclay ""

	(furnace ?v0)
	(clay ?v1)
	(fire ?v2)
	(coal ?v3)
	(not (craftbakedclay))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id clay) ", " (find-text-for-id fire) ", " (find-text-for-id coal) " => " (find-text-for-id bakedclay) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftbakedclay
			(create$ (str-cat "(bakedclay " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftbakedclay
		)
	)	
)

(defrule craft-bakedclay_ ""

	(furnace ?v0)
	(clay ?v1)
	(fire ?v2)
	(wood ?v3)
	(not (craftbakedclay_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id clay) ", " (find-text-for-id fire) ", " (find-text-for-id wood) " => " (find-text-for-id bakedclay) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftbakedclay_
			(create$ (str-cat "(bakedclay " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftbakedclay_
		)
	)	
)

(defrule craft-bakedclay__ ""

	(furnace ?v0)
	(clay ?v1)
	(fire ?v2)
	(board ?v3)
	(not (craftbakedclay__))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id clay) ", " (find-text-for-id fire) ", " (find-text-for-id board) " => " (find-text-for-id bakedclay) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftbakedclay__
			(create$ (str-cat "(bakedclay " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftbakedclay__
		)
	)	
)

(defrule craft-driedkelp ""

	(furnace ?v0)
	(laminaria ?v1)
	(fire ?v2)
	(coal ?v3)
	(not (craftdriedkelp))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id laminaria) ", " (find-text-for-id fire) ", " (find-text-for-id coal) " => " (find-text-for-id driedkelp) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftdriedkelp
			(create$ (str-cat "(driedkelp " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftdriedkelp
		)
	)	
)

(defrule craft-driedkelp_ ""

	(furnace ?v0)
	(laminaria ?v1)
	(fire ?v2)
	(wood ?v3)
	(not (craftdriedkelp_))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id laminaria) ", " (find-text-for-id fire) ", " (find-text-for-id wood) " => " (find-text-for-id driedkelp) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftdriedkelp_
			(create$ (str-cat "(driedkelp " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftdriedkelp_
		)
	)	
)

(defrule craft-driedkelp__ ""

	(furnace ?v0)
	(laminaria ?v1)
	(fire ?v2)
	(board ?v3)
	(not (craftdriedkelp__))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ?v3 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id furnace) ", " (find-text-for-id laminaria) ", " (find-text-for-id fire) ", " (find-text-for-id board) " => " (find-text-for-id driedkelp) "  " (* 100 ?v0 ?v1 ?v2 ?v3 ) "%")
			craftdriedkelp__
			(create$ (str-cat "(driedkelp " (* 1 ?v0 ?v1 ?v2 ?v3 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftdriedkelp__
		)
	)	
)

(defrule craft-netheritepickaxe ""

	(blacksmithstable ?v0)
	(netherite ?v1)
	(pickaxe ?v2)
	(not (craftnetheritepickaxe))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ?v2 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id blacksmithstable) ", " (find-text-for-id netherite) ", " (find-text-for-id pickaxe) " => " (find-text-for-id netheritepickaxe) "  " (* 100 ?v0 ?v1 ?v2 ) "%")
			craftnetheritepickaxe
			(create$ (str-cat "(netheritepickaxe " (* 1 ?v0 ?v1 ?v2 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftnetheritepickaxe
		)
	)	
)

(defrule craft-fish ""

	(water ?v0)
	(fishingrod ?v1)
	(not (craftfish))

	=>

	(if (< (/ (mod (random) 100) 100) (* 1 ?v0 ?v1 ))
		then 
		(
			handle-state interview
			?*target*
			(str-cat (find-text-for-id water) ", " (find-text-for-id fishingrod) " => " (find-text-for-id fish) "  " (* 100 ?v0 ?v1 ) "%")
			craftfish
			(create$ (str-cat "(fish " (* 1 ?v0 ?v1 ) ")" ))
		)
		else
		(
			handle-state interview
			?*target*
			"Crafting failed"
			craftfish
		)
	)	
)

