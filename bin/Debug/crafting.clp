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

	(backpack)
	(wood)
	(not (craftboard))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id board)
		craftboard
		(create$ "(board)" )))

(defrule craft-workbench ""

	(backpack)
	(board)
	(not (craftworkbench))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id workbench)
		craftworkbench
		(create$ "(workbench)" )))

(defrule craft-torch ""

	(backpack)
	(stick)
	(coal)
	(not (crafttorch))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id torch)
		crafttorch
		(create$ "(torch)" )))

(defrule craft-redstonetorch ""

	(backpack)
	(stick)
	(redstone)
	(not (craftredstonetorch))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id redstonetorch)
		craftredstonetorch
		(create$ "(redstonetorch)" )))

(defrule craft-lighter ""

	(backpack)
	(flint)
	(iron)
	(not (craftlighter))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id lighter)
		craftlighter
		(create$ "(lighter)" )))

(defrule craft-snowblock ""

	(backpack)
	(snowball)
	(not (craftsnowblock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id snowblock)
		craftsnowblock
		(create$ "(snowblock)" )))

(defrule craft-clayblock ""

	(backpack)
	(clay)
	(not (craftclayblock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id clayblock)
		craftclayblock
		(create$ "(clayblock)" )))

(defrule craft-jackslamp ""

	(backpack)
	(pumpkin)
	(torch)
	(not (craftjackslamp))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id jackslamp)
		craftjackslamp
		(create$ "(jackslamp)" )))

(defrule craft-stonebricks ""

	(backpack)
	(stone)
	(not (craftstonebricks))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id stonebricks)
		craftstonebricks
		(create$ "(stonebricks)" )))

(defrule craft-mossystone ""

	(backpack)
	(stone)
	(vines)
	(not (craftmossystone))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id mossystone)
		craftmossystone
		(create$ "(mossystone)" )))

(defrule craft-rodwithcarrot ""

	(backpack)
	(carrot)
	(fishingrod)
	(not (craftrodwithcarrot))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id rodwithcarrot)
		craftrodwithcarrot
		(create$ "(rodwithcarrot)" )))

(defrule craft-sugar ""

	(backpack)
	(cane)
	(not (craftsugar))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id sugar)
		craftsugar
		(create$ "(sugar)" )))

(defrule craft-wheat ""

	(backpack)
	(sheafofhay)
	(not (craftwheat))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id wheat)
		craftwheat
		(create$ "(wheat)" )))

(defrule craft-firepowder ""

	(backpack)
	(firerod)
	(not (craftfirepowder))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id firepowder)
		craftfirepowder
		(create$ "(firepowder)" )))

(defrule craft-endermaneye ""

	(backpack)
	(endermanpearl)
	(firepowder)
	(not (craftendermaneye))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id endermaneye)
		craftendermaneye
		(create$ "(endermaneye)" )))

(defrule craft-lavacream ""

	(backpack)
	(slime)
	(firepowder)
	(not (craftlavacream))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id lavacream)
		craftlavacream
		(create$ "(lavacream)" )))

(defrule craft-star ""

	(backpack)
	(gunpowder)
	(dye)
	(not (craftstar))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id star)
		craftstar
		(create$ "(star)" )))

(defrule craft-stick ""

	(backpack)
	(bamboo)
	(not (craftstick))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id stick)
		craftstick
		(create$ "(stick)" )))

(defrule craft-magmablock ""

	(backpack)
	(lavacream)
	(not (craftmagmablock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id magmablock)
		craftmagmablock
		(create$ "(magmablock)" )))

(defrule craft-glowstonedust ""

	(backpack)
	(glowstone)
	(not (craftglowstonedust))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id glowstonedust)
		craftglowstonedust
		(create$ "(glowstonedust)" )))

(defrule craft-furnace ""

	(workbench)
	(stone)
	(not (craftfurnace))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id furnace)
		craftfurnace
		(create$ "(furnace)" )))

(defrule craft-chest ""

	(workbench)
	(wood)
	(not (craftchest))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id chest)
		craftchest
		(create$ "(chest)" )))

(defrule craft-boiler ""

	(workbench)
	(iron)
	(not (craftboiler))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id boiler)
		craftboiler
		(create$ "(boiler)" )))

(defrule craft-meltingfurnace ""

	(workbench)
	(iron)
	(stone)
	(furnace)
	(not (craftmeltingfurnace))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id meltingfurnace)
		craftmeltingfurnace
		(create$ "(meltingfurnace)" )))

(defrule craft-smokehouse ""

	(workbench)
	(furnace)
	(wood)
	(not (craftsmokehouse))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id smokehouse)
		craftsmokehouse
		(create$ "(smokehouse)" )))

(defrule craft-stonecutter ""

	(workbench)
	(iron)
	(stone)
	(not (craftstonecutter))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id stonecutter)
		craftstonecutter
		(create$ "(stonecutter)" )))

(defrule craft-anvil ""

	(workbench)
	(iron)
	(ironblock)
	(not (craftanvil))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id anvil)
		craftanvil
		(create$ "(anvil)" )))

(defrule craft-hive ""

	(workbench)
	(wood)
	(honeycomb)
	(not (crafthive))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id hive)
		crafthive
		(create$ "(hive)" )))

(defrule craft-blacksmithstable ""

	(workbench)
	(iron)
	(wood)
	(not (craftblacksmithstable))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id blacksmithstable)
		craftblacksmithstable
		(create$ "(blacksmithstable)" )))

(defrule craft-cartographerstable ""

	(workbench)
	(paper)
	(board)
	(not (craftcartographerstable))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id cartographerstable)
		craftcartographerstable
		(create$ "(cartographerstable)" )))

(defrule craft-archerstable ""

	(workbench)
	(flint)
	(board)
	(not (craftarcherstable))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id archerstable)
		craftarcherstable
		(create$ "(archerstable)" )))

(defrule craft-hopper ""

	(workbench)
	(iron)
	(chest)
	(not (crafthopper))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id hopper)
		crafthopper
		(create$ "(hopper)" )))

(defrule craft-torch_ ""

	(workbench)
	(stick)
	(coal)
	(not (crafttorch_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id torch)
		crafttorch_
		(create$ "(torch)" )))

(defrule craft-leash ""

	(workbench)
	(thread)
	(slime)
	(not (craftleash))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id leash)
		craftleash
		(create$ "(leash)" )))

(defrule craft-helmet ""

	(workbench)
	(turtleshell)
	(not (crafthelmet))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id helmet)
		crafthelmet
		(create$ "(helmet)" )))

(defrule craft-campfire ""

	(workbench)
	(stick)
	(coal)
	(wood)
	(not (craftcampfire))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id campfire)
		craftcampfire
		(create$ "(campfire)" )))

(defrule craft-ironblock ""

	(workbench)
	(iron)
	(not (craftironblock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id ironblock)
		craftironblock
		(create$ "(ironblock)" )))

(defrule craft-goldenblock ""

	(workbench)
	(gold)
	(not (craftgoldenblock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id goldenblock)
		craftgoldenblock
		(create$ "(goldenblock)" )))

(defrule craft-diamondblock ""

	(workbench)
	(diamond)
	(not (craftdiamondblock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id diamondblock)
		craftdiamondblock
		(create$ "(diamondblock)" )))

(defrule craft-snowblock_ ""

	(workbench)
	(snowball)
	(not (craftsnowblock_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id snowblock)
		craftsnowblock_
		(create$ "(snowblock)" )))

(defrule craft-clayblock_ ""

	(workbench)
	(clay)
	(not (craftclayblock_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id clayblock)
		craftclayblock_
		(create$ "(clayblock)" )))

(defrule craft-boneblock ""

	(workbench)
	(bonemeal)
	(not (craftboneblock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id boneblock)
		craftboneblock
		(create$ "(boneblock)" )))

(defrule craft-iron ""

	(workbench)
	(ironblock)
	(not (craftiron))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id iron)
		craftiron
		(create$ "(iron)" )))

(defrule craft-gold ""

	(workbench)
	(goldenblock)
	(not (craftgold))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id gold)
		craftgold
		(create$ "(gold)" )))

(defrule craft-diamond ""

	(workbench)
	(diamondblock)
	(not (craftdiamond))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id diamond)
		craftdiamond
		(create$ "(diamond)" )))

(defrule craft-pumpkinpart ""

	(workbench)
	(pumpkin)
	(not (craftpumpkinpart))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id pumpkinpart)
		craftpumpkinpart
		(create$ "(pumpkinpart)" )))

(defrule craft-pumpkinseed ""

	(workbench)
	(pumpkinpart)
	(not (craftpumpkinseed))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id pumpkinseed)
		craftpumpkinseed
		(create$ "(pumpkinseed)" )))

(defrule craft-snowlayer ""

	(workbench)
	(snowblock)
	(not (craftsnowlayer))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id snowlayer)
		craftsnowlayer
		(create$ "(snowlayer)" )))

(defrule craft-cloth ""

	(workbench)
	(thread)
	(not (craftcloth))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id cloth)
		craftcloth
		(create$ "(cloth)" )))

(defrule craft-bed ""

	(workbench)
	(cloth)
	(board)
	(not (craftbed))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id bed)
		craftbed
		(create$ "(bed)" )))

(defrule craft-picture ""

	(workbench)
	(cloth)
	(stick)
	(not (craftpicture))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id picture)
		craftpicture
		(create$ "(picture)" )))

(defrule craft-lighter_ ""

	(workbench)
	(flint)
	(iron)
	(not (craftlighter_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id lighter)
		craftlighter_
		(create$ "(lighter)" )))

(defrule craft-watch ""

	(workbench)
	(gold)
	(redstone)
	(not (craftwatch))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id watch)
		craftwatch
		(create$ "(watch)" )))

(defrule craft-compass ""

	(workbench)
	(iron)
	(redstone)
	(not (craftcompass))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id compass)
		craftcompass
		(create$ "(compass)" )))

(defrule craft-map ""

	(workbench)
	(paper)
	(compass)
	(not (craftmap))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id map)
		craftmap
		(create$ "(map)" )))

(defrule craft-fishingrod ""

	(workbench)
	(stick)
	(thread)
	(not (craftfishingrod))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id fishingrod)
		craftfishingrod
		(create$ "(fishingrod)" )))

(defrule craft-woodenpickaxe ""

	(workbench)
	(wood)
	(stick)
	(not (craftwoodenpickaxe))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id woodenpickaxe)
		craftwoodenpickaxe
		(create$ "(woodenpickaxe)" )))

(defrule craft-stonepickaxe ""

	(workbench)
	(stone)
	(stick)
	(not (craftstonepickaxe))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id stonepickaxe)
		craftstonepickaxe
		(create$ "(stonepickaxe)" )))

(defrule craft-ironpickaxe ""

	(workbench)
	(iron)
	(stick)
	(not (craftironpickaxe))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id ironpickaxe)
		craftironpickaxe
		(create$ "(ironpickaxe)" )))

(defrule craft-goldenpickaxe ""

	(workbench)
	(gold)
	(stick)
	(not (craftgoldenpickaxe))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id goldenpickaxe)
		craftgoldenpickaxe
		(create$ "(goldenpickaxe)" )))

(defrule craft-diamondpickaxe ""

	(workbench)
	(diamond)
	(stick)
	(not (craftdiamondpickaxe))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id diamondpickaxe)
		craftdiamondpickaxe
		(create$ "(diamondpickaxe)" )))

(defrule craft-netherite ""

	(workbench)
	(netheritescrap)
	(gold)
	(not (craftnetherite))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id netherite)
		craftnetherite
		(create$ "(netherite)" )))

(defrule craft-arrow ""

	(workbench)
	(flint)
	(stick)
	(feather)
	(not (craftarrow))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id arrow)
		craftarrow
		(create$ "(arrow)" )))

(defrule craft-ghostarrow ""

	(workbench)
	(glowstonedust)
	(arrow)
	(not (craftghostarrow))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id ghostarrow)
		craftghostarrow
		(create$ "(ghostarrow)" )))

(defrule craft-tnt ""

	(workbench)
	(gunpowder)
	(sand)
	(not (crafttnt))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id tnt)
		crafttnt
		(create$ "(tnt)" )))

(defrule craft-jackslamp_ ""

	(workbench)
	(pumpkin)
	(torch)
	(not (craftjackslamp_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id jackslamp)
		craftjackslamp_
		(create$ "(jackslamp)" )))

(defrule craft-slimeblock ""

	(workbench)
	(slime)
	(not (craftslimeblock))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id slimeblock)
		craftslimeblock
		(create$ "(slimeblock)" )))

(defrule craft-mossystone_ ""

	(workbench)
	(stone)
	(vines)
	(not (craftmossystone_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id mossystone)
		craftmossystone_
		(create$ "(mossystone)" )))

(defrule craft-redstonetorch_ ""

	(workbench)
	(stick)
	(redstone)
	(not (craftredstonetorch_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id redstonetorch)
		craftredstonetorch_
		(create$ "(redstonetorch)" )))

(defrule craft-repeater ""

	(workbench)
	(redstone)
	(redstonetorch)
	(stone)
	(not (craftrepeater))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id repeater)
		craftrepeater
		(create$ "(repeater)" )))

(defrule craft-cookies ""

	(workbench)
	(beans)
	(wheat)
	(not (craftcookies))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id cookies)
		craftcookies
		(create$ "(cookies)" )))

(defrule craft-bread ""

	(workbench)
	(wheat)
	(not (craftbread))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id bread)
		craftbread
		(create$ "(bread)" )))

(defrule craft-sugar_ ""

	(workbench)
	(cane)
	(not (craftsugar_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id sugar)
		craftsugar_
		(create$ "(sugar)" )))

(defrule craft-cake ""

	(workbench)
	(milk)
	(wheat)
	(egg)
	(sugar)
	(not (craftcake))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id cake)
		craftcake
		(create$ "(cake)" )))

(defrule craft-goldenapple ""

	(workbench)
	(goldenblock)
	(apple)
	(not (craftgoldenapple))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id goldenapple)
		craftgoldenapple
		(create$ "(goldenapple)" )))

(defrule craft-pumpkinpie ""

	(workbench)
	(pumpkin)
	(egg)
	(sugar)
	(not (craftpumpkinpie))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id pumpkinpie)
		craftpumpkinpie
		(create$ "(pumpkinpie)" )))

(defrule craft-sheafofhay ""

	(workbench)
	(wheat)
	(not (craftsheafofhay))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id sheafofhay)
		craftsheafofhay
		(create$ "(sheafofhay)" )))

(defrule craft-wheat_ ""

	(workbench)
	(sheafofhay)
	(not (craftwheat_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id wheat)
		craftwheat_
		(create$ "(wheat)" )))

(defrule craft-paper ""

	(workbench)
	(bamboo)
	(not (craftpaper))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id paper)
		craftpaper
		(create$ "(paper)" )))

(defrule craft-stage ""

	(workbench)
	(bamboo)
	(thread)
	(not (craftstage))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id stage)
		craftstage
		(create$ "(stage)" )))

(defrule craft-book ""

	(workbench)
	(paper)
	(not (craftbook))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id book)
		craftbook
		(create$ "(book)" )))

(defrule craft-firepowder_ ""

	(workbench)
	(firerod)
	(not (craftfirepowder_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id firepowder)
		craftfirepowder_
		(create$ "(firepowder)" )))

(defrule craft-endermaneye_ ""

	(workbench)
	(endermanpearl)
	(firepowder)
	(not (craftendermaneye_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id endermaneye)
		craftendermaneye_
		(create$ "(endermaneye)" )))

(defrule craft-lavacream_ ""

	(workbench)
	(slime)
	(firepowder)
	(not (craftlavacream_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id lavacream)
		craftlavacream_
		(create$ "(lavacream)" )))

(defrule craft-fireball ""

	(workbench)
	(firepowder)
	(gunpowder)
	(coal)
	(not (craftfireball))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id fireball)
		craftfireball
		(create$ "(fireball)" )))

(defrule craft-editablebook ""

	(workbench)
	(feather)
	(dye)
	(book)
	(not (crafteditablebook))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id editablebook)
		crafteditablebook
		(create$ "(editablebook)" )))

(defrule craft-star_ ""

	(workbench)
	(gunpowder)
	(dye)
	(not (craftstar_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id star)
		craftstar_
		(create$ "(star)" )))

(defrule craft-rocket ""

	(workbench)
	(star)
	(paper)
	(gunpowder)
	(not (craftrocket))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id rocket)
		craftrocket
		(create$ "(rocket)" )))

(defrule craft-greydye ""

	(workbench)
	(ink)
	(bonemeal)
	(not (craftgreydye))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id greydye)
		craftgreydye
		(create$ "(greydye)" )))

(defrule craft-magmablock_ ""

	(workbench)
	(lavacream)
	(not (craftmagmablock_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id magmablock)
		craftmagmablock_
		(create$ "(magmablock)" )))

(defrule craft-glowstone ""

	(workbench)
	(glowstonedust)
	(not (craftglowstone))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id glowstone)
		craftglowstone
		(create$ "(glowstone)" )))

(defrule craft-glowstonedust_ ""

	(workbench)
	(glowstone)
	(not (craftglowstonedust_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id glowstonedust)
		craftglowstonedust_
		(create$ "(glowstonedust)" )))

(defrule craft-library ""

	(workbench)
	(board)
	(book)
	(not (craftlibrary))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id library)
		craftlibrary
		(create$ "(library)" )))

(defrule craft-cathedral ""

	(workbench)
	(board)
	(library)
	(not (craftcathedral))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id cathedral)
		craftcathedral
		(create$ "(cathedral)" )))

(defrule craft-musicbox ""

	(workbench)
	(board)
	(diamond)
	(not (craftmusicbox))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id musicbox)
		craftmusicbox
		(create$ "(musicbox)" )))

(defrule craft-glass ""

	(furnace)
	(sand)
	(fire)
	(coal)
	(not (craftglass))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id glass)
		craftglass
		(create$ "(glass)" )))

(defrule craft-glass_ ""

	(furnace)
	(sand)
	(fire)
	(wood)
	(not (craftglass_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id glass)
		craftglass_
		(create$ "(glass)" )))

(defrule craft-glass__ ""

	(furnace)
	(sand)
	(fire)
	(board)
	(not (craftglass__))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id glass)
		craftglass__
		(create$ "(glass)" )))

(defrule craft-iron_ ""

	(furnace)
	(ironore)
	(fire)
	(coal)
	(not (craftiron_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id iron)
		craftiron_
		(create$ "(iron)" )))

(defrule craft-iron__ ""

	(furnace)
	(ironore)
	(fire)
	(wood)
	(not (craftiron__))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id iron)
		craftiron__
		(create$ "(iron)" )))

(defrule craft-iron___ ""

	(furnace)
	(ironore)
	(fire)
	(board)
	(not (craftiron___))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id iron)
		craftiron___
		(create$ "(iron)" )))

(defrule craft-gold_ ""

	(furnace)
	(goldenore)
	(fire)
	(coal)
	(not (craftgold_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id gold)
		craftgold_
		(create$ "(gold)" )))

(defrule craft-gold__ ""

	(furnace)
	(goldenore)
	(fire)
	(wood)
	(not (craftgold__))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id gold)
		craftgold__
		(create$ "(gold)" )))

(defrule craft-gold___ ""

	(furnace)
	(goldenore)
	(fire)
	(board)
	(not (craftgold___))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id gold)
		craftgold___
		(create$ "(gold)" )))

(defrule craft-netheritescrap ""

	(furnace)
	(ancientwreckage)
	(fire)
	(coal)
	(not (craftnetheritescrap))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id netheritescrap)
		craftnetheritescrap
		(create$ "(netheritescrap)" )))

(defrule craft-netheritescrap_ ""

	(furnace)
	(ancientwreckage)
	(fire)
	(wood)
	(not (craftnetheritescrap_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id netheritescrap)
		craftnetheritescrap_
		(create$ "(netheritescrap)" )))

(defrule craft-netheritescrap__ ""

	(furnace)
	(ancientwreckage)
	(fire)
	(board)
	(not (craftnetheritescrap__))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id netheritescrap)
		craftnetheritescrap__
		(create$ "(netheritescrap)" )))

(defrule craft-bakedclay ""

	(furnace)
	(clay)
	(fire)
	(coal)
	(not (craftbakedclay))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id bakedclay)
		craftbakedclay
		(create$ "(bakedclay)" )))

(defrule craft-bakedclay_ ""

	(furnace)
	(clay)
	(fire)
	(wood)
	(not (craftbakedclay_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id bakedclay)
		craftbakedclay_
		(create$ "(bakedclay)" )))

(defrule craft-bakedclay__ ""

	(furnace)
	(clay)
	(fire)
	(board)
	(not (craftbakedclay__))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id bakedclay)
		craftbakedclay__
		(create$ "(bakedclay)" )))

(defrule craft-driedkelp ""

	(furnace)
	(laminaria)
	(fire)
	(coal)
	(not (craftdriedkelp))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id driedkelp)
		craftdriedkelp
		(create$ "(driedkelp)" )))

(defrule craft-driedkelp_ ""

	(furnace)
	(laminaria)
	(fire)
	(wood)
	(not (craftdriedkelp_))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id driedkelp)
		craftdriedkelp_
		(create$ "(driedkelp)" )))

(defrule craft-driedkelp__ ""

	(furnace)
	(laminaria)
	(fire)
	(board)
	(not (craftdriedkelp__))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id driedkelp)
		craftdriedkelp__
		(create$ "(driedkelp)" )))

(defrule craft-netheritepickaxe ""

	(blacksmithstable)
	(netherite)
	(pickaxe)
	(not (craftnetheritepickaxe))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id netheritepickaxe)
		craftnetheritepickaxe
		(create$ "(netheritepickaxe)" )))

(defrule craft-fish ""

	(water)
	(fishingrod)
	(not (craftfish))

	=>

		(handle-state interview
		?*target*
		(find-text-for-id fish)
		craftfish
		(create$ "(fish)" )))

