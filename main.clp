;+	Athanasakis Theodosios, 2903
;+	Makris Ioannis, 2950

;+	Steps in order to find the store with the contamination:
;+
;+	1. Asks the user which metrics is going to give a measurement for.
;+	2. Take the measurement for each metric.
;+	3. Based on the previous answers-measurements, the algorithm finds the stores 
;+	   that are candidates for contamination.
;+	4. Based on the candidated stores, the algorithm constructs a path for every store. 
;+	   Every path starts with the candidate store and finishes with the monitoring station.
;+	5. Then it finds the node(s) that is/are meeting point for at least two paths (if any).
;+	6. Based on the nodes been found in step 5, the algorithm takes the most right one (closer to the monitoring station).
;+	7. Then the program asks if there is a contamination in that node.
;+	8. Based on the answer of step 7, the paths and the candidated stores are being updated.
;+	9. The previous steps (5-8) are executed until there is only one path.
;+	10.Finally, the program prints the store with the contamination and the possible hazards of the chemical
;+	   that caused the contamination (if there is any).

(defclass chemical
	(is-a USER)
	(role concrete)
	(multislot colour
		(type SYMBOL)
		(allowed-values clear red white)
		(default clear)
		(create-accessor read-write))
	(multislot specific_gravity
		(type SYMBOL)
		(allowed-values equal_to_1 above_1 below_1)
		(default equal_to_1)
		(create-accessor read-write))
	(multislot radioactivity
		(type SYMBOL)
		(allowed-values no yes)
		(default no)
		(create-accessor read-write))
	(multislot pH-high
		(type INTEGER)
		(range 0 14)
		(create-accessor read-write))
	(multislot pH-low
		(type INTEGER)
		(range 0 14)
		(create-accessor read-write))
	(multislot solubility
		(type SYMBOL)
		(allowed-values soluble insoluble)
		(default soluble)
		(create-accessor read-write))
	(multislot smell
		(type SYMBOL)
		(allowed-values none vinegar choking)
		(default none)
		(create-accessor read-write))
	(multislot hazards
		(type SYMBOL)
		(allowed-values asphyxiation burns_skin explosive highly_toxic_PCBs)
		(create-accessor read-write))
	(multislot spectrometry
		(type SYMBOL)
		(allowed-values none sulphur carbon sodium metal)
		(default none)
		(create-accessor read-write))
	(slot candidate
		(type SYMBOL)
		(allowed-values yes no)
		(default yes)
		(create-accessor read-write)))

(defclass acid
	(is-a chemical)
	(role concrete)
	(multislot pH-high
		(type INTEGER)
		(range 0 14)
		(default 6)
		(create-accessor read-write))
	(multislot pH-low
		(type INTEGER)
		(range 0 14)
		(default 0)
		(create-accessor read-write)))

(defclass strong_acid
	(is-a acid)
	(role concrete)
	(multislot pH-high
		(type INTEGER)
		(range 0 14)
		(default 3)
		(create-accessor read-write))
	(multislot hazards
		(type SYMBOL)
		(allowed-values asphyxiation burns_skin explosive highly_toxic_PCBs)
		(default burns_skin)
		(create-accessor read-write)))

(defclass weak_acid
	(is-a acid)
	(role concrete)
	(multislot pH-low
		(type INTEGER)
		(range 0 14)
		(default 3)
		(create-accessor read-write)))

(defclass base
	(is-a chemical)
	(role concrete)
	(multislot pH-high
		(type INTEGER)
		(range 0 14)
		(default 14)
		(create-accessor read-write))
	(multislot pH-low
		(type INTEGER)
		(range 0 14)
		(default 8)
		(create-accessor read-write)))

(defclass strong_base
	(is-a base)
	(role concrete)
	(multislot pH-low
		(type INTEGER)
		(range 0 14)
		(default 11)
		(create-accessor read-write))
	(multislot hazards
		(type SYMBOL)
		(allowed-values asphyxiation burns_skin explosive highly_toxic_PCBs)
		(default burns_skin)
		(create-accessor read-write)))

(defclass weak_base
	(is-a base)
	(role concrete)
	(multislot pH-high
		(type INTEGER)
		(range 0 14)
		(default 11)
		(create-accessor read-write)))

(defclass oil
	(is-a chemical)
	(role concrete)
	(multislot pH-high
		(type INTEGER)
		(range 0 14)
		(default 8)
		(create-accessor read-write))
	(multislot pH-low
		(type INTEGER)
		(range 0 14)
		(default 6)
		(create-accessor read-write))
	(multislot solubility
		(type SYMBOL)
		(allowed-values soluble insoluble)
		(default insoluble)
		(create-accessor read-write))
	(multislot spectrometry
		(type SYMBOL)
		(allowed-values none sulphur carbon sodium metal)
		(default carbon)
		(create-accessor read-write)))

(defclass node
	(is-a USER)
	(role concrete)
	(multislot downstream
		(type INSTANCE)
		(allowed-classes node)
		(create-accessor read-write)))
	

(defclass store
	(is-a node)
	(role concrete)
	(multislot contents
		(type INSTANCE)
		(allowed-classes chemical)
		(cardinality 1 ?VARIABLE)
		(create-accessor read-write))
	(slot candidate
		(type SYMBOL)
		(allowed-values yes no)
		(default no)
		(create-accessor read-write)))

(defclass manhole
	(is-a node)
	(role concrete))

(definstances facts

([acetic_acid] of  weak_acid

	(smell vinegar))

([aluminium_hydroxide] of  weak_base

	(colour white)
	(specific_gravity above_1)
	(spectrometry metal))

([carbonic_acid] of  weak_acid

	(spectrometry carbon))

([chromogen_23] of  weak_base

	(colour red)
	(specific_gravity below_1))

([hydrochloric_acid] of  strong_acid

	(hazards asphyxiation burns_skin)
	(smell choking))

([manhole_1] of  manhole

	(downstream [manhole_9]))

([manhole_10] of  manhole

	(downstream [manhole_12]))

([manhole_11] of  manhole

	(downstream [manhole_13]))

([manhole_12] of  manhole

	(downstream [monitoring_station]))

([manhole_13] of  manhole

	(downstream [monitoring_station]))

([manhole_2] of  manhole

	(downstream [manhole_9]))

([manhole_3] of  manhole

	(downstream [manhole_9]))

([manhole_4] of  manhole

	(downstream [manhole_10]))

([manhole_5] of  manhole

	(downstream [manhole_10]))

([manhole_6] of  manhole

	(downstream [manhole_11]))

([manhole_7] of  manhole

	(downstream [manhole_11]))

([manhole_8] of  manhole

	(downstream [manhole_13]))

([manhole_9] of  manhole

	(downstream [manhole_12]))

([monitoring_station] of  node
)

([petrol] of  oil

	(hazards explosive)
	(specific_gravity below_1))

([rubidium_hydroxide] of  weak_base

	(radioactivity yes)
	(specific_gravity above_1)
	(spectrometry metal))

([sodium_hydroxide] of  strong_base

	(spectrometry sodium))

([store_1] of  store

	(contents
		[sulphuric_acid]
		[petrol])
	(downstream [manhole_1]))

([store_2] of  store

	(contents
		[hydrochloric_acid]
		[acetic_acid])
	(downstream [manhole_2]))

([store_3] of  store

	(contents
		[rubidium_hydroxide]
		[transformer_oil])
	(downstream [manhole_3]))

([store_4] of  store

	(contents
		[carbonic_acid]
		[acetic_acid]
		[petrol])
	(downstream [manhole_4]))

([store_5] of  store

	(contents
		[chromogen_23]
		[sulphuric_acid]
		[petrol])
	(downstream [manhole_5]))

([store_6] of  store

	(contents
		[aluminium_hydroxide]
		[transformer_oil]
		[carbonic_acid])
	(downstream [manhole_6]))

([store_7] of  store

	(contents
		[hydrochloric_acid]
		[sulphuric_acid])
	(downstream [manhole_7]))

([store_8] of  store

	(contents
		[acetic_acid]
		[carbonic_acid]
		[sodium_hydroxide])
	(downstream [manhole_8]))

([sulphuric_acid] of  strong_acid

	(spectrometry sulphur))

([transformer_oil] of  oil

	(hazards highly_toxic_PCBs))

)

;+ Functions

;+ Function for printing and cheking an input.
;+ Returns an acceptable answer.
(deffunction ask-string (?question $?possible-answers)
	(printout t ?question " " $?possible-answers ": ")
	(bind ?answer (read))
	(while (not (member$ ?answer $?possible-answers)) do
		(printout t ?question " " $?possible-answers ": ")
		(bind ?answer (read))
	)
	
	(return ?answer)
)

;+ Function for printing and cheking a number in a range.
;+ Returns an acceptable answer.
(deffunction ask-number (?question ?min ?max)
	(printout t ?question " (range " ?min "-" ?max "): ")
	(bind ?answer (read))
	(while (not (and (numberp ?answer)
			(>= ?answer ?min) 
			(<= ?answer ?max)))
	do
		(printout t ?question " (range " ?min "-" ?max "): ")
		(bind ?answer (read))
	)
	
	(return ?answer)
)




;+ Rules

;+ Input Stage
;+ ##################################################
(defrule initial-question "Program start with initial question"
	?x <- (initial-fact)
 =>
	(retract ?x)
	(set-strategy mea)
	(printout t "What metrics do you want to give? "
		"(pH solubility spectrometry colour smell specific_gravity radioactivity): ")
	(bind ?answer (explode$ (readline)))
	(printout t crlf)
	(assert (metrics ?answer))
	(assert (goal take-metrics))
)

(defrule ask-questions "Questions about the metrics we have"
	?x <- (goal take-metrics)
	?y <- (metrics $?answer)
=>
	(retract ?x)
	
	(if (member$ pH $?answer) then
		(assert (pH (ask-number "What's the pH?" 0 14)))
	)
	
	(if (member$ solubility $?answer) then
		(assert (solubility (ask-string "Is it soluble?" soluble insoluble)))
	)

	(if (member$ spectrometry $?answer) then
		(assert (spectrometry (ask-string "What's the spectrometry?" carbon solphur metal sodium)))
	)
	
	(if (member$ colour $?answer) then
		(assert (colour (ask-string "What's the colour?" clear red white)))
	)
	
	(if (member$ smell $?answer) then
		(assert (smell (ask-string "What's the smell?" none choking vinegar)))
	)
	
	(if (member$ specific_gravity $?answer) then
		(assert (specific_gravity (ask-number "What's the specific gravity?" 0.9 1.1)))
	)
	
	(if (member$ radioactivity $?answer) then
		(assert (radioactivity (ask-string "Is it radioactive?" yes no)))
	)
	
	(printout t crlf)
	(retract ?y)
	(assert (goal classification))
)

;+ ##################################################


;+ Chemical Innocence Stage
;+ ##################################################

(defrule check-pH "If pH is out of range, the chemical is innocent"
	(pH ?ph)
	(object (is-a chemical)
		(name ?x)
		(pH-high ?high)
		(pH-low ?low)
		(candidate yes)
	)
	(test (or (> ?ph ?high) (< ?ph ?low)))
=>
	(modify-instance ?x (candidate no))
)

(defrule check-solubility "If solubility of chemical is different, the chemical is innocent"
	(solubility ?metric)
	(object (is-a chemical)
		(name ?x)
		(solubility ?chem)
		(candidate yes)
	)
	(test (neq ?metric ?chem))
=>
	(modify-instance ?x (candidate no))
)

(defrule check-spectrometry "If spectrometry of chemical is different, the chemical is innocent"
	(spectrometry ?metric)
	(object (is-a chemical)
		(name ?x)
		(spectrometry ?chem)
		(candidate yes)
	)
	(test (neq ?metric ?chem))
=>
	(modify-instance ?x (candidate no))
)

(defrule check-colour "If colour of chemical is different, the chemical is innocent"
	(colour ?metric)
	(object (is-a chemical)
		(name ?x)
		(colour ?chem)
		(candidate yes)
	)
	(test (neq ?metric ?chem))
=>
	(modify-instance ?x (candidate no))
)

(defrule check-smell "If smell of chemical is different, the chemical is innocent"
	(smell ?metric)
	(object (is-a chemical)
		(name ?x)
		(smell ?chem)
		(candidate yes)
	)
	(test (neq ?metric ?chem))
=>
	(modify-instance ?x (candidate no))
)


(defrule check-specific_gravity-below_1 "If specific gravity of chemical is <1, assert the specific gravity as below_1"
	(declare (salience 10))
	?x <- (specific_gravity ?metric)
	(test (numberp ?metric))
	(test (< ?metric 1))
=>
	(retract ?x)
	(assert (specific_gravity below_1))
)

(defrule check-specific_gravity-equal_to_1 "If specific gravity of chemical is =1, assert the specific gravity as equal_to_1"
	(declare (salience 10))
	?x <- (specific_gravity ?metric)
	(test (numberp ?metric))
	(test (= ?metric 1))
=>
	(retract ?x)
	(assert (specific_gravity equal_to_1))
)

(defrule check-specific_gravity-above_1 "If specific gravity of chemical is >1, assert the specific gravity as above_1"
	(declare (salience 10))
	?x <- (specific_gravity ?metric)
	(test (numberp ?metric))
	(test (> ?metric 1))
=>
	(retract ?x)
	(assert (specific_gravity above_1))
)

(defrule check-specific_gravity "If specific gravity of chemical is different, the chemical is innocent"
	(specific_gravity ?metric)
	(object (is-a chemical)
		(name ?x)
		(specific_gravity ?chem)
		(candidate yes)
	)
	(test (neq ?metric ?chem))
=>
	(modify-instance ?x (candidate no))
)

(defrule check-radioactivity "If radioactivity of chemical is different, the chemical is innocent"
	(radioactivity ?metric)
	(object (is-a chemical)
		(name ?x)
		(radioactivity ?chem)
		(candidate yes)
	)
	(test (neq ?metric ?chem))
=>
	(modify-instance ?x (candidate no))
)

(defrule go-to-check-store "Changes the target after guilty chemicals have been found"
	(declare (salience -10))
	?x <- (goal classification)
=>
	(retract ?x)
	(assert (goal check-stores))
)
	
;+ ##################################################


;+ Store Innocence Stage
;+ ##################################################

(defrule check-store "Check if the store is guilty"
	(goal check-stores)
	(object (is-a store)
		(name ?x)
		(contents $?chems)
		(candidate no)
	)
	(object (is-a chemical)
		(name ?y)
		(candidate yes)
	)
	(test (member$ ?y $?chems))
=>
	(modify-instance ?x (candidate yes))
)

(defrule go-to-make-paths "Changes the target after guilty stores have been found"
	(declare (salience -10))
	?x <- (goal check-stores)
=>
	(retract ?x)
	(assert (goal make-paths))
)

;+ ##################################################


;+ Path Finding Stage
;+ ##################################################

(defrule first-downstream-finding "Finds the first downstream from guilty store"
	(goal make-paths)
	(object (is-a store)
		(name ?x)
		(downstream ?down)
		(candidate yes)
	)
=>
	(assert (path ?x ?down))
)

(defrule path-finding "Finds the path from a guilty store to monitoring station"
	?y <- (path $?path ?last)
	(object (is-a node)
		(name ?x)
		(downstream ?down)
	)
	(test (eq ?last ?x))
=>
	(retract ?y)
	(assert (path $?path ?last ?down))
)

(defrule go-to-ask-questions-for-the-guilty "Changes the target after paths from guilty stores to monitoring station have been found"
	(declare (salience -10))
	?x <- (goal make-paths)
=>
	(retract ?x)
	(assert (goal ask-questions-for-the-guilty))
	(assert (goal guilty-manholes))
)

;+ ##################################################


;+ Real Guilty Finding Stage
;+ ##################################################

(defrule guilty-manholes "Finds the manholes that exist in more than one guilty paths"
	(goal ask-questions-for-the-guilty)
	(goal guilty-manholes)
	(path $? ?before-x ?x $?)
	(path $? ?before-y ?y $?)
	(test (neq ?before-x ?before-y))
	(test (eq ?x ?y))
=>
	(assert (check-manhole ?x))
)

(defrule manhole-for-questioning "Finds the manhole that needs question"
	(goal ask-questions-for-the-guilty)
	(goal guilty-manholes)
	(path $? ?left $? ?right $?)
	?x <- (check-manhole ?wrong-manhole)
	(check-manhole ?right-manhole)
	(test (eq ?right ?right-manhole))
	(test (eq ?left ?wrong-manhole))
=>
	(retract ?x)
)

(defrule go-to-make-question-at-manhole "Changes the target after the manhole for questioning have been found"
	(declare (salience -5))
	(goal ask-questions-for-the-guilty)
	?x <- (goal guilty-manholes)
=>
	(retract ?x)
	(assert (goal question-manhole))
)

(defrule make-question-at-manhole "Make question at the manhole"
	(goal ask-questions-for-the-guilty)
	?x <- (goal question-manhole)
	?y <- (check-manhole ?parent)
	(object (is-a node)
		(name ?child1)
		(downstream ?one)
	)
	(object (is-a node)
		(name ?child2)
		(downstream ?other)
	)
	(test (eq ?one ?other))
	(test (eq ?one ?parent))
	(test (neq ?child1 ?child2))
=>
	(retract ?x)
	(retract ?y)
	(assert (question-manhole ?child1 (ask-string (implode$ (create$ Is there contamination in (instance-name-to-symbol ?child1))) yes no)))
)

(defrule overhaul-the-paths-if-answer-yes "Overhaul the paths after the answer (yes) about manhole contamination"
	(goal ask-questions-for-the-guilty)
	(question-manhole ?manhole yes)
	?x <- (path ?f $?path)
	(object (is-a store)
		(name ?store)
		(candidate yes)
	)
	(test (not (member$ ?manhole $?path)))
	(test (eq ?f ?store))
=>
	(retract ?x)
	(modify-instance ?store (candidate no))
)

(defrule overhaul-the-paths-if-answer-no "Overhaul the paths after the answer (no) about manhole contamination"
	(goal ask-questions-for-the-guilty)
	(question-manhole ?manhole no)
	?x <- (path ?f $?path)
	(object (is-a store)
		(name ?store)
		(candidate yes)
	)
	(test (member$ ?manhole $?path))
	(test (eq ?f ?store))
=>
	(retract ?x)
	(modify-instance ?store (candidate no))
)

(defrule overhaul-the-manhole-for-questioning "Changes the target after the paths have been overhauled"
	(declare (salience -5))
	(goal ask-questions-for-the-guilty)
	?x <- (question-manhole $?)
=>
	(retract ?x)
	(assert (goal guilty-manholes))
)


(defrule go-to-print-results "Changes the target after the real guilty has been found"
	(declare (salience -10))
	?x <- (goal ask-questions-for-the-guilty)
	?y <- (goal question-manhole)
	?z <- (path $?)
=>
	(printout t crlf)
	(retract ?x)
	(retract ?y)
	(retract ?z)
	(assert (goal print-hazards))
)

;+ ##################################################


;+ Print Results Stage
;+ ##################################################

(defrule print-hazard
	(goal print-hazards)
	(object (is-a chemical)
		(name ?x)
		(hazards $?haz)
		(candidate yes)
	)
	(object (is-a store)
		(name ?y)
		(contents $?chems)
		(candidate yes)
	)
	(test (member$ ?x $?chems))
=>
	(printout t "Possible chemical that created the contamination is " (instance-name-to-symbol ?x) crlf)
	(if (> (length$ $?haz) 0) then
		(printout t "Possible outcomes: " (implode$ (create$ $?haz)) crlf)
	)
	(printout t crlf)
)

(defrule go-to-print-store "Changes the target after the guilty chemicals and hazards of them have been printed"
	(declare (salience -10))
	?x <- (goal print-hazards)
=>
	(printout t crlf)
	(retract ?x)
	(assert (goal print-store))
)


(defrule print-store
	?y <- (goal print-store)
	(object (is-a store)
		(name ?x)
		(candidate yes)
	)
=>
	(printout t "Contamination at: " (instance-name-to-symbol ?x) crlf)
	(printout t crlf)
	(printout t crlf)
	(printout t crlf)
	(retract ?y)
)

;+ ##################################################

			
	











