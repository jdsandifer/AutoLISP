;;;;;;;[  Helper Functions  ];;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  All shared helper functions.                ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 01/19/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  01/19/2016                                  ;;
;;  - Added functions from PlanDraw(s).         ;;
;;  - Cleaned up some formatting.               ;;
;;                                              ;;
;;  01/23/2016                                  ;;
;;  - Added RoundBy function.                   ;;
;;                                              ;;
;;  02/06/2016                                  ;;
;;  - Added SwapListItems function.             ;;
;;  - Added BubbleSort function.                ;;
;;                                              ;;
;;  02/08/2016                                  ;;
;;  - Found better solution for sorting         ;;
;;    without removing and updated all sorting  ;;
;;    functions except mine.                    ;;
;;  - Added &Set prefix to my sort method to    ;;
;;    show that it sets and sorts.              ;;
;;  - BubbleSort - takes in any comparison      ;;
;;    function.  (Tested - already did!)        ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Make functions more efficient.            ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;; Assoc+Qty  -  J.D. (inspired by Assoc++ by Lee Mac)
;;; Adds to the value of a key in an association list if present,
;;; else adds key to the list with the new amount.
;;; theKey - [any] Key of an element in the list
;;; theList - [lst] Association list (may be nil)
;;; addQuantity - [int] Number of key to add (negative number = subtract)

(defun Assoc+Qty ( theKey theList addQuantity / item )
   (if (setq item (assoc theKey theList))
      (setq theList
			   (subst (cons theKey (+ (cdr item) addQuantity)) item theList))
      (setq theList (cons  (cons theKey addQuantity) theList)))
	(if (<= (cdr (assoc theKey theList)) 0)
		(setq theList (vl-remove (assoc theKey theList) theList)))
   theList)




;; Assoc++ - Simplifier for Assoc+Qty
;; Increments the value of a key in an association list if present, 
;; else adds key to the list.
;; theKey - [any] Key of an element in the list
;; theList - [list] Association list (may be nil)

(defun Assoc++ ( theKey theList / )
   (Assoc+Qty theKey theList 1))


	

; Assoc-- - Simplifier for Assoc+Qty
; Decrements list item by one or removes it if qty is zero
; theKey - [any] Key of an element in the list
; theList - [list] Association list (may be nil)

(defun Assoc-- ( theKey theList / )
   (Assoc+Qty theKey theList -1))


	

;; BubbleSort - does bubble sort using supplied function
;; theList - [list] list to be sorted
;; functionName [symbol] - function to use in sorting

(defun BubbleSort (theList functionName / 
					    stillSwapping index theList temp)
   (setq stillSwapping T
			index 1)		; loop through starting with second value
	(while stillSwapping
		(setq stillSwapping nil)
		(while (< index (length theList))
			(cond (
				(not (or ((eval functionName)(nth (1- index) theList)(nth index theList))
							(= (nth (1- index) theList)(nth index theList))))
				(setq theList (SwapListItems theList (1- index)))
				(setq stillSwapping T)))
			(setq index (1+ index)))	
		(setq index 1))
	theList)
	

	

;; SwapListItems - Returns list with two neighboring items swapped
;; theList - [list] list to be sorted
;; firstItemIndex [integer] - index of first item to be sorted

(defun SwapListItems (theList firstItemIndex / index finalList)
   (setq index 0)
	(while (< index (length theList))
		(if (/= index firstItemIndex)
			(setq finalList (append finalList (list (nth index theList))))
			(progn
				(setq finalList (append finalList (list (nth (1+ index)
																			theList))))
				(setq finalList (append finalList (list (nth index theList))))
				(setq index (1+ index))))
		
		(setq index (1+ index)))
	finalList)
	
	
	
	
;; DisplayCount - Displays the count list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun DisplayCount (result)
   (princ result)
   (princ "\n")
   (princ))

	
	
	
;; DTR - (Converts) Degrees To Radians - useful for polar command
;; angleInDegrees [ integer ]

(defun DTR ( angleInDegrees )
	(* PI (/ angleInDegrees 180.0)))




;;; FindClosestIntegerDivisor
;;; Finds the integer divisor to create a result closest to the goal (ties = smaller divisor for larger spacing).

(defun FindClosestIntegerDivisor (numerator goal / aim x pivotDivisor result)
   (setq aim (/ (* 1.0 numerator) goal))
   (setq x (fix aim))
   (setq pivotDivisor (/ (* 2.0 X (+ X 1.0)) (+ (* 2.0 X) 1.0)))	; Complicated equation to determine pivot divisor -
   (if (<= aim pivotDivisor)                                      ; the one that determines which way to round.
      (setq result x)
      (setq result (RoundUpTo 1 aim))))

	
	
	
;;; MakeVarNotNil - simple function for verifying variables are set and
;;;                 giving them a default value if not
;;; variableName [symbol] - name of variable to check
;;; valueToSetIfEmpty [atom] - default value to use if nothing is set

(defun MakeVarNotNil (variableName valueToSetIfEmpty)
	(if (or (= (eval variableName) nil) (= (eval variableName) ""))		
      (set variableName valueToSetIfEmpty)
		(eval variableName)))
		
		
		
;;; Set&SortListBy - Sorts the list by the provided comparison function.
;;; functionName - [symbol] the function to use 
;;; listName - [symbol] name of the list to sort

(defun Set&SortListBy (listName functionName)
   (set listName (BubbleSort (eval listName) functionName)))
	
	
	
;;; OrderListBy - Sorts the list by the provided comparison function
;;; 					using vl-sort-i to get sorted indexes and assembling a 
;;;   			   sorted list with those indexes.
;;; thelist - [list] name of the list to sort
;;; functionName - [symbol] the function to use 
;;; Based on (mapcar '(lambda (x) (nth x a)) (vl-sort-i a '<))
;;; provided by *Kim, HeeTea on forums.autodesk.com, 1/23/2002 (02/06/16)



(defun OrderListBy (thelist functionName)
   (mapcar '(lambda (index) (nth index thelist))
				(vl-sort-i thelist functionName)))
	
	
	
;;; OrderList - List ordering using OrderListBy (legacy support)
;;; theList - [association list] Sorts in descending (alphanumerical) order by keys. 

(defun OrderList (theList)
   (OrderListBy theList '(lambda (e1 e2) (> (car e1) (car e2)))))




;; OrderListAscending - List ordering using OrderListBy (legacy support)
;; list - [association list] Sorts in ascending (alphabetical) order by keys. 

(defun OrderListAscending (theList)
   (OrderListBy theList '(lambda (e1 e2) (< (car e1) (car e2)))))




; ResetCutList - Resets global cut list.

(defun ResetCutList ()
   (setq *cutList* nil)
   (princ "Cut list has been reset.")
   (princ))
	
		
	
	
;;; ResetInfillCutList - Resets global infill cut list.

(defun ResetInfillCutList ()
   (setq *infillCutList* nil)
   (princ "Infill cut list was reset.")
   (princ))

	
	

;;; RoundTo
;;; Rounds the value to the nearest multiple of "multiple" and returns it as an integer.

(defun RoundTo (multiple value)
   (cond
      ((< (rem value multiple) (/ multiple 2.0))
         (fix (- value (rem value multiple))))
      ((>= (rem value multiple) (/ multiple 2.0))
			(fix (+ (- value (rem value multiple)) multiple))) ))
			
			
			
			
;;; RoundUpBy
;;; Rounds the value up to the next multiple of "multiple" and returns it as an integer.

(defun RoundUpBy (multiple value)
   (if (= (rem value multiple) 0)
      ; Then
      (fix value)
      ; Else
      (fix (+ (- value (rem value multiple)) multiple))))

		 


;;; Rounding up helper function (legacy support - uses new RoundUpTo function)

(defun RoundUpInt (value)
   (RoundUpBy 1 value) )




;;; RoundUpTo
;;; Rounds the value up to the next multiple of "multiple" and returns it as an integer.

(defun RoundUpTo (multiple value)
   (RoundUpBy multiple value))



		
(princ)		; Clean load