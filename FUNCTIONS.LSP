;;;;;;;[  Helper Functions  ];;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  General shared helper functions file.       ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 01/19/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  08/29/2016                                  ;;
;;  - Added SortAssocListKeys and               ;;
;;    SortAssocListValues.                      ;;
;;  - Added unit tests for above.               ;;
;;                                              ;;
;;  08/24/2016                                  ;;
;;  - Added DisplayAssocList.                   ;;
;;                                              ;;
;;  06/15/2016                                  ;;
;;  - Added CarCdr: Return first list item AND  ;;
;;    remove it from the list.                  ;;
;;                                              ;;
;;  04/13/2016                                  ;;
;;  - Added FilterSelectionSet.                 ;;
;;                                              ;;
;;  03/16/2016                                  ;;
;;  - Moved math functions to FUNCTIONS_MATH.   ;;
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


; Runs all unit tests for this file
; Output: T if all unit tests passed, else nil

(defun C:TestFunctions ( / testList)
	;; Setup for tests
	(princ "\n")
	(princ "Testing FUNCTIONS\n")
	(princ "--------------------\n")
	
	;; Actual tests
	(princ "SortAssocListKeys\n")
	(Assert 'SortAssocListKeys '('() '<) '())
	(Assert 'SortAssocListKeys '('(("a" . 1)) '<) '(("a" . 1)))
	(Assert
		'SortAssocListKeys
		'('(("a" . 1)("b" . 0)) '<)
		'(("a" . 1)("b" . 0)))
	(Assert
		'SortAssocListKeys
		'('(("a" . 1)("c" . 3)("b" . 0)) '<)
		'(("a" . 1)("b" . 0)("c" . 3)))
	(Assert
		'SortAssocListKeys
		'('(("a" . 1)("c" . 3)("b" . 0)) '>)
		'(("c" . 3)("b" . 0)("a" . 1)))
		
	(princ "\n")
	(princ "SortAssocListValues\n")
	(Assert 'SortAssocListValues '('() '<) '())
	(Assert 'SortAssocListValues '('(("a" . 1)) '<) '(("a" . 1)))
	(Assert
		'SortAssocListValues
		'('(("a" . 1)("b" . 0)) '<)
		'(("b" . 0)("a" . 1)))
	(Assert
		'SortAssocListValues
		'('(("a" . 1)("c" . 3)("b" . 0)) '<)
		'(("b" . 0)("a" . 1)("c" . 3)))
	(Assert
		'SortAssocListValues
		'('(("a" . 1)("c" . 3)("b" . 0)) '>)
		'(("c" . 3)("a" . 1)("b" . 0)))
		
	(princ "\n")
	(princ "ListRemove\n")
	(setq aList '(1))
	(princ aList)(princ "\n")
	(Assert 'ListRemove '(0 'aList) 1)
	(setq aList '(1))
	(Verify 'ListRemove '(0 'aList) '(equal alist '()))
	(setq aList '(1 2))
	(princ aList)(princ "\n")
	(Assert 'ListRemove '(0 'aList) 1)
	(setq aList '(1 2))
	(Verify 'ListRemove '(0 'aList) '(equal alist '(2)))
	(setq aList '(1 2))
	(Assert 'ListRemove '(1 'aList) 2)
	(setq aList '(1 2))
	(Verify 'ListRemove '(1 'aList) '(equal alist '(1)))
	(setq aList '(1 2 3))
	(princ aList)(princ "\n")
	(Assert 'ListRemove '(0 'aList) 1)
	(setq aList '(1 2 3))
	(Verify 'ListRemove '(0 'aList) '(equal alist '(2 3)))
	(setq aList '(1 2 3))
	(Assert 'ListRemove '(1 'aList) 2)
	(setq aList '(1 2 3))
	(Verify 'ListRemove '(1 'aList) '(equal alist '(1 3)))
	(setq aList '(1 2 3))
	(Assert 'ListRemove '(2 'aList) 3)
	(setq aList '(1 2 3))
	(Verify 'ListRemove '(2 'aList) '(equal alist '(1 2)))
	
	(princ "\n")
	(princ "ListSearch\n")
	(setq aList '())
	(princ aList)(princ "\n")
	(Assert 'ListSearch '('(= 1 (nth i theList)) aList) nil)
	(setq aList '(1))
	(princ aList)(princ "\n")
	(Assert 'ListSearch '('(= 1 (nth i theList)) aList) 0)
	(setq aList '(1 2))
	(princ aList)(princ "\n")
	(Assert 'ListSearch '('(= 1 (nth i theList)) aList) 0)
	(Assert 'ListSearch '('(= 2 (nth i theList)) aList) 1)
	(setq aList '("a" "b" "c"))
	(princ aList)(princ "\n")
	(Assert 'ListSearch '('(= "a" (nth i theList)) aList) 0)
	(Assert 'ListSearch '('(= "b" (nth i theList)) aList) 1)
	(Assert 'ListSearch '('(= "c" (nth i theList)) aList) 2)
		
	;; Displaying the results of the tests
	(JD:PrintTestResults (JD:CountBooleans testList)))
		
	

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

	

;; DisplayCount - Displays the count list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun DisplayCount (result)
   (princ result)
   (princ "\n")
   (princ))
	
	
	
;;; JD:CarCdr - same function as car followed by cdr (returns/removes 1st item)
;;; listName [symbol] - name of list to work on

(defun JD:CarCdr (listName / firstItem)
	(setq firstItem (car (eval listName)))
	(set listName (cdr (eval listName)))
	firstItem)
		
		

; Removes the item at index n and returns it. List is altered.
; Arg: n [int 0 to list length-1] = index to remove, listName = symbol of list
; Ret: nth item from list
; FX:  list no longer contains nth item
		
(defun ListRemove (n listName / returnItem newList)
	(setq returnItem (nth n (eval listName))) 
	(foreach i (Range 0 (- (length (eval listName)) 1) 1)
		(if (/= i n)
			(setq newList (append newList (list (nth i (eval listName)))))))
	(set listName newList)
	returnItem)

	

; Returns first index of item that matches criteria
; Arg: criteria ['(comparator test (nth i theList)], theList = list to search
; Ret: index of item if found, nil if not
	
(defun ListSearch (criteria theList / n)
	(if (/= nil theList)
		(foreach i (Range 0 (1- (length theList)) 1)
			(if (eval criteria)
				(setq n i)))
		(setq n nil))
		n)
			
			
			
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


	
;;; SortAssocListKeys - Sorts the list by the provided comparison function
;;; 					using vl-sort-i to get sorted indexes and assembling a 
;;;   			   sorted list with those indexes.
;;; thelist - [list] name of the list to sort
;;; functionName - [symbol] the function to use 
;;; Based on (mapcar '(lambda (x) (nth x a)) (vl-sort-i a '<))
;;; provided by *Kim, HeeTea on forums.autodesk.com, 1/23/2002 (02/06/16)



(defun SortAssocListKeys (theAssoclist functionName)
   (mapcar '(lambda (index) (nth index theAssoclist))
				(vl-sort-i
					(mapcar
						'car theAssoclist)
					functionName)))
	
	
	
;;; SortAssocListValues - Sorts the list by the provided comparison function
;;; 					using vl-sort-i to get sorted indexes and assembling a 
;;;   			   sorted list with those indexes.
;;; thelist - [list] name of the list to sort
;;; functionName - [symbol] the function to use 
;;; Based on (mapcar '(lambda (x) (nth x a)) (vl-sort-i a '<))
;;; provided by *Kim, HeeTea on forums.autodesk.com, 1/23/2002 (02/06/16)



(defun SortAssocListValues (theAssoclist functionName)
   (mapcar '(lambda (index) (nth index theAssoclist))
				(vl-sort-i
					(mapcar
						'cdr theAssoclist)
					functionName)))
	
	
	
;;; UnAssoc - Turns an assoc list back into a regular list (multiples)
;;; theAssoclist - [assoc list] name of the list to split up

(defun UnAssoc (theAssoclist / newList)
   (foreach item theAssoclist
		))
					
					
					
;; DisplayAssocList - Displays the association list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun JD:DisplayAssocList (assocList)
   (princ "\n=Desc==Qty=\n")
   (foreach item assocList
      (progn
			(princ " ")
			(princ (car item))
			(princ " . ")
			(princ (cdr item))
			(princ "\n")))
   (princ "===========\n") )



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
	
	
		
;;----------------------------------------------------------------------;;


(princ
    (strcat
        "\n:: FUNCTIONS.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;