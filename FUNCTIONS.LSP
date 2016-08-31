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
;;  - Added SortKeys and                        ;;
;;    SortValues.                               ;;
;;  - Added unit tests for above.               ;;
;;  - Added QtyList & UnQtyList.                ;;
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
	(princ "\n")
	(princ "Testing FUNCTIONS\n")
	(princ "--------------------\n")
	
	;; Actual tests
	(princ "Assoc*\n")
	(Assert 'Assoc* '(1 '()) '())
	(Assert 'Assoc* '(1 '((222 . 1))) '((222 . 1)))
	(Assert 'Assoc* '(0 '((222 . 1))) '((222 . 1)))
	(Assert 'Assoc* '(-5 '((222 . 1))) '((222 . 1)))
	(Assert 'Assoc* '(7 '((222 . 1))) '((222 . 7)))
	(Assert
		'Assoc* 
		'(1 '((222 . 3)(60 . 10)(12 . 5))) 
		'((222 . 3)(60 . 10)(12 . 5)))
	(Assert
		'Assoc* 
		'(4 '((222 . 3)(60 . 10)(12 . 5))) 
		'((222 . 12)(60 . 40)(12 . 20)))
	
	(princ "\n")
	(princ "AssocAppend\n")
	(Assert 'AssocAppend '('() '()) '())
	(Assert 'AssocAppend '('((222 . 1)) '()) '((222 . 1)))
	(Assert 'AssocAppend '('() '((222 . 1))) '((222 . 1)))
	(Assert 'AssocAppend '('((222 . 1)) '((222 . 1))) '((222 . 2)))
	(Assert 'AssocAppend '('((242 . 1)) '((222 . 1))) '((242 . 1)(222 . 1)))
	(Assert
		'AssocAppend 
		'('((242 . 1)(222 . 1)) '((222 . 1)(12 . 5))) 
		'((242 . 1)(222 . 2)(12 . 5)))
	(Assert
		'AssocAppend 
		'('((222 . 1)(60 . 3)(24 . 1)) '((242 . 1)(222 . 1)(24 . 2)(12 . 5))) 
		'((242 . 1)(222 . 2)(60 . 3)(24 . 3)(12 . 5)))
	
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
	(setq aList '(1 2 3))
	(princ aList)(princ "\n")
	(Assert 'ListSearch '('(<= 1 (nth i theList)) aList) 0)
	(Assert 'ListSearch '('(<= 2 (nth i theList)) aList) 1)
	(Assert 'ListSearch '('(<= 3 (nth i theList)) aList) 2)
	
	(princ "\n")
	(princ "SortKeys\n")
	(Assert 'SortKeys '('() '<) '())
	(Assert 'SortKeys '('(("a" . 1)) '<) '(("a" . 1)))
	(Assert
		'SortKeys
		'('(("a" . 1)("b" . 0)) '<)
		'(("a" . 1)("b" . 0)))
	(Assert
		'SortKeys
		'('(("a" . 1)("c" . 3)("b" . 0)) '<)
		'(("a" . 1)("b" . 0)("c" . 3)))
	(Assert
		'SortKeys
		'('(("a" . 1)("c" . 3)("b" . 0)) '>)
		'(("c" . 3)("b" . 0)("a" . 1)))
		
	(princ "\n")
	(princ "Sort\n")
	(Assert 'Sort '('() '<) '())
	(Assert 'Sort '('(1) '<) '(1))
	(Assert 'Sort '('("a" "b") '<) '("a" "b"))
	(Assert 'Sort '('("a" "b") '>) '("b" "a"))
	(Assert 'Sort '('(1 2 3) '<) '(1 2 3))
	(Assert 'Sort '('(2 3 1) '<) '(1 2 3))
	(Assert 'Sort '('(3 2 1) '<) '(1 2 3))
	(Assert 'Sort '('(1 3 2) '<) '(1 2 3))
	(Assert 'Sort '('(4 1 1022 3 10) '<) '(1 3 4 10 1022))
		
	(princ "\n")
	(princ "SortValues\n")
	(Assert 'SortValues '('() '<) '())
	(Assert 'SortValues '('(("a" . 1)) '<) '(("a" . 1)))
	(Assert
		'SortValues
		'('(("a" . 1)("b" . 0)) '<)
		'(("b" . 0)("a" . 1)))
	(Assert
		'SortValues
		'('(("a" . 1)("c" . 3)("b" . 0)) '<)
		'(("b" . 0)("a" . 1)("c" . 3)))
	(Assert
		'SortValues
		'('(("a" . 1)("c" . 3)("b" . 0)) '>)
		'(("c" . 3)("a" . 1)("b" . 0)))
		
	(princ "\n")
	(princ "QtyList\n")
	(Assert 'QtyList '('()) '())
	(Assert 'QtyList '('("a")) '(("a" . 1)))
	(Assert 'QtyList '('("a" "b" 100)) '(("a" . 1)("b" . 1)(100 . 1)))
		
	(princ "\n")
	(princ "UnQtyList\n")
	(Assert 'UnQtyList '('()) '())
	(Assert 'UnQtyList '('(("a" . 1))) '("a"))
	(Assert 'UnQtyList '('(("a" . 1)("b" . 1)(100 . 1))) '("a" "b" 100))
	(Assert
		'UnQtyList 
		'('((242 . 3)(180 . 2)(24 . 10)(12 . 2)))
		'(242 242 242 180 180 24 24 24 24 24 24 24 24 24 24 12 12))
	
	(princ "\n")
	(princ "JD:DisplayAssocList \n")	
	(JD:DisplayAssocList '(("A" . "apple")("B" . "banana")("C" . "canteloupe"))) 
	
	(princ "\n")
	(princ "JD:DisplayQtyList \n")	
	(JD:DisplayQtyList '((242 . 1)(180 . 2)("RUN A" . 3))) 
		
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
      (setq theList (append  theList (list (cons theKey addQuantity)))))
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

	

; Assoc* - Multiplies each item in the qtylist by multiplier
; multiplier - [int] amount to multiply each item by
; qtyList - [qty list] the list to multiply (may be nil)

(defun Assoc* ( multiplier qtyList / )
	(if (> multiplier 0)
		(foreach item qtyList
			(setq qtylist 
				(subst (cons (car item) (* multiplier (cdr item))) item qtylist)))
		qtylist))
	
	
	
; AssocAppend - Adds one qtylist to another
; qtyListA - [qty list] the list to multiply (may be nil)
; qtyListB - [qty list] the list to multiply (may be nil)

(defun AssocAppend ( qtyListA qtyListB / )
	(if qtyListA
		(progn
			(foreach item qtyListB
				(setq qtyListA
					(Assoc+Qty (car item) qtyListA (cdr item))))
			(SortKeys qtyListA '>))
		qtyListB))
	
	
	
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
	(cond
		(	(/= nil theList)
			(setq i 0)
			(while (< i (length theList))
				(cond
					(	(eval criteria)
						(setq n i)
						(setq i (length theList))))
				(setq i (1+ i))))
		( 	T
			(setq n nil)))
	n)
			
			
			
;;; MakeVarNotNil - simple function for verifying variables are set and
;;;                 giving them a default value if not
;;; variableName [symbol] - name of variable to check
;;; valueToSetIfEmpty [atom] - default value to use if nothing is set

(defun MakeVarNotNil (variableName valueToSetIfEmpty)
	(if (or (= (eval variableName) nil) (= (eval variableName) ""))		
      (set variableName valueToSetIfEmpty)
		(eval variableName)))
		
		
		
;;; Sort - Sorts the list by the provided comparison function
;;; 					using vl-sort-i to get sorted indexes and assembling a 
;;;   			   sorted list with those indexes.
;;; thelist - [list] name of the list to sort
;;; functionName - [symbol] the function to use 
;;; Based on (mapcar '(lambda (x) (nth x a)) (vl-sort-i a '<))
;;; provided by *Kim, HeeTea on forums.autodesk.com, 1/23/2002 (02/06/16)



(defun Sort (thelist functionName)
   (mapcar '(lambda (index) (nth index thelist))
				(vl-sort-i thelist functionName)))
	
	
	
;;; SortKeys - Sorts the list by the provided comparison function
;;; 					using vl-sort-i to get sorted indexes and assembling a 
;;;   			   sorted list with those indexes.
;;; thelist - [list] name of the list to sort
;;; functionName - [symbol] the function to use 
;;; Based on (mapcar '(lambda (x) (nth x a)) (vl-sort-i a '<))
;;; provided by *Kim, HeeTea on forums.autodesk.com, 1/23/2002 (02/06/16)



(defun SortKeys (theAssoclist functionName)
   (mapcar '(lambda (index) (nth index theAssoclist))
				(vl-sort-i
					(mapcar
						'car theAssoclist)
					functionName)))
	
	
	
;;; SortValues - Sorts the list by the provided comparison function
;;; 					using vl-sort-i to get sorted indexes and assembling a 
;;;   			   sorted list with those indexes.
;;; thelist - [list] name of the list to sort
;;; functionName - [symbol] the function to use 
;;; Based on (mapcar '(lambda (x) (nth x a)) (vl-sort-i a '<))
;;; provided by *Kim, HeeTea on forums.autodesk.com, 1/23/2002 (02/06/16)



(defun SortValues (theAssoclist functionName)
   (mapcar '(lambda (index) (nth index theAssoclist))
				(vl-sort-i
					(mapcar
						'cdr theAssoclist)
					functionName)))

					
					
;;; QtyList - Turns a list with possible multiples into an association list
;;; theAssoclist - [assoc list] name of the list to split up

(defun QtyList (thelist / newQtyList)
   (foreach item thelist
		(setq newQtyList (Assoc++ item newQtyList)))
	newQtyList)
	
	
	
;;; UnQtyList - Turns an assoc list back into a regular list (multiples)
;;; theAssoclist - [assoc list] name of the list to split up

(defun UnQtyList (theQtylist / newList)
   (foreach item theQtylist
		(foreach i (Range 1 (cdr item) 1)
			(setq newList (append newlist (list (car item))))))
	newList)
					
					
					
;; DisplayAssocList - Displays the association list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun JD:DisplayAssocList (assocList)
   (princ "\n=Key===Value=\n")
   (foreach item assocList
      (progn
			(princ " ")
			(princ (car item))
			(princ " . ")
			(princ (cdr item))
			(princ "\n")))
   (princ "===========\n")
	(princ))



;; DisplayQtyList - Displays the association list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun JD:DisplayQtyList (assocList)
   (princ "\n=Desc==Qty=\n")
   (foreach item assocList
      (progn
			(princ " ")
			(princ (car item))
			(princ " . ")
			(princ (cdr item))
			(princ "\n")))
   (princ "===========\n")
	(princ))



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