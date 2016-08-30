;;;;;;;[  Functional Programming  ];;;;;;;;;;;;;;;
;;                                              ;;
;;  Functions for functional programming.       ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 07/26/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  08/29/2016                                  ;;
;;  - Added Range and unit test for it.         ;;
;;                                              ;;
;;  07/26/2016                                  ;;
;;  - Started with Filter.                      ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Make functions more efficient?            ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Runs all unit tests for this file
; Output: List of test results and summary

(defun C:TestFuncProg ( / testList)
	;; Setup for tests
	(princ "\n")
	(princ "Testing FUNC_PROG\n")
	(princ "--------------------\n")
	
	;; Actual tests
	;(princ "Map\n")
		
	;(princ "\n")
	(princ "Range\n")
	(Assert 'Range '(1 1 1) '(1))
	(Assert 'Range '(1 2 1) '(1 2))
	(Assert 'Range '(1 5 1) '(1 2 3 4 5))
	(Assert 'Range '(1 10 2) '(1 3 5 7 9))
	(Assert 'Range '(2 10 2) '(2 4 6 8 10))
	(Assert 'Range '(5 1 -1) '(5 4 3 2 1))
	(Assert 'Range '(10 2 -2) '(10 8 6 4 2))
		
	;; Displaying the results of the tests
	(JD:PrintTestResults (JD:CountBooleans testList)))
	
	
	
;|===={ Filter }============================|;
;| Returns a sublist of the supplied list   |;
;| including only values that return true   |;
;| when input to the supplied function.     |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/26/2016 |;
;|==========================================|;

(defun Filter (comparisonFunction listToFilter / listToReturn)
	(foreach item listToFilter
		(if (apply comparisonFunction (list item))
			(setq listToReturn
				(append listToReturn (list item)))))
	listToReturn)

	
	
;|===={ Map }===============================|;
;| Simple wrapper for the "mapcar" function |;
;| to allow it to be accessed by the more   |;
;| common functional programming name.      |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 08/01/2016 |;
;|==========================================|;

(defun Map (theFunction theList / )
	(mapcar theFunction theList))
	
	
	
;|===={ Range }=============================|;
;| Creates a list of sequential numbers     |;
;| based on an initial value, increment,    |;
;| and a final value to reach or not        |;
;| exceed at the least. Useful for turning  |;
;| foreach into a typical for loop.         |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 08/29/2016 |;
;|==========================================|;

(defun Range (value finalValue increment / rangeList comparator)
	(if (>= finalValue value)
		(setq comparator '<=)
		(setq comparator '>=))
	(while ((eval comparator) value finalValue)
		(setq rangeList (append rangeList (list value)))
		(setq value (+ increment value)))
	rangeList)	
	
	
	
;|===={ Reduce }============================|;
;| Applies a supplied function to the       |;
;| first two values of the supplied list.   |;
;| The result is then used as the first     |;
;| value for the function and the next      |;
;| value from the list is the second. This  |;
;| continues until the list is *reduced*    |;
;| to one value which is returned. The      |;
;| given function must expect two inputs.   |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/26/2016 |;
;|==========================================|;

(defun Reduce (reductionFunction listToReduce / valueToReturn)
	(setq valueToReturn (car listToReduce))
	(setq listToReduce (cdr listToReduce))
	(foreach item listToReduce
		(setq valueToReturn
			(apply reductionFunction (list valueToReturn item))))
	valueToReturn)	
				
		

;;----------------------------------------------------------------------;;


(princ
    (strcat
        "\n:: FUNC_PROG.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;