;;;;;;;[   Unit Tests   ];;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Testing suite - including actual tests and  ;;
;;  helper functions. Unit testing only.        ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 02/23/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  02/23/2016                                  ;;
;;  - Started.                                  ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Runs all unit tests
; Output: T if all unit tests passed, else nil

(defun C:UnitTests ( / testList testResults)

	;(ReloadFile "UNIT_TESTS")
	(Assert 'isInserted "not-a-block" nil)
	(Assert 'isInserted "not-a-block" T)
	(setq testResults (CountBooleans testList))
	(PrintTestResults testResults))
		
	
	
; Checks if function operates as expected and add result to testList
; Input: function (symbol), argument for the function, expected result
; Output: result of test
; Return: none


(defun Assert ( functionName argumentForFunction expectedResult / passed)
	(cond
		((= (functionName argumentForFunction) expectedResult)
			(princ "passed...(")
			(setq passed T))
		(T
			(princ "failed...(")
			(setq passed nil)))
	(princ (quote functionName))
	(princ argumentForFunction)
	(princ ")")
	passed)
		
	
	
; Counts boolean values
; Input: simple list of boolean values - (T T T T T T T T T T)
; Output: none
; Return: assoc. list of the qty of T's and F's - (("T" . 10) ("F" . 0))


(defun CountBooleans ( booleanList / countList)
	(foreach listItem booleanList
		(if (= listItem T)
			(setq countList (Assoc++ "T" countList))
			(setq countList (Assoc++ "F" countList))))
	(OrderList countList))
	
	
	
; Prints test results
; Input: assoc list of T's and F's - (("T" . 10) ("F" . 0))
; Output: simple description of the results -
;			Results:
;			----------------
;			 10 tests passed
;			  0 tests failed
; Note: there are always four characters before each "tests"

(defun PrintTestResults ( testResults / trues falses)
	(princ "\nResults:")
	(princ "\n----------------")
	
	(setq trues (cdr (Assoc "T" testResults)))
	(setq falses (cdr (Assoc "F" testResults)))
	
	; add code to count digits in numbers and add correct space before
	; and use test vs. tests correctly?
	
	(princ "\n  ")(princ trues)(princ " tests passed")
	(princ "\n  ")(princ falses)(princ " tests failed")
	(princ))
	



; Reloads the input file
; Input: name of LISP file to load
; Output: results of load
; Return: T or nil (success or failure)

;(defun reloadFile ( fileToLoad /)
	;reload code)
	
	

; Silent load
(princ)