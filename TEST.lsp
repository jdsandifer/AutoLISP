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
;;  06/28/2016                                  ;;
;;  - Added Verify for functions that don't     ;;
;;    return a value.                           ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Tests the testing functions.
; Output: T if all unit tests passed, else nil

(defun C:Test ( / testList testResults varList)

	;; Setup for tests
	(princ "\n")
	(princ "Testing TEST\n")
	(princ "---------------\n")
	
	;; Actual tests
	(princ "Assert\n")
	(Assert '+ '(1 2 3) 6)
	;(princ "Verify\n")
	;(Verify 'JD:ResetVar '("osmode" 'varList) '(= (getvar "osmode") 180))
			
	;; Displaying the results of the tests
	(JD:PrintTestResults (JD:CountBooleans testList)))
		
	
	
; Checks if function operates as expected and adds result to testList
; Input: function (symbol), argument (list) for function, expected return value
; Output: result of test
; Return: T or nil


(defun Assert ( functionName argumentList expectedReturn / 
					 actualReturn passed)
	(if (not (= (type argumentList) 'LIST))
		(setq argumentList (list argumentList)))
	(cond
		((equal (setq actualReturn (eval (cons functionName argumentList)))
			  expectedReturn)
			(princ "passed...(")
			(setq passed T)
			(setq actualReturn nil))
		(T
			(princ "failed...(")
			(setq passed nil)
			(setq actualReturn 
				(strcat (vl-princ-to-string actualReturn) " instead of "))))
	
	;; continue printing result...
	(princ (strcase (vl-symbol-name functionName) T))
	(princ " ")
	(princ 
		(vl-string-subst 
			"'"
			"(QUOTE " 
			(vl-string-subst 
				"'"
				"(QUOTE " 
				(vl-prin1-to-string argumentList))))
	(princ ") returned ")
	(if actualReturn (princ actualReturn))
	(princ expectedReturn)
	(princ "\n")
	(setq testList (append testList (list passed)))
	
	passed)
	
	
	
; Checks if function operates as expected and adds result to testList
; (for functions that don't return a value)
; Input: function (symbol), argument (list) for the function, expression
; that should return true if the function acts as expected
; Output: result of test
; Return: T or nil


(defun Verify ( functionName argumentList testForResult / 
					 passed)
					 
	(eval (cons (eval functionName) argumentList))	;run the test
	(cond
		((eval testForResult)
			(princ "passed...(")
			(setq passed T))
		(T
			(princ "failed...(")
			(setq passed nil)))
	
	;; continue printing result...
	(princ (strcase (vl-symbol-name functionName) T))
	(princ " ")
	(prin1 argumentList)
	(princ ") functioned ")
	(if passed
		(princ "as expected")
		(princ "erroneously"))
	(princ "\n")
	(setq testList (append testList (list passed)))
	
	passed)
	
	
	
; Counts boolean values
; Input: simple list of boolean values - (T T T T T T T T T T)
; Output: none
; Return: assoc. list of the qty of T's and F's - (("T" . 10) ("F" . 0))


(defun JD:CountBooleans ( booleanList / countList)
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

(defun JD:PrintTestResults ( testResults / trues falses)
	(princ "\nResults:")
	(princ "\n----------------")
	
	(setq trues (cdr (Assoc "T" testResults)))
	(setq falses (cdr (Assoc "F" testResults)))
	
	(if (= trues nil)
		(setq trues 0))
	(if (= falses nil)
		(setq falses 0))
	
	
	; add code to count digits in numbers and add correct space before.
	; also, choose between "test" and "tests" correctly?
	
	(princ "\n  ")(princ trues)(princ " tests passed")
	(princ "\n  ")(princ falses)(princ " tests failed")
	(princ))
	



; Reloads the input file
; Input: name of LISP file to load
; Output: results of load
; Return: T or nil (success or failure)

;(defun reloadFile ( fileToLoad /)
	;reload code)
	
	
	
;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
   (strcat
      "\n:: TEST.lsp loaded. | \\U+00A9 J.D. Sandifer "
      (menucmd "m=$(edtime,0,yyyy)")
      " ::"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;