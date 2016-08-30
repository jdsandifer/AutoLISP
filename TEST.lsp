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
;;  08/29/2016                                  ;;
;;  - Added JD:ReplaceAllSubst to clean up      ;;
;;    display of argument lists.                ;;
;;  - Tweaked Assert and Verify to use it.      ;;
;;  - Added tests of the tests...still need     ;;
;;    real tests of Assert & Verify. Maybe not  ;;
;;    immediately print results? Separate       ;;
;;    function?                                 ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Tests the testing functions.
; Output: T if all unit tests passed, else nil

(defun C:Test ( / testList)
	;; Setup for tests
	(princ "\n")
	(princ "Testing TEST\n")
	(princ "---------------\n")
	
	;; Actual tests
	(princ "Assert\n")
	(Assert '+ '(1 2 3) 6)
	(princ "\nVerify\n")
	
	(setq varToTest nil)
	(Verify 'set '('varToTest 6) '(= varToTest 6))
	(setq varToTest nil)
	
	(princ "\nJD:ReplaceAllSubst\n")
	(Assert 'JD:ReplaceAllSubst '("y" "a" "Hello") "Hello")
	(Assert 'JD:ReplaceAllSubst '("o" "a" "Hella") "Hello")
	(Assert 'JD:ReplaceAllSubst '("o" "a" "Hella hella hella") "Hello hello hello")
	(Assert 'JD:ReplaceAllSubst '("o" "a" "a hella") "o hello")
	(Assert 'JD:ReplaceAllSubst '("o" "a" "A hella") "A hello")
	(Assert 'JD:ReplaceAllSubst '("o" "a" "hallaw") "hollow")
	
	(princ "\nJD:CountBooleans\n")
	(Assert 'JD:CountBooleans '('()) '(("T" . 0)("F" . 0)))
	(Assert 'JD:CountBooleans '('(T T T)) '(("T" . 3)("F" . 0)))
	(Assert 'JD:CountBooleans '('(F F F)) '(("T" . 0)("F" . 3)))
	(Assert 'JD:CountBooleans '('(F T F)) '(("T" . 1)("F" . 2)))
	
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
		(JD:ReplaceAllSubst 
			"'"
			"(QUOTE " 
			(vl-prin1-to-string argumentList)))
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
	(princ 
		(JD:ReplaceAllSubst 
			"'"
			"(QUOTE " 
			(vl-prin1-to-string argumentList)))
	(princ ") functioned ")
	(if passed
		(princ "as expected")
		(princ "erroneously"))
	(princ "\n")
	(setq testList (append testList (list passed)))
	
	passed)
	
	
	
; Changes all "pattern"s to "newSubst". Like vl-string-subst, but replaces
;    ALL instances of pattern instead of just the first one.
; Argument: String to alter.
; Return: Altered string.

(defun JD:ReplaceAllSubst (newSubst pattern string / pattern)
	(while (vl-string-search pattern string)
		(setq
			string
			(vl-string-subst newSubst pattern string)))
	string)
	
	
	
; Counts boolean values
; Input: simple list of boolean values - (T T T T T T T T T T)
; Output: none
; Return: assoc. list of the qty of T's and F's - (("T" . 10) ("F" . 0))


(defun JD:CountBooleans ( booleanList / countList)
	(setq countList '(("T" . 0)("F" . 0)))
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
	(setq trues (cdr (Assoc "T" testResults)))
	(setq falses (cdr (Assoc "F" testResults)))
	
	(if (= trues nil)
		(setq trues 0))
	(if (= falses nil)
		(setq falses 0))
	
	(princ "\nResults:")
	(if (= falses 0)
		(princ " ALL PASS"))
	(princ "\n-----------------")
	
	
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