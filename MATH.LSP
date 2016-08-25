;;;;;;;[  Helper Functions - Math  ];;;;;;;;;;;;;;
;;                                              ;;
;;  Shared helper math functions.               ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 01/19/2016 (3/16/2016 this file)   ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  03/16/2016                                  ;;
;;  - Moved math functions to this file.        ;;
;;    No more list and general functions...     ;;
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



;;; RoundUpByDbl
;;; Rounds the value up to the next multiple of "multiple" and returns it as an integer.

(defun RoundUpByDbl (multiple value)
   (if (= (rem value multiple) 0)
      ; Then
      (* value 1.0)
      ; Else
      (* (+ (- value (rem value multiple)) multiple) 1.0)))
		
		
		
;;; Rounding up helper function (legacy support - uses new RoundUpTo function)

(defun RoundUpInt (value)
   (RoundUpBy 1 value) )



;;; RoundUpTo
;;; Rounds the value up to the next multiple of "multiple" and returns it as an integer.

(defun RoundUpTo (multiple value)
   (RoundUpBy multiple value))


		
(princ)		; Clean load