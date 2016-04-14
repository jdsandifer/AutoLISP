;;;;;;;[   Test   ];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Simple file for testing stuff.              ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 02/04/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  02/04/2016                                  ;;
;;  - Testing variable scope - local variables  ;;
;;    as hash tables for my hash functions.     ;;
;;    Works great!                              ;;
;;  - Local variables w/out passing. Works!     ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:Test (/ hashTable)

	(JD:PutHash 1 "Bananas" 'hashTable)
	(JD:PutHash 3 "Monkeys" 'hashTable)
	(JD:PutHash 2 "Oranges" 'hashTable)

	(showMeTheHash)
	
	(princ (JD:GetHash 2 'hashTable))
	(princ "\n")
	
	(JD:ClearHash 'hashTable)	
	
	(JD:PrintHash 'hashTable)	
	
	(princ (JD:GetHash 2 'hashTable))
	
   (princ))
	
(defun showMeTheHash ( / )
	(JD:PutHash 4 "Rocks" 'hashTable)
	(JD:PrintHash 'hashTable))



;;; Simple error message handler that resets defaults.

;(defun *errorHandler* (msg)
;   (princ msg)
;   (setq *error* oldError)
;   (princ))



; Silent load
(princ)