;;;;;;;[  Functions for Hash Table  ];;;;;;;;;;;;;
;;                                              ;;
;;  All functions needed to create and edit     ;;
;;  a basic hash table. Includes re-assigning   ;;
;;  the list variable after all functions!      ;;
;;  (Association list wrapper functions.)       ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 01/20/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  01/20/2016                                  ;;
;;  - Built JD:CreateDatabase.                  ;;
;;  - Built JD:AddItemToDatabase.               ;;
;;  - Built JD:GetQuantityOfItemInDatabase.     ;;
;;                                              ;;
;;  01/26/2016                                  ;;
;;  - Realized I was basically creating         ;;
;;    wrapper functions for a hash table and    ;;
;;    decided to generalize the nomenclature.   ;;
;;                                              ;;
;;  02/01/2016                                  ;;
;;  - Simplified the nomenclature.              ;;
;;  - Added JD:RemoveHash.                      ;;
;;  - Fixed PrintHashTable so it could handle   ;;
;;    any type as a value.                      ;; 
;;                                              ;;
;;  02/04/2016                                  ;;
;;  - Added SortHash.                           ;;
;;  - PutHash replaces existing values now.     ;;
;;  - PrintHash shows the hash name instead of  ;;
;;    "Hash Table" now.                         ;;
;;  - Added PullHash. Tested it too.            ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Make PrintHash account for different      ;;
;;    lengths in keys, values, and hash name.   ;;
;;  - Add a required entry for the (nice) name  ;;
;;    of the hash table?                        ;;
;;  - PrintHash should list "(empty)" or        ;;
;;    "(nothing)" if the hash is empty.         ;;
;;  - If odd tests are used more, convert       ;;
;;    PrintHash odd test into Odd().            ;; 
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                  
;;; JD:ClearHash - Copyright 2016 J.D. Sandifer
;;; Erases an old hash table if present so data is always fresh.
;;; hashName [symbol] - variable name of the hash table 

(defun JD:ClearHash ( hashName / )
	(set hashName nil)
	(princ))

	
	
;;; JD:PutHash - Copyright 2016 J.D. Sandifer
;;; Adds an item to a hash table (at the end of the list) or
;;; replaces a current item with the same hash key
;;; newKey [any type] - key for new item to be added
;;; newValue [any type] - value of new item to be added
;;; hashName [symbol] - variable name of the hash table

(defun JD:PutHash ( newKey newValue hashName / item )	
	(if (not (setq item (assoc newKey (eval hashName ))))
		(set hashName (append (eval hashName)
											(list (cons newKey newValue))))
		(set hashName
			   (subst (cons newKey newValue) item (eval hashName))))
	(princ))


	
;;; JD:GetHash - Copyright 2016 J.D. Sandifer
;;; Looks for an item in a hash table and returns it's value or nil
;;; key - [string] - key of which to get the value
;;; hashName [symbol] - variable name of the hash table

(defun JD:GetHash ( key hashName / )	
	(cdr (assoc key (eval hashName))))
	

	
;;; JD:PullHash - Copyright 2016 J.D. Sandifer
;;; Looks for an item in a hash table and returns it's value or nil
;;; while removing it from the hash table.
;;; key - [string] - key of which to get the value
;;; hashName [symbol] - variable name of the hash table

(defun JD:PullHash ( key hashName / item )
	(setq item (assoc key (eval hashName)))
	(set hashName (vl-remove item (eval hashName)))
	(cdr item))
	
	
	
;;; SortHash - Sorts the hash by ascending value (alphabetical)
;;;            Works on any list of lists.
;;; hashName - [symbol] name of the hash to sort 

(defun JD:SortHash (hashName)
   (set hashName (vl-sort (eval hashName)
		(function (lambda (e1 e2) (< (car e1) (car e2) )))))
	(princ))

	
	
;;; SortHashBy - Sorts the hash by the provided comparison function.
;;;              Works on any list of lists.
;;; functionName - [symbol] the function to use 
;;; hashName - [symbol] name of the hash to sort

(defun JD:SortHashBy (functionName hashName)
   (set hashName (vl-sort (eval hashName)
		'(lambda (e1 e2) ((eval functionName) (car e1) (car e2) ))))
	(princ))	
	
			
			
;;; JD:PrintHash - Copyright 2016 J.D. Sandifer
;;; Prints hash table to the command line in a nice format.
;;; hashName [symbol] - variable name of the hash table

(defun JD:PrintHash ( hashName / currentItem)
	(JD:SortHash hashName)
	(princ "\n")
	(PrintLine (strcat "Contents of " (vl-princ-to-string hashName)))
	(PrintLine "-------------------------")
	(foreach currentItem (eval hashName)
		(princ (car currentItem))
		(printdots 5)
		; Need to do something here to print the right number of dots...
		(PrintLine (cdr currentItem))		)
	(PrintLine "-------------------------")
	(princ "\n")
	(princ))
	
		
		
;;; PrintLine - Copyright 2016 J.D. Sandifer
;;; Prints something to the command line and adds a newline at the end
;;; message [string] - stuff to print

(defun PrintLine ( message / )	
	(princ message)
	(princ "\n")
   (princ))			; Clean exit (hide last return value)
	
		
		
;;; PrintDots - Copyright 2016 J.D. Sandifer
;;; Prints a certain number of alternating spaces and 
;;;    periods to the command line
;;; numberOfDots [integer] - how much space to take up with the dots

(defun PrintDots ( numberOfDots / )
	(if (= 1 (rem numberOfDots 2))
		(setq isOdd 1)
		(setq isOdd nil))
	(repeat numberOfDots
		(cond
			((= isOdd 1)
				(princ " ")
				(setq isOdd nil))
			((not isOdd)
				(princ ".")
				(setq isOdd 1))))
	(princ))			; Clean exit (hide last return value)
	
		
				
;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
   (strcat
      "\n:: HASH.lsp loaded. | \\U+00A9 J.D. Sandifer "
      (menucmd "m=$(edtime,0,yyyy)")
      " ::"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;