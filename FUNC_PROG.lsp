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
;;  07/26/2016                                  ;;
;;  - Started with Filter.                      ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Make functions more efficient?            ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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