;;;;;;;[  Selection Set Functions  ];;;;;;;;;;;;;;
;;                                              ;;
;;  All functions needed to work with           ;;
;;  selection sets functionally.                ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 04/15/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  04/15/2016                                  ;;
;;  - Finished basic HighlightSelectionSet      ;;
;;    with flag to also choose unhighlighting.  ;;
;;  - Moved FilterSelectionSet and              ;;
;;    PrintSelectionSet here.                   ;;
;;  - Did preliminary testing.                  ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Selection set to entity list function     ;;
;;    better functional programming (mapcar).   ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	
	
;; FilterSelectionSet - J.D. Sandifer
;; Returns a subset of a selection set based on the provided filter.
;; filter [dot pair] - ex. (0 . "*LINE,INSERT") for blocks and all "lines."

(defun JD:FilterSelectionSet (filter selectionSet / 
									subset filterType filterValue index)
   (setq subset (ssadd)
			filterType (car filter)
			filterValue (cdr filter))
			
	(setq index (sslength selectionSet))
	(while (> index 0)
		(setq entity (ssname selectionSet (1- index)))
		(if (= (type filterValue) 'STR)
			(if (wcmatch (strcase (cdr (assoc filterType (entget entity))) T)
			   			 (strcase filterValue T))
				(setq subset (ssadd entity subset)))
			(if (= (cdr (assoc filterType (entget entity))) filterValue)
				(setq subset (ssadd entity subset))))
		(setq index (1- index)))
   subset)
	
	
	
;; HighlightSelectionSet - J.D. Sandifer
;; Highlights or unhighlights all objects in a selection set.
;; doHighlight [bool] - T for highlight, nil for unhighlight
;; selectionSet [sel set]

(defun JD:HighlightSelectionSet (doHighlight selectionSet / 
											highlightMode index)
   (cond 
		(doHighlight
			(setq highlightMode 3))
		((not doHighlight)
			(setq highlightMode 4)))
	
	(setq index (sslength selectionSet))
	(while (> index 0)
		(setq entity (ssname selectionSet (1- index)))
		(redraw entity highlightMode)
		(setq index (1- index)))
   (princ))

	
	
;; PrintSelectionSet - J.D. Sandifer
;; Prints a selection set, one entity per line. (Mainly for testing...)
;; selectionSet [sel set]

(defun JD:PrintSelectionSet (selectionSet / index)
   (setq 
		index (sslength selectionSet))
	(princ "\nSelection set contains:")
	(while 
		(> index 0)
		(princ "\n")
		(princ 
			(entget 
				(ssname 
					selectionSet 
					(1- index))))
		(setq 
			index (1- index)))
	(princ "\n")
	(princ))



;;----------------------------------------------------------------------;;


(princ
    (strcat
        "\n:: SELECTION_SET.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::\n"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;