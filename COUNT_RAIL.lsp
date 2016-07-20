;;;;;;;[  Top Rail Count Subfunstion  ];;;;;;;;;;;
;;                                              ;;
;;  Does cut list count based on measurements   ;;
;;  of center lines. Prompts for stock length   ;;
;;  and multiplier and stores in global         ;;
;;  variables.                                  ;;
;;    (forked from RAIL_COUNT_COM)              ;;
;;    (forked from RAIL_COUNT_SPLIT)            ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 11/17/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  04/06/2016                                  ;;
;;  - Tweaked chopping function to fix bug.     ;;
;;  - Added CountStockLengths to simplify       ;;
;;    what I was typing into the command line.  ;;
;;                                              ;;
;;  02/08/2016                                  ;;
;;  - Tweaked chopping function to fix bug.     ;;
;;                                              ;;
;;  02/04/2016                                  ;;
;;  - Did a lot of simplifying                  ;;
;;  - Changed CountRails to create a list for   ;;
;;    the counts instead of printing them.      ;;
;;  - Created PrintCutList to print the data    ;;
;;    so there's no change in function.         ;;
;;                                              ;;
;;  01/31/2016                                  ;;
;;  - Changed to be a subfunction of            ;;
;;    PartsCount.                               ;;
;;  - Fixed the glitch with the first/stock     ;;
;;    lengths not combining into one line       ;;
;;    correctly.                                ;;
;;                                              ;;
;;  11/18/2015                                  ;;
;;  - Revised algorithms for counting and       ;;
;;    measuring to be more precise.             ;;
;;  - Fixed the glitch with the first/stock     ;;
;;    lengths not combining into one line       ;;
;;    correctly.                                ;;
;;                                              ;;
;;  11/17/2015                                  ;;
;;  - Added layer prompt option.                ;;
;;  - Added layer variable to SSGET.            ;;
;;  - Refined prompts - some actions set        ;;
;;    option to next logical choice.            ;;
;;  - Set defaults for options - 242",          ;;
;;    1 floor, Center layer.                    ;;
;;  - Changed display to show duplicate cuts    ;;
;;    as x#.                                    ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Untangle layer setting from               ;;
;;    MeasureLineSegments.                      ;;
;;  - MVP best practice - cuts to a list to     ;;
;;    pass to a display function. Where else    ;;
;;    do I need to separate things like that?   ;;
;;  - Re-org everything for easy reading &      ;;
;;    logic, and add comments.                  ;;
;;  - Add post spacing prompt to better figure  ;;
;;    out remainder lengths on chops.           ;;
;;  - Add stair function. And manual mode.      ;;
;;  - Fix bug in chopping function that leaves  ;;
;;    longer than stock length pieces.          ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Global variables referenced:                ;;
;;    *stockLength*      *cutList*              ;;
;;    *postSpacing*                             ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Counts toprail (mlines) and infill/bottom rail (plines).
(defun C:cr ( / stockLength roundingFactorToprail
					 roundingFactorInfill layerToCountToprail 
					 fudgeFactorToAddToprail fudgeFactorToAddInfill 
					 layerToCountInfill selSet subset isConfirmed)
	
	(setq stockLength 242)
	(setq roundingFactorToprail 3
			fudgeFactorToAddToprail 9
			layerToCountToprail "1")
	(setq roundingFactorInfill 2
			fudgeFactorToAddInfill 1
			layerToCountInfill "Center")
				
	(setq selSet (ssget (list '(0 . "mline,*polyline")
										(cons 8 (strcat layerToCountToprail "," layerToCountInfill)))))
	
	;(JD:HighlightSelectionSet nil selSet)
	;(JD:HighlightSelectionSet T
	;	(setq subset
	;		(JD:FilterSelectionSet (cons 8 layerToCount) selSet)))
	
	(princ 
		(strcat "\nTop rail stock lengths: " 
			(itoa 
				(CountRails 
					(ChopLongLengths
							(MeasureLineSegments
								roundingFactorToprail 
								fudgeFactorToAddToprail
								(JD:FilterSelectionSet (cons 8 layerToCountToprail) 
									(JD:FilterSelectionSet (cons 0 "mline") selSet)))
							stockLength)
					stockLength))))

	(princ 
		(strcat "\nInfill/bottom rail stock lengths: " 
			(itoa 
				(CountRails 
					(MeasureLineSegments
						roundingFactorInfill
						fudgeFactorToAddInfill
						(JD:FilterSelectionSet (cons 8 layerToCountInfill)  
							(JD:FilterSelectionSet (cons 0 "*polyline") selSet)))
					stockLength))))
	(princ))
	
	
	
(defun RailCountSub (/ floorsMultiplier qtyNeeded timeToReturn input)
   ;; Save system variables
	(JD:SaveVar "osmode" 'systemVariables)
	(JD:SaveVar "dimzin" 'systemVariables)
	(princ "System variables saved.")
   
   ;; Set defaults (only do global variables if they aren't set yet)
   (MakeVarNotNil '*stockLength* 242)
   (MakeVarNotNil '*ctrLineLayer* "A-HRAL-CNTR")
   (MakeVarNotNil '*choice* "Add")
	(setq floorsMultiplier 1)
	(princ "variable setup")

   (while (/= timeToReturn "True")
   	(initget "Stock Multiplier Layer Add Count RESET Done")
		(setq *choice*
			(cond
				((getkword
					(strcat "\nChoose cut list option [Stock/Multiplier/"
							  "Layer/Add/Count/RESET/Done] <" *choice* ">:" )))
			(*choice*)))
		
		(cond
			(	(= *choice* "RESET")
				(ResetCutList)
				(setq *choice* "Add"))

			(	(= *choice* "Done")
				(setq timeToReturn "True")
				(setq *choice* "RESET"))

			( 	(= *choice* "Stock")
				(setq *choice* "Add")
				(initget (+ 2 4))				; prevent 0 or negative values
				(setq *stockLength*
					(cond
						((getint (strcat "\nStock length (in inches) <" 
											  (itoa *stockLength*)
											  ">:")))
						(*stockLength*))) )

			( 	(= *choice* "Multiplier")
				(setq *choice* "Add")
				(initget (+ 2 4))				; prevent 0 or negative values
				(setq floorsMultiplier
					(cond
						((getint
							(strcat "\nNumber of identical floors (multiplier) <" 
									  (itoa floorsMultiplier)
									  ">:")))
						(floorsMultiplier))))

			( 	(= *choice* "Layer")
				(setq *choice* "Add")
				(setq input (getstring (strcat "\nCenter line layer <"
														 *ctrLineLayer*
														 ">:")))
				(if (/= input "")
					(setq *ctrLineLayer* input)))
			
			( 	(= *choice* "Add")
				(setq *cutList*
					(OrderList (append *cutList* (MeasureCenterLines))))
				(princ "\nCutlist: ")
				(DisplayCount *cutList*))
		
			( 	(= *choice* "Count")
				(setq *choice* "Done")
				(setq *cutList* (OrderList 
					(ChopLongLengths *cutList* *stockLength*)))
				(princ "\nCutlist: ")
				(DisplayCount *cutList*)
		
				;; Counting for parts list handled in this function
				(setvar "dimzin" 8)
				(setq qtyNeeded (CountRails *cutList* *stockLength*))
								
				;; Display handled in these functions
				(PrintCutList *fullCutList*)
				(princ "Stock lengths: ")
				(DisplayCount qtyNeeded))))

   ;; Restore changed system variables
   (JD:ResetVar "osmode" 'systemVariables)
   (JD:ResetVar "dimzin" 'systemVariables)
   
	qtyNeeded)




;;; Error handling function - prints error message nicely and resets system variables

(defun errorHandler (msg)
   (princ "\nOops! ")
   (princ msg)

   ;; Reset system variables
	(JD:ResetVar "osmode" 'systemVariables)
   (JD:ResetVar "dimzin" 'systemVariables)

   (princ "\nSystem variables reset")
   (princ "\n")
	
	(setq *error* nil)
   (princ))			; Hide last return value (clean exit)




; MeasureCenterLines
; Created: 10/29/2015 by J.D. Sandifer
; Purpose: Measures lengths of lines on *ctrLineLayer* layer and returns a 
; list of their lengths.
; Input: User selects area contaning the lines.
; Returns: A list of lengths.
; Revision History:
; 10/30/15 - Measuring works! (for one line)

(defun MeasureCenterLines (/ buffer selSet centerline
			     centerLineInfo centerLinelength cutList)

   (setq buffer 6)
   (setq selSet (ssget (list (cons 8 *ctrLineLayer*) '(0 . "line"))))
      ; have the user select an area that includes the lines to measure
      ; and filter in only lines on "A-HRAL-CNTR" layer (not case-sensitive)
   (setq index 0)
   (while (< index (sslength selSet))
      ; loop through the selection set
      (setq centerLine (ssname selSet index))
         ; get the next center line from the list
      (setq centerLineInfo (entget centerLine))
         ; get the entity info list for the line
      (setq centerLinelength
         (distance
            (cdr (assoc 10 centerLineInfo))
            (cdr (assoc 11 centerLineInfo))
               ; access the info at keys 10 & 11 (start and end points),
   	       ; chop off the key so its just the points,
         )
            ; measure the distance between the two points,
	 
      )
         ; and assign it to centerLineLength
      (setq centerLineLength (+ buffer centerLineLength))
         ; add buffer to centerLineLength
      (setq centerLineLength (RoundUpTo 2 centerLineLength))
         ; round up to the nearest 3"
      (setq cutList (Assoc+Qty centerLinelength cutList floorsMultiplier))
	 ; and add it to cutList
      (setq index (1+ index))
         ; increment counter (very important)
   )
      ; end of while loop

   (OrderList cutlist)
   
)



   
;; ChopLongLengths - Cuts all lengths longer than stock length and adds back parts. 
;; cutList - [association list] The cut list.

(defun ChopLongLengths (cutList stockLength / currentCutIndex currentCutLength
			currentCutQuantity multiplier remainder finalCutList splices
			MIN-CHOP-LENGTH ROUND-UP-LENGTH)

	(setq MIN-CHOP-LENGTH 87
			ROUND-UP-LENGTH 24)
			
			
   (princ "\nStock length: ")
   (princ stockLength)
   
   (setq currentCutIndex 0)
	(setq finalCutList nil)
	(setq splices 0)
	
   (while (< currentCutIndex (length cutList))

      (setq currentCutLength (car (nth currentCutIndex cutList)))
      (setq currentCutQuantity (cdr (nth currentCutIndex cutList)))

      (if (> currentCutLength stockLength)
			(progn
				(princ "\n= ")
				(princ currentCutQuantity)
				(princ " x ")
				(princ currentCutLength)
				(princ ", ")
				(setq multiplier (fix (/ currentCutLength stockLength)))
				(princ multiplier)
				(setq splices (+ splices (* multiplier currentCutQuantity)))
					; how many stock lengths do we need (per long length)?
				(princ ", ")
				(setq remainder
					(RoundUpTo 2 (rem currentCutLength stockLength)))
				(princ remainder)
					; what's left over after the chop?
				(setq cutList
					(vl-remove (assoc currentCutLength cutList) cutList))
					; remove the long piece
				(setq finalCutList
					(Assoc+Qty stockLength finalCutList
								  (* multiplier currentCutQuantity)))
					; add the stock lengths
				(cond
					; if it's too small, make it long enough (66")
					(	(<= remainder MIN-CHOP-LENGTH)
						(setq finalCutList (Assoc+Qty MIN-CHOP-LENGTH finalCutList 
													currentCutQuantity)))
					; make sure we don't add to greater than stock length
					(	(and
							(> remainder MIN-CHOP-LENGTH)
							(<= remainder (- stocklength ROUND-UP-LENGTH)))
						(setq finalCutList (Assoc+Qty (+ remainder ROUND-UP-LENGTH)
												 finalCutList
												 currentCutQuantity)))
					; if the remainder is long enough, add a stock length, too
					(	(> remainder (- stocklength ROUND-UP-LENGTH))
						(setq finalCutList (Assoc+Qty stockLength finalCutList
								  currentCutQuantity)))))
			(progn
				(setq finalCutList 
					(Assoc+Qty currentCutLength finalCutList currentCutQuantity))
				(setq currentCutIndex (1+ currentCutIndex))))
				
      (princ) )

	(princ "\nSplices needed: ")
	(princ splices)
	(princ " (")
	(princ (* 2 splices))
	(princ " splice plates)")
	(princ "\n")
		
   (OrderList finalCutList))




;;; CountRails
;;; Determines stock lengths needed to fulfil quantities of rail in cutList.
;;; cutList - [association list] (Length . qtyNeeded) list of railing cuts 
;;; (must be shorter than stock length).
;;; Returns an association list of stock lengths starting with full length
;;; (like cutList).

(defun CountRails (cutList stockLength / 
						 stockLengthLeft currentCutIndex stockLengthsNeeded currentCutKey bladeWidth); finalCuts finalCutList)

   ;Counters
   (setq stockLengthLeft 0.000)
   (setq currentCutIndex 0)
   (setq stockLengthsNeeded 0)	; will become association list (currently integer)
   (setq bladeWidth 0.125)
	
;;;;prep for full cut list counting
   (setq finalCuts nil)
	(JD:ClearHash 'finalCutList)
	(JD:ClearHash '*fullCutList*)

   (while (> (length cutList) 0)
      
      (setq currentCutLength (car (nth currentCutIndex cutList)))
      
      (cond
			;; Cut length is too long
			((> currentCutLength stockLength)
				(*error*
				(strcat "Problem: Current cut ("
				(itoa currentCutLength)
				"\") is longer than stock length ("
				(itoa stockLength) "\")."))
				(setq cutList nil))
	      
			;;no more length
			((<= stockLengthLeft 0)
				(setq stockLengthLeft stockLength)
				(setq stockLengthsNeeded (1+ stockLengthsNeeded)))
	 
			;;there is more length, but cut won't fit
			((and (> stockLengthLeft 0)
					(> currentCutLength stockLengthLeft))
				(setq currentCutIndex (1+ currentCutIndex)))
	    
			;;there is more length and cut will fit
			((and (> stockLengthLeft 0) (<= currentCutLength stockLengthLeft))
            ;subtract cut length from stock length
				(setq stockLengthLeft (- stockLengthLeft currentCutLength bladeWidth))
            ;;add this cut to the full cut list			
				(setq finalCuts (append finalCuts (list currentCutLength)))
				(JD:PutHash "cuts" (list finalCuts) 'finalCutList)

            ;decrement cut length quantity (or remove from list) - function
				(setq cutList (assoc-- currentCutLength cutList))))

      ;;end of cut list
      (cond
			((or (>= currentCutIndex (length cutList))
				  (<= stockLengthLeft 0)
				  (<= (length cutList) 0))
				(setq currentCutIndex 0)
										
				;;add scrap to the full cut list and prep for the next loop
				(if (= stockLengthLeft -0.125)
					(JD:PutHash "scrap" 0 'finalCutList)
					(JD:PutHash "scrap" stockLengthLeft 'finalCutList))
				(setq *fullCutList* (append *fullCutList* (list finalCutList)))
				(setq finalCuts nil)
				(JD:ClearHash 'finalCutList)

				(setq stockLengthLeft 0)))) ;end of while loop	
				
   stockLengthsNeeded)
		
			
			
;;; PrintCutList - Copyright 2016 J.D. Sandifer
;;; Prints cut list to the command line in a nice format.
;;; cutList [full cut list] - the full cut list

(defun PrintCutList ( cutList / 
							 repeats lastItem currentItem cut)
	(princ "\n")
   (princ "\nStock length cuts")
   (princ "\n--------------------")
	
	(setq repeats 1)
	(setq lastItem nil)
	
	(if (/= cutList nil)
		(progn
			(setq cutList (append cutList '("end")))
			(foreach currentItem cutList
				(if (equal currentItem lastItem)
					(setq repeats (1+ repeats))
					(progn
						(cond ((/= lastItem nil)
							(princ "\n")
							(foreach cut (car (JD:GetHash "cuts" 'lastItem))
								(princ cut)
								(princ " / "))
							(princ "(")
							(princ (JD:GetHash "scrap" 'lastItem))
							(princ ")")
							(if (> repeats 1)
								(princ (strcat " x " (itoa repeats))))
							(setq repeats 1)))))
				(setq lastItem currentItem)))
		(princ "\n   (nothing)"))
		
	(princ "\n--------------------")
	(princ "\n")
	(princ))




;;----------------------------------------------------------------------;;


(princ
    (strcat
        "\n:: COUNT_RAIL.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;