;;;;;;;[  Plan Drawing by Goal  ];;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Draws centerline and top rail, places       ;;
;;  intermediate posts, and dimensions all.     ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 11/16/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  11/16/2015                                  ;;
;;  - Forked from PLANDRAW for commercial,      ;;
;;    spacing-goal-oriented functionality.      ;;
;;  - Added overall length dimension drawing.   ;;
;;  - Simplified dimensions on single space     ;;
;;    runs.                                     ;;
;;  - Revised spacing calc to aim for an ideal  ;;
;;    spacing (not less than a max).            ;;
;;                                              ;;
;;  11/18/2015                                  ;;
;;  - Cleaned up layout and added title block.  ;;
;;  - Fixed undo during error exit.             ;;
;;  - Made custom error function instead of     ;;
;;    overriding the *error* function.          ;;
;;  - Changed file name to PLANDRAW_GOAL        ;;
;;                                              ;;
;;  11/25/2015                                  ;;
;;  - Created a function to accurately          ;;
;;    calculate  closest spacing to a goal      ;;
;;    length. It was surpriringly complex.      ;;
;;    And surprising that it was necerrary.     ;;
;;                                              ;;
;;  01/19/2016                                  ;;
;;  - Moved helper functions out.               ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Add top rail counting? (And revise        ;;
;;    RAIL_COUNT to be primarily an editor?)    ;;
;;  - Add choice of post spacing with default.  ;;
;;    Ditto rail width.                         ;;
;;  - Revise copy on prompts.                   ;;
;;  - Create list of blocks, layers, etc. on    ;;
;;    which this app relies. (With a check      ;;
;;    for them?)                                ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun C:plandrawgoal (/ oldAttributesRequiredSwitch oldSnapMode oldBlipMode
		     oldLayer oldCmdEcho
		     ;|intPostBlock ctrLineLayer postLayer dimLayer dimOffset|;)
   	             ; Define function (inc. local variables)
   (command "._UNDO" "_Begin")		; Start UNDO group
   
   (setq oldCmdEcho (getvar "cmdecho"))
   (setvar "cmdecho" 0)			; Turn off command line output

   (setq oldError *error*		
	 *error* errorHandler)		; Save old error handler and transfer calls to the custom one
   					
   ; Save system variables that will be changed
   (setq oldAttributesRequiredSwitch (getvar "attreq"))
   (setq oldSnapMode (getvar "osmode"))
   (setq oldBlipMode (getvar "blipmode"))
   (setq oldLayer (getvar "clayer"))

   ; Change system variables
   (setvar "attreq" 0)
   (setvar "blipmode" 0)

   ; Set block & layer names, & other options
   (setq intPostBlock "fb")
   (setq endPlateBlock "END_PLATE")
   (setq ctrLineLayer "center")
   (setq postLayer "Detail")
   (setq dimLayer "Dims")
   (setq railLayer "Detail")
   (setq dimOffset "24")		; String means easy concatenation later
   (setq dim2Offset "54")		; String means easy concatenation later
   (setq railWidth 3)

   ;(ResetCutList)
   
   (PlanDrawTool)			; Actual work done in this function

   ; Reset system variables      
   (setvar "attreq" oldAttributesRequiredSwitch)
   (setvar "osmode" oldSnapMode)
   (setvar "blipmode" oldBlipMode)
   (setvar "clayer" oldLayer)
   (setvar "cmdecho" oldCmdEcho)

   (command "._UNDO" "_End")		; End UNDO group
   
   (setq *error* oldError)
   
   (princ)			; Hide last return value (clean exit)
)




;;; Error handling function - prints error message nicely and resets system variables

(defun errorHandler (msg)
   (princ "\nThere's a slight problem: ")
   (princ msg)

   ;; Reset system variables      
   (setvar "attreq" oldAttributesRequiredSwitch)
   (setvar "osmode" oldSnapMode)
   (setvar "blipmode" oldBlipMode)
   (setvar "clayer" oldLayer)
   (setvar "cmdecho" oldCmdEcho)
   
   (command-s "._UNDO" "_End")		; End UNDO group
   ;(command "._U")			; Undo whatever got done so far
   
   (princ "\nSystem variables reset")
   (princ "\n")
   
   (setq *error* oldError)
   
   (princ))			; Hide last return value (clean exit)




;;; Actual plan drawing tool

(defun PlanDrawTool (/ postSpacing Pt1 Pt2 centerLine
		       spaces lineLength infillLength
		       lineAngle isFirstTime pointList
		       railWidth)

   ; Get user input 
   (setq postSpacing 51) ;(getreal "\nIdeal post spacing in inches:"))
   (setq railWidth 3) ;(getreal "\nRail width in inches:"))
   (setvar "osmode" 2)
   (setq Pt1 (getpoint "\nChoose first point:"))
   (setq pointList (list Pt1))
   (setq isFirstTime "True")

   ; (Start loop for drawing each run)
   (while (/= (setq Pt2 (getpoint Pt1 "\nChoose next point:")) nil)
      (setq pointList (cons Pt2 pointList))
      (setvar "osmode" 0)

      (setq lineAngle (angle Pt1 Pt2))
      (if (/= isFirstTime nil)
         (progn
            (setvar "clayer" postLayer)
            (command "insert" endPlateBlock "y" railWidth "r" 
					(angtos (- lineAngle PI) 0 9) Pt1)
            (setq isFirstTime nil)))

      (setvar "clayer" ctrLineLayer)
      (command "line" Pt1 Pt2 "")
      (setq centerLine (entlast))
      (setq lineLength (distance Pt1 Pt2))
      
      (if (<= lineLength postSpacing)
			(setq spaces 1)
			(setq spaces (FindClosestIntegerDivisor lineLength postSpacing)))
  
      (setvar "clayer" postLayer)
      (if (>= spaces 2) 				; Only divide if necessary
			(command "divide" centerLine "B" intPostBlock "Y" spaces))

      ;; This section adds to cut list for infill/bottom rail/series 100
      (setq infillLength (RoundUpTo 3 (/ lineLength spaces)))
			; calc infill length & round up to the nearest 3"
      (setq *infillCutList* (Assoc+Qty infillLength *infillCutList* spaces))

     
      ;; This section calculates points for dimensions and draws them
      ; Got the angle earlier - lineAngle (in radians)
      (setq spaceLength (/ lineLength spaces))
      (setvar "clayer" dimLayer)
      (setq currentPt Pt1)
      (setq dimIndex spaces)

      (setq offsetString (strcat "@" dim2Offset "<" (angtos (- lineAngle (/ PI 2)) 0 9)))
      (if (> spaces 1)
			(command "dimaligned" Pt1 Pt2 offsetString))

      (while (> dimIndex 0)
         (setq nextPt (polar currentPt lineAngle spaceLength))
         (setq offsetString (strcat "@" dimOffset "<" (angtos (- lineAngle (/ PI 2)) 0 9)))
         (command "dimaligned" currentPt nextPt offsetString)
			; Get last dimension.
			(setq lastDim (entget (entlast)))
			;; Is this the last one? If so use the full label (unless it's the only one).
			;; Otherwise, just label it "EQ".
			(cond
				((and (= dimIndex 1) (= spaces 1)))
				((= dimIndex 2)
					(setq label (strcat "<>\\X(" (itoa spaces) "X)"))
					(entmod (subst (cons 1 label) (assoc 1 lastDim) lastDim)))
				(T
					(entmod (subst (cons 1 "EQ") (assoc 1 lastDim) lastDim))))
	 
			(setq currentPt nextPt)
			(setq dimIndex (1- dimIndex)))
     
      (setq Pt1 Pt2)		; Makes the 1st point equal the current 2nd point
      (setvar "osmode" 2)      ; Prep for next loop
   )

   (setvar "osmode" 0)
   
   (setvar "clayer" postLayer)
   (command "insert" endPlateBlock "y" railWidth "r" (angtos lineAngle 0 9) Pt1)

   (setvar "clayer" railLayer)
   (setq settingsList (list "_.mline" "justification" "zero" "scale" railWidth "style" "TOP_RAIL"))
   (setq settingsList (append settingsList pointList (list "")))
   (apply 'command settingsList) 
   
   (setq *infillcutlist (OrderList *infillCutlist*))
   
   (princ "\nInfill cut list: ")
   (princ *infillCutlist*)

   ;; Function to chop lengths larger than stock length (not necessary?)
   ;; (setq *infillCutlist* (OrderList (ChopLongLengths *infillCutlist* gStockLength)))
   
   ;; Counting for parts list handled in this function
   (setq resultList (CountRails *infillCutlist* 242))
   (princ)

	  
   ;; Display handled in this function
   (princ "\nStock lengths: ")
   (DisplayCount resultList)
   (princ)) ; End function

   


;; CountRails
;; Determines stock lengths needed to fulfill quantities of rail in cutList.
;; cutList - [association list] (Length . qtyNeeded) list of railing cuts (must be shorter than stock length).
;; Returns an association list of stock lengths starting with full length (like cutList).

(defun CountRails (cutList stockLength / stockLengthLeft currentCutIndex stockLengthsNeeded currentCutKey bladeWidth)

   ;Counters
   (setq stockLengthLeft 0.000)
   (setq currentCutIndex 0)
   (setq stockLengthsNeeded 0)	; will become association list (currently integer)
   (setq bladeWidth 0.125)

   (princ "\n")
   (princ "\nStock length cuts")
   (princ "\n--------------------")
   (while (> (length cutList) 0)
      
      (setq currentCutLength (car (nth currentCutIndex cutList)))
      
      (cond
	 ;no more length
	 (
	    (<= stockLengthLeft 0)
	    (princ "\n")
	    (setq stockLengthLeft stockLength)
	    (setq stockLengthsNeeded (1+ stockLengthsNeeded))
	 )
         ;there is more length, but cut won't fit
	 (
	    (and
	       (> stockLengthLeft 0)
	       (> currentCutLength stockLengthLeft)
            )
	    (setq currentCutIndex (1+ currentCutIndex))
	    ;did we go through the whole cutList?
	    (if (= currentCutIndex (length cutList))
	       (progn
		  (setq currentCutIndex 0)
		  (princ "(") (princ stockLengthLeft) (princ ")")
		  (setq stockLengthLeft 0)
	       )
	    )   
	 )
	 ;there is more length and cut will fit
	 (
	    (and
	       (> stockLengthLeft 0)
	       (<= currentCutLength stockLengthLeft)
            )
	    ;subtract cut length from stock length
	    (setq stockLengthLeft (- stockLengthLeft currentCutLength bladeWidth))
            ;print out the length for this cut
	    (princ currentCutLength)
	    (princ " / ")
            ;decrement cut length quantity (or remove from list) - function
	    (setq cutList (assoc-- currentCutLength cutList))
	    ;did we go through the whole cutList?
	    (if (= currentCutIndex (length cutList))
	       (progn
		  (setq currentCutIndex 0)
		  (princ "(") (princ stockLengthLeft) (princ ")")
		  (setq stockLengthLeft 0)
	       )
	    )
	 )

      ;|if length left over
	- stockLength - length left over = odd length
	- decrement stock length qty|;

      ) ;end cond

   ) ;end of while loop
   (princ "\n--------------------")

   (eval stockLengthsNeeded))

 


(princ)		; Clean load