;;;;;;;[  Plan Drawing  ];;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Draws centerline and top rail, places       ;;
;;  intermediate posts, and dimensions all.     ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 10/28/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  11/08/2015                                  ;;
;;  - Added infill counting aspect to           ;;
;;    function.                                 ;;
;;                                              ;;
;;  11/09/2015                                  ;;
;;  - Began work on adding dimensions.          ;;
;;  - Changed RoundUpInt to use RoundUpTo       ;;
;;    (legacy support).                         ;;
;;                                              ;;
;;  11/10/2015                                  ;;
;;  - Added error handling function.            ;;
;;  - Added dimensioning feature.               ;;
;;  - Added infill stock list counting          ;;
;;    feature. This is starting to be awesome!  ;;
;;  - Added feature that changes dimension      ;;
;;    text into "EQ" and "(dim)" over "(#X)".   ;;
;;  - Added mline drawing & inserting end       ;;
;;    plates!!! (Requires TOP_RAIL mlstyle.)    ;;
;;                                              ;;
;;  11/16/2015                                  ;;
;;  - Added user input back.                    ;;
;;                                              ;;
;;  12/02/2015 - JD                             ;;
;;  - Added rubber band feature to point        ;;
;;    picking.                                  ;;
;;  - Changed endplate insertion to railLayer.  ;;
;;                                              ;;
;;  01/19/2016                                  ;;
;;  - Dimension label is now at second-to-last  ;;
;;    dim (just distance on a single dim).      ;;
;;  - Moved helper functions to separate file.  ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Combine with PlanDrawGoal & Comm w/       ;;
;;    appropriate options.                      ;;
;;  - Add top rail counting?                    ;;
;;  - Add choice of post spacing with default.  ;;
;;    Ditto on rail width.                      ;;
;;  - Revise copy on prompts.                   ;;
;;  - Check for endplate block, top_rail        ;;
;;    mline, and current layers and blocks.     ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:plandraw (/ oldAttributesRequiredSwitch oldSnapMode oldBlipMode
		     oldLayer oldCmdEcho intPostBlock ctrLineLayer
		     postLayer dimLayer dimOffset snapMode infillStockLength
			  postSpacing railWidth)
   	             ; Define function (inc. local variables)
   (command "._UNDO" "_Begin")		; Start UNDO group
   
   (setq oldCmdEcho (getvar "cmdecho"))
   (setvar "cmdecho" 0)			; Turn off command line output
					
   ; Save system variables that will be changed
   (setq oldAttributesRequiredSwitch (getvar "attreq"))
   (setq oldSnapMode (getvar "osmode"))
   (setq oldBlipMode (getvar "blipmode"))
   (setq oldLayer (getvar "clayer"))

   ; Change system vriables
   (setvar "attreq" 0)
   (setvar "blipmode" 0)

   ; Set block & layer names, & other options
   (setq intPostBlock "POST-STANCHION")
   (setq endPlateBlock "END_PLATE")
   (setq ctrLineLayer "A-HRAL-CNTR")
   (setq postLayer "A-HRAL-POST")
   (setq dimLayer "Dims")
   (setq railLayer "A-HRAL-RAIL")
   (setq dimOffset "18")		; String means easy concatenation later
	(setq snapMode 10)
	(setq infillStockLength 242)
	
	; Get user input 
   ;(setq postSpacing (getreal "\nMax post spacing in inches:"))
	(setq postSpacing 60)
   ;(setq railWidth (getreal "\nRail width in inches:"))
	(setq railWidth "3")		; String or integer work here

   ;(ResetInfillCutList)
   
   (PlanDrawTool)			; Actual work done in this function

   ; Reset system variables      
   (setvar "attreq" oldAttributesRequiredSwitch)
   (setvar "osmode" oldSnapMode)
   (setvar "blipmode" oldBlipMode)
   (setvar "clayer" oldLayer)
   (setvar "cmdecho" oldCmdEcho)

   (command "._UNDO" "_End")		; End UNDO group
   
   (princ))			; Hide last return value (clean exit)




;;; Error handling function - prints error message nicely and resets system variables

(defun *error* (msg)
   (princ "\n")
   (princ msg)

   ;; Reset system variables      
   (setvar "attreq" oldAttributesRequiredSwitch)
   (setvar "osmode" oldSnapMode)
   (setvar "blipmode" oldBlipMode)
   (setvar "clayer" oldLayer)
   (setvar "cmdecho" oldCmdEcho)

   (princ "\nSystem variables reset")
   (princ "\n")
   
   (princ))			; Hide last return value (clean exit)




;;; Actual plan drawing tool

(defun PlanDrawTool ( /  Pt1 Pt2 centerLine
		       spaces lineLength infillLength
		       lineAngle isFirstTime pointList)

   (setvar "osmode" snapMode)
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
            (setvar "clayer" railLayer)
            (command "insert" endPlateBlock "y" railWidth "r" (angtos (- lineAngle PI) 0 9) Pt1)
            (setq isFirstTime nil)))

      (setvar "clayer" ctrLineLayer)
      (command "line" Pt1 Pt2 "")
      (setq centerLine (entlast))
      (setq lineLength (distance Pt1 Pt2))
      (setq spaces (RoundUpInt (/ lineLength postSpacing)))
  
      (setvar "clayer" postLayer)
      (if (>= spaces 2) 				; Only divide if necessary
			(command "divide" centerLine "B" intPostBlock "Y" spaces))

      ;; This section adds to cut list for infill/bottom rail/series 100
      (setq infillLength (RoundUpTo 3 (/ lineLength spaces)))         ; calc infill length & round up to the nearest 3"
      (setq *infillCutList* (Assoc+Qty infillLength *infillCutList* spaces))

     
      ;; This section calculates points for dimensions and draws them
      ; Got the angle earlier - lineAngle (in radians)
      (setq spaceLength (/ lineLength spaces))
      (setvar "clayer" dimLayer)
      (setq currentPt Pt1)
      (setq dimIndex spaces)

      (while (> dimIndex 0)
         (setq nextPt (polar currentPt lineAngle spaceLength))
         (setq offsetString (strcat "@" dimOffset "<" (angtos (- lineAngle (/ PI 2)) 0 9)))
         (command "dimaligned" currentPt nextPt offsetString)
			; Get last dimension.
			(setq lastDim (entget (entlast)))
			;; Is this the second-to-last one? If so use the full label.
			;; Otherwise, just label it "EQ". (Unless there's only one.)
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
      (setvar "osmode" snapMode))      ; Prep for next loop

   (setvar "osmode" 0)
   
   (setvar "clayer" railLayer)
   (command "insert" endPlateBlock "y" railWidth "r" 
		(angtos lineAngle 0 9) Pt1)

   (setvar "clayer" railLayer)
   (setq settingsList (list "_.mline" "justification" "zero" "scale" railWidth "style" "TOP_RAIL"))
   (setq settingsList (append settingsList pointList (list "")))
   (apply 'command settingsList) 
   
   (setq *infillcutlist* (OrderList *infillCutlist*))
   
   (princ "\nInfill cut list: ")
   (princ *infillCutlist*)

   ;; Counting for parts list handled in this function
	(setvar "dimzin" 8)
	(setq qtyNeeded (CountRails *infillCutList* infillStockLength))
					
	;; Display handled in these functions
	(PrintCutList *fullCutlist*)
	(princ "Stock lengths: ")
	(DisplayCount qtyNeeded)
	(princ "\n")
	
   (princ))

 



(princ)		; Clean load