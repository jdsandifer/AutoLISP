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
;;  03/31/2016                                  ;;
;;  - Added post call-out placement.            ;;
;;                                              ;;
;;  04/04/2016                                  ;;
;;  - Made post tag placement conditional.      ;;
;;    (Only does it if it's a cable railing.)   ;;
;;                                              ;;
;;  04/13/2016                                  ;;
;;  - Changed filename to DRAW_PLAN.            ;;
;;                                              ;;
;;  05/03/2016                                  ;;
;;  - Made end plate placement conditional.     ;;
;;    (Only does it if flag is true - setup     ;;
;;    for Series 100 railing.)                  ;;
;;                                              ;;
;;  06/15/2016                                  ;;
;;  - Revised cable drawing for pline with      ;;
;;    width, offsetting first point, and        ;;
;;    placing threaded terminal.                ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Combine with PlanDrawGoal & Comm w/       ;;
;;    appropriate options.                      ;;
;;  - Add choice of post spacing with default.  ;;
;;    Ditto on rail width.                      ;;
;;  - Revise copy on prompts.                   ;;
;;  - Check for endplate block, top_rail        ;;
;;    mline, and current layers and blocks.     ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:dp (/ intPostBlock ctrLineLayer postLayer dimLayer dimOffset
					snapMode infillStockLength postSpacing railWidth
					isCableRailing isPicketRailing cableOffsetDistance
					tagOffsetDistance tagLayer tagBlock cableLayer 
					placeDims placeEndPlates placePosts placeCenteLine
					tagScale cableEndShortening placeRail)
	
   (command "._UNDO" "_Begin")		; Start UNDO group
	
   ; Save system variables and change to needed values
   (JD:ClearVars 'systemVariables)
   (JD:Save&ChangeVar "cmdEcho" 'systemVariables 0)
   (JD:Save&ChangeVar "attreq" 'systemVariables 0)
   (JD:Save&ChangeVar "blipmode" 'systemVariables 0)
   (JD:SaveVar "osmode" 'systemVariables)
   (JD:SaveVar "clayer" 'systemVariables)
   
   ; Set block names
   (setq intPostBlock "BP"
			endPlateBlock "end_plate"
			tagBlock "POST-DRILLED CALL-OUT")
			
	; layer names
   (setq ctrLineLayer "Center"
			postLayer "Detail"
			dimLayer "Dims"
			railLayer "1"
			tagLayer "POST-TAG"
			cableLayer "Cable")
			
	; and other options
   (setq dimOffset "24")		; String means easy concatenation later
	(setq	tagOffsetDistance 24
			tagScale 4
			cableOffsetDistance 24
			cableEndShortening 12
			snapMode 64)
	
	; rail options
	(setq postSpacing 50.75)
	(setq railWidth "2 11/16")		; String or integer works here
	
	; Set flags to control whether certain things are drawn
	(setq isCableRailing T
			isPicketRailing nil
			placeDims nil
			placeEndPlates nil
			placePosts T
			placeCenteLine nil
			placeRail nil)
   
   (PlanDrawTool)			; Actual work done in this function

   (JD:ResetAllVars 'systemVariables)
   (command "._UNDO" "_End")		; End UNDO group
   
   (princ))			; Hide last return value (clean exit)



;;; Error handling function - prints error message nicely and resets system variables

(defun *error* (msg)
   (princ "\n")
   (princ msg)

   (JD:ResetAllVars 'systemVariables)
   
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
      ;; Keep a list of the selected points for later use
		(setq pointList (append pointList (list Pt2)))
      
		;; Change snap mode so it doesn't interfere with auto-drawing
		(setvar "osmode" 0)

		;; Main calculations
      (setq lineAngle (angle Pt1 Pt2))
      (setq lineLength (distance Pt1 Pt2))
      (setq spaces (RoundUpInt (/ lineLength postSpacing)))
		(setq spaceLength (/ lineLength spaces))
		
		;; Insert the starting endplate if this is the first post
		(if (and isFirstTime placeEndPlates)
         (progn
            (setvar "clayer" railLayer)
            (command "._insert" endPlateBlock "y" railWidth "r" 
					(angtos (- lineAngle PI) 0 9) Pt1)
            (setq isFirstTime nil)))
      
		;; Place mid posts if needed
      (if (and (>= spaces 2) placePosts)
			(progn
				(setvar "clayer" postLayer)
				(command "line" Pt1 Pt2 "")
				(setq centerLine (entlast))
				(command "divide" centerLine "B" intPostBlock "Y" spaces)
				(entdel centerLine)))

      ;; This section sets up points for dimensions and draws them
		;; using a loop to go through segment by segment
      (setq currentPt Pt1)
		(setq currentPtList (list Pt1))
      (setq dimIndex spaces)	; spaces should always be at least 1
      (while (and (> dimIndex 0)
						(or placeCenteLine placeDims isCableRailing isPicketRailing))
         ;; Calculate next point and add it to the list for this section
			(setq nextPt (polar currentPt lineAngle spaceLength))
			(setq currentPtList (append currentPtList (list nextPt)))
			
			;; Draw the centerline (pline)
			(cond 
				(placeCenteLine
					(setvar "clayer" ctrLineLayer)
					(command "._pline" currentPt nextPt "")))
			
         (cond
				(placeDims
					(setq offsetString (strcat "@" dimOffset "<" 
												(angtos (- lineAngle (/ PI 2)) 0 9)))
					(setvar "clayer" dimLayer)
					(command "._dimaligned" currentPt nextPt offsetString)
					; Get last dimension.
					(setq lastDim (entget (entlast)))
					;; Is this the second-to-last one? If so use the full label.
					;; Otherwise, just label it "EQ". (Unless there's only one.)
					(cond
						((and (= dimIndex 1) (= spaces 1)))
						((= dimIndex 2)
							(setq label (strcat "<>\\X(" (itoa spaces) "X)"))
							(entmod (subst (cons 1 label) 
											   (assoc 1 lastDim)
												lastDim)))
						(T
							(entmod (subst (cons 1 "EQ") 
												(assoc 1 lastDim) 
												lastDim))))))
			
			
			;; add post call out for each intermediate post
			(cond
				(	(and (> dimIndex 1)	isCableRailing)
					(setq postTagPt (polar nextPt (+ (/ PI 2) lineAngle) 		
												  tagOffsetDistance))
					(setvar "clayer" tagLayer)
					(command "._insert" tagBlock "s" tagScale "r" 0 postTagPt))
					
				(	isPicketRailing
					(setq postTagPt 
						(polar
							(polar nextPt 
									 (+ (/ PI 2) lineAngle) 		
									 tagOffsetDistance)
							lineAngle
							(- 0 (/ spaceLength 2))))
					(setvar "clayer" tagLayer)
					(command "._insert" tagBlock "s" tagScale "r" 0 postTagPt)
					))
								
			; prep for next loop (dimensioning & call-outs on next segment)
			(setq currentPt nextPt)
			(setq dimIndex (1- dimIndex)))
			
		;; Draw centerline (pline) - when I figure out how to feed in the pts
		;(setvar "clayer" ctrLineLayer)
		;(command "._pline" currentPtList "")
      
      ;; Prep for next loop (next railing segment)
		(setq Pt1 Pt2)
      (setvar "osmode" snapMode))    

   (setvar "osmode" 0)
   
	(cond (placeEndPlates
		(setq insertAngle (angtos lineAngle 0 9))
		(setvar "clayer" railLayer)
		(command "._insert" endPlateBlock "y" railWidth "r" insertAngle Pt1)))

   (cond (placeRail
		(setvar "clayer" railLayer)
		(setq settingsList (list "_.mline" "justification" "zero" "scale" railWidth "style" "TOP_RAIL"))
		(setq settingsList (append settingsList pointList (list "")))
		(apply 'command settingsList)))
	
	(cond (isCableRailing
		(setvar "clayer" cableLayer)
		(DrawCableLine pointList cableOffsetDistance)))
   
   (setvar "dimzin" 8)
		
   (princ))
	
	
	
;|========={ Draw cable lines }=============|;
;| Draw polyline for cable run base on      |;
;| a provided point list and distance.      |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 03/31/2016 |;
;|==========================================|;

(defun DrawCableLine (pointList cableOffsetDistance / 
										  Pt1 Pt2 Pt3 lineAngle offsetAngle
										  Pt1offset Pt2offset lastPt1offset
										  lastLineAngle)

	(JD:ChangeVar "osmode" 0)
	
	(setq CABLE_WIDTH "3/8"
			TERMINAL_BLOCK "TOGGLE")
	
	(setq Pt1 (JD:CarCdr 'pointList))
	(setq Pt2 (JD:CarCdr 'pointList))
	
	(setq lineAngle (angle Pt1 Pt2))
	(setq offsetAngle (- lineAngle (/ PI 2)))
		
	(setq Pt1offset (polar
							(polar Pt1 offsetAngle cableOffsetDistance)
							(+ offsetAngle (/ PI 2)) 
							cableEndShortening))	
	(setq Pt2offset (polar Pt2 offsetAngle cableOffsetDistance))
	
	(command "._pline" Pt1offset "W" CABLE_WIDTH CABLE_WIDTH Pt2offset "")
	(command "._insert" TERMINAL_BLOCK "r" 
							(angtos (+ offsetAngle (/ PI 2)) 0 9) "s" 1 Pt1offset)
	
	(setq lastPt1offset Pt2offset)
	(setq lastLineAngle lineAngle)
	(setq Pt1 Pt2)
	
	(foreach Pt2 pointList
		
		(setq lineAngle (angle Pt1 Pt2))
		(setq offsetAngle (- lineAngle (/ PI 2)))
		(setq oldOffsetAngle (- lastLineAngle (/ PI 2)))
		
		(setq Pt1offset (polar Pt1 offsetAngle cableOffsetDistance))
		(setq Pt2offset (polar Pt2 offsetAngle cableOffsetDistance))
		
		(command "._arc" "c" Pt1 lastPt1offset Pt1offset)
		(command "._pline" Pt1offset "W" CABLE_WIDTH CABLE_WIDTH Pt2offset "")
		
		; Prep for next round
		(setq lastPt1offset Pt2offset)
		(setq lastLineAngle lineAngle)
		(setq Pt1 Pt2))
		
	(princ))

	
;|=========={ Get point list }==============|;
;| Get a series of points from the user     |;
;| and return a list of the points.         |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 03/22/2016 |;
;|==========================================|;


(defun GetPointList ( / selectedPoint lastPoint pointList)

	(setq selectedPoint (getpoint "\nChoose first point:"))
	(setq pointList (append pointlist (list selectedPoint)))
	(setq lastPoint selectedPoint)

	(while (/= 
		(setq selectedPoint (getpoint lastPoint "\nChoose next point:"))
		nil)
		(setq pointList (append pointlist (list selectedPoint)))
		(setq lastPoint selectedPoint))
      
	pointList)

 

;;----------------------------------------------------------------------;;


(princ
    (strcat
        "\n:: DRAW_PLAN.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::\n"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;