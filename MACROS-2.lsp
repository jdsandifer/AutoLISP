;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                         ;;;;
;;;;  JD's Macros for Helpful CAD Functions  ;;;;
;;;;                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;|============{ Post Placing }==============|;
;| Automatic end post and total dimension   |;
;| placement based on offset distances.     |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 05/16/2016 |;
;|==========================================|;

(defun C:pp (/ systemVariables cornerPostBlock postLayer snapMode 
					egdeOffsetDistance wallOffsetDistance pointList
					dimLayer isCableRailing dimOffset tagScale)
	; pp - heehee!
   ; Start UNDO group so the entire process can be easily reversed
	(command "._UNDO" "_Begin")
	; setup custom error message?
	(JD:ClearVars 'systemVariables)
	(JD:Save&ChangeVar "cmdEcho" 'systemVariables 0)
	(JD:Save&ChangeVar "osmode" 'systemVariables 33)
   (JD:Save&ChangeVar "attreq" 'systemVariables 0)
   (JD:Save&ChangeVar "blipmode" 'systemVariables 0)

   ; Set block & layer names, & other options
   (setq postLayer "Detail")
	(setq endPostBlock "BP")
   (setq cornerPostBlock endPostBlock)
	(setq dimLayer "Dims")
	(setq isCableRailing T)
	(setq tagLayer "POST-TAG")
	(setq tagBlock "POST-DRILLED CALL-OUT")
	
	; Set distance options
   (setq edgeOffsetDistance 4.5)
   (setq wallOffsetDistance 4.5)
	(setq tagOffsetDistance 9)
	(setq tagScale 1)
	(setq dimOffset 48)

	; Run dialog box to get user input
	; Warn if dialog fails and exit
	; Eventually, define a function based on the input so settings are
	;   saved and can be easily repeated without choices needed each time
	;   (even after the file is closed if the settings aren't changed)
	
	(setq setupDCLID (load_dialog "PostPlacementSetup.dcl"))
	
	(if (not (new_dialog "PostPlacementSetup" setupDCLID))
		(princ "\nDialog box not found in file!\n")
		;(exit)
		)
	
	(action_tile "surface"
		"(setq endPostBlock \"BP\")
		(setq cornerPostBlock endPostBlock)
		(setq edgeOffsetDistance 4.5)")
	(action_tile "fascia" 
		"(setq endPostBlock \"FB\")
		(setq cornerPostBlock \"FBO\")
		(setq edgeOffsetDistance (- 0 2.4375))")
	(action_tile "stanchion" "(princ \"Stanchion mount selected!\")")
	(action_tile "core" "(princ \"Core mount selected!\")")
	
	(action_tile "detail" "(setq postLayer \"Detail\")")
	(action_tile "hral-post" "(setq postLayer \"A-HRAL-POST\")")
	
	(action_tile "cable" "(setq isCableRailing T)")
	(action_tile "noCable" "(setq isCableRailing nil)")
		
	(action_tile "accept" "(done_dialog)(setq userChoice T)")
	(action_tile "cancel" "(done_dialog)(setq userChoice nil)")
	
	(start_dialog)
	(unload_dialog setupDCLID)
	
	(if (not userChoice)
		(exit))
	
	(setq pointList (GetPointList))
	
	(PlaceMainPosts pointList)
	(DimExterior pointList dimOffset)
	
              
	(JD:ResetAllVars 'systemVariables)
   (command "._UNDO" "_End")		; End UNDO group
   
   (princ))	
	
	

; Helper function for above - for now... 4/07/2016
	
(defun PlaceMainPosts ( pointList / Pt1 Pt2 Pt2offset Pt1offset)
	
	(JD:Save&ChangeVar "osmode" 'systemVariables 0)
	
	(setq Pt1 (car pointList))
	(setq pointList (cdr pointList))
	
	(setq Pt2 (car pointList))
	(setq pointList (cdr pointList))
	
	(setq lineAngle (angle Pt1 Pt2))
	(setq offsetAngle (+ lineAngle (/ PI 2)))
		
	(setq Pt1offset (polar Pt1 lineAngle wallOffsetDistance))
	(setq Pt1offset (polar Pt1offset offsetAngle edgeOffsetDistance))
	(setq Pt1offset (list (car Pt1offset) (cadr Pt1offset)))
	
	(setq postTagPt (polar Pt1offset offsetAngle tagOffsetDistance))
	(setq postTagPt (list (car postTagPt) (cadr postTagPt)))
	
	(JD:Save&ChangeVar "clayer" 'systemVariables postLayer)
	(setq theAngle (angtos lineAngle 0 9))	
   (command "._insert" endPostBlock "s" 1 "r" theAngle Pt1offset)
	
	(cond (isCableRailing
			(setvar "clayer" tagLayer)
			(command "._insert" tagBlock "s" tagScale "r" 0 postTagPt)
			(setq lastTag (entget (entnext (entlast))))
			(entmod (subst (cons 1 "B") (assoc 1 lastTag) lastTag))))
	
	(foreach Pt3 pointList
		(setq incomingAngle (angle Pt2 Pt1))
		(setq outgoingAngle (angle Pt2 Pt3))
		
		
		; determine average angle (bisector of the two angles)
		(cond
			((> outgoingAngle incomingAngle)
			 (setq offsetAngle
			    (- (+ (/ (- outgoingAngle incomingAngle) 2) incomingAngle) PI)))
			
			((> incomingAngle outgoingAngle)	 
			 (setq offsetAngle
				 (+ (/ (- incomingAngle outgoingAngle) 2) outgoingAngle))))
		
		;; determine corner offset distances (trig math)
		(setq offsetDistance	(/ edgeOffsetDistance 
			    (sin (abs (- incomingAngle offsetAngle)))))
		(setq cornerTagOffsetDistance
			(/ tagOffsetDistance (sin (abs (- incomingAngle offsetAngle)))))
				
		(setq Pt2offset (polar Pt2 offsetAngle offsetDistance))
		(setq Pt2offset (list (car Pt2offset) (cadr Pt2offset)))
		
		
		(setq postTagPt (polar Pt2offset offsetAngle cornerTagOffsetDistance))
		(setq postTagPt (list (car postTagPt) (cadr postTagPt)))
				
		(setq theAngle (angtos outgoingAngle 0 9))
		(setvar "clayer" postLayer)
		(command "._insert" cornerPostBlock "s" 1 "r" theAngle Pt2offset)
		
		(cond (isCableRailing
			(setvar "clayer" tagLayer)
			(command "._insert" tagBlock "s" tagScale "r" 0 postTagPt)
			(setq lastTag (entget (entnext (entlast))))
			(entmod (subst (cons 1 "D") (assoc 1 lastTag) lastTag))))
		
		; Prep for next round
		(setq Pt1 Pt2)
		(setq Pt2 Pt3))
	
	(setq lineAngle (angle Pt1 Pt2))	
	(setq offsetAngle (+ lineAngle (/ PI 2)))
	
	(setq Pt2offset (polar Pt2 lineAngle (- 0 wallOffsetDistance)))
	(setq Pt2offset (polar Pt2offset offsetAngle edgeOffsetDistance))
	(setq Pt2offset (list (car Pt2offset) (cadr Pt2offset)))
	
	(setq postTagPt (polar Pt2offset offsetAngle tagOffsetDistance))
	(setq postTagPt (list (car postTagPt) (cadr postTagPt)))
	
	(setq theAngle (angtos lineAngle 0 9))
	(setvar "clayer" postLayer)
   (command "._insert" endPostBlock "s" 1 "r" theAngle Pt2offset)
	
	(cond (isCableRailing
		(setvar "clayer" tagLayer)
		(command "._insert" tagBlock "s" tagScale "r" 0 postTagPt)
		(setq lastTag (entget (entnext (entlast))))
		(entmod (subst (cons 1 "C") (assoc 1 lastTag) lastTag))))
	
	(JD:ResetVar "clayer" 'systemVariables)
	
	(princ))	


	
;|========{ Continuous Dimlinear }==========|;
;| Allows for continual dimensioning at     |;
;| a specific distance from the points.     |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 03/16/2016 |;
;|==========================================|;


(defun C:DLICont ( / snapMode  dimOffset    systemVariables 
							lastPoint currentPoint angleToOffset   offsetPoint)

	(command "._UNDO" "_Begin")		; Start UNDO group
   
	(setq snapMode 191)
	(setq dimOffset 48)
	
	(JD:ClearVars 'systemVariables)
	(JD:Save&ChangeVar "osmode" 'systemVariables snapMode)
	
	
   (setq lastPoint (getpoint "\nChoose first point:"))

   (while (/= 
		(setq currentPoint (getpoint lastPoint "\nChoose next point:"))
		nil)
		
      (setq angleToOffset (- (angle lastPoint currentPoint) (/ PI 2)))
		(setq offsetPoint (polar currentPoint angleToOffset dimOffset))
      
		
      (command "dimlinear" lastPoint currentPoint offsetPoint)
		      
      (setq lastPoint currentPoint))
      
	(JD:ResetAllVars 'systemVariables)
	(command "._UNDO" "_End")		; End UNDO group
	
   (princ))


	
; Helper function version of above - for now... 5/04/2016

(defun DimExterior ( pointList dimOffset / snapMode  dimOffset lastPoint)

	
	(JD:Save&ChangeVar "osmode" 'systemVariables 0)
	(JD:Save&ChangeVar "clayer" 'systemVariables dimLayer)
	
	(setq lastPoint (car pointList))
	(setq pointList (cdr pointList))
	   
   (foreach currentPoint pointList
		
      (setq angleToOffset (- (angle lastPoint currentPoint) (/ PI 2)))
		(setq offsetPoint (polar currentPoint angleToOffset dimOffset))
      
		
      (command "dimaligned" lastPoint currentPoint offsetPoint)
		      
      (setq lastPoint currentPoint))
      
	(JD:ResetVar "clayer" 'systemVariables)
	(JD:ResetVar "osmode" 'systemVariables)
   
	(princ))
	
	
	
;|=========={ Dimension Labels }============|;
;| Places label for middle dimension and    |;
;| blanks for all others.                   |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 04/25/2016 |;
;|==========================================|;


(defun c:DougsDimLabels ();( / dimSelSet      index      selSetLength 
							;  dimEntityInfo  dimEntityText)
	(setq dimSelSet (ssget '((0 . "DIMENSION"))))
	(setq index 0)
	(setq selSetLength (sslength dimSelSet))
	(while (< index selSetLength)
		(setq dimEntity (ssname dimSelSet index))
		(setq dimEntityInfo (entget dimEntity))
		(setq dimEntityText (assoc 1 dimEntityInfo))
		(setq index (1+ index))	
		(if (= index (RoundTo 1 (/ selSetLength 2.0)))
			(progn
				(entmod
					(subst 
						(cons 1 (strcat (itoa selSetLength) " EQ. SPACES (~<>)"))
						dimEntityText
						dimEntityInfo))
				(setq dimEntityInfo (entget dimEntity))	
				(setq bgFillEntityData
						'((-3 ("ACAD" (1000 . "DSTYLE") 
										  (1002 . "{") 
										  (1070 . 69) 
										  (1070 . 1) 
										  (1002 . "}")))))
				(setq newDimEntityInfo (append dimEntityInfo bgFillEntityData))
				(entmod newDimEntityInfo)
				(vl-cmdf "_draworder" dimEntity "" "f")
				)
			(entmod (subst (cons 1 " ") dimEntityText dimEntityInfo))))
	(print))

;|============{ Draw Handrail }=============|;
;| Places beginning, end, and mid post      |;
;| lines for 1.9" dia. handrail elevation.  |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 03/16/2016 |;
;|==========================================|;


(defun c:DrawHandrail (/ systemVariables point0
								 point1     point2    point3 point4)
	(command "._UNDO" "_Begin")
	; setup custom error message?
	(JD:ClearVars 'systemVariables)
	(JD:Save&ChangeVar "CMDECHO" 'systemVariables 0)
	(JD:Save&ChangeVar "OSMODE" 'systemVariables 33)
		
	; Get beginning (left-most) point from user
	(initget 1)
	(setq point0 (getpoint "Please select the beginning point."))
	; Turn off snaps so it doesn't affect drawing
	(setvar "OSMODE" 0)
	(setq point0 (polar point0 (dtr -90) 8))
	(setq point2 (polar point0 (dtr 180) 0.95))
	(setq point3 (polar point0 (dtr 0) 0.95))
	(setq point1 (polar point2 (dtr 90) 42))
   (setq point4 (polar point3 (dtr 90) 42)) 	
	(command "._PLINE" point1 point2 point3 point4 "")
	; Set snaps to intersection and endpoint
	(setvar "OSMODE" 33)
	
	; Get first mid point from user
	(initget 1)
	(setq point0 (getpoint "Please select the frist mid point."))
	; Turn off snaps so it doesn't affect drawing
	(setvar "OSMODE" 0)
	(setq point0 (polar point0 (dtr -90) 8))
	(setq point2 (polar point0 (dtr 180) 0.95))
	(setq point3 (polar point0 (dtr 0) 0.95))
	(setq point1 (polar point2 (dtr 90) 48))
   (setq point4 (polar point3 (dtr 90) 48)) 	
	(command "._PLINE" point1 point2 point3 point4 "")
	; Set snaps to intersection and endpoint
	(setvar "OSMODE" 33)
	
	; Get second mid point from user
	(initget 1)
	(setq point0 (getpoint "Please select the second mid point."))         
	; Turn off snaps so it doesn't affect drawing
	(setvar "OSMODE" 0)
	(setq point0 (polar point0 (dtr -90) 8))
	(setq point2 (polar point0 (dtr 180) 0.95))
	(setq point3 (polar point0 (dtr 0) 0.95))
	(setq point1 (polar point2 (dtr 90) 48))
   (setq point4 (polar point3 (dtr 90) 48)) 	
	(command "._PLINE" point1 point2 point3 point4 "")
	; Set snaps to intersection and endpoint
	(setvar "OSMODE" 33)
	
	; Get end (right-most) point from user
	(initget 1)
	(setq point0 (getpoint "Please select the end point."))
	; Turn off snaps so it doesn't affect drawing
	(setvar "OSMODE" 0)
	(setq point0 (polar point0 (dtr -90) 8))
	(setq point2 (polar point0 (dtr 180) 0.95))
	(setq point3 (polar point0 (dtr 0) 0.95))
	(setq point1 (polar point2 (dtr 90) 42))
   (setq point4 (polar point3 (dtr 90) 42)) 	
	(command "._PLINE" point1 point2 point3 point4 "")
	
	(JD:ResetAllVars 'systemVariables)
	(command "._UNDO" "_End")		; End UNDO group
	
	(princ))
	
	
	
;|==========={ Rotate Block 90° }===========|;
;| Rotates a user-selected block 90 degrees |;
;| (counter-clockwise) about its insertion. |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 03/16/2016 |;
;|==========================================|;

;; Note: Only works in world/standard UCS.

(defun c:RotateBlock90 (/ systemVariables block selSet 
								  entityInfo point )
	(command "._UNDO" "_Begin")
	; setup custom error message?
	(JD:ClearVars 'systemVariables)
	(JD:Save&ChangeVar "CMDECHO" 'systemVariables 0)
	(JD:SaveVar "OSMODE" 'systemVariables)
	
	; Get block entity name
	(setq block (car (entsel "\nSelect block to rotate 90 degrees:")))
	
	; Create a new selection set with the block
	(setq selSet (ssadd block))
	
	; Get the block's entity info
	(setq entityInfo (entget block))
	; and extract the insertion point
	(setq point (cdr (assoc 10 entityInfo)))
	
	
	; Turn off snaps so it doesn't affect this
	(setvar "OSMODE" 0)
	; Rotate the block 90° (counter-clockwise) about it's insertion point
	(command "._ROTATE" selSet "" point "90")
	
	(JD:ResetAllVars 'systemVariables)
	(command "._UNDO" "_End")		; End UNDO group
	
	(princ) )
	
	
	

;|==========={ Replace Block }==============|;
;| Replaces selected blocks with another    |;
;| previously selected source block.        |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 01/26/2016 |;
;|------------------------------------------|;
;| Based on: Replace Selected Blocks        |;
;|           (17-IV-2012)                   |;
;|           by Mircea (MSasu)              |;
;|           as posted on CadTutor.net      |;
;|           and accessed on 2016-01-18     |; 
;|==========================================|;


(defun c:ReplaceBlock ( / sourceBlock  sourceBlockName  sourceBlockEntity   
								  targetBlock  targetBlockInfo )
	(command "._UNDO" "_Begin")		; Start UNDO group
	
	(prompt "\nSelect source block: ")	
   ; Ask user to select a source block and continue only if one is found	
	(if (setq sourceBlock (ssget "_:S:E" '((0 . "INSERT"))))
		(progn
			; Get the entity name of the source block
			(setq sourceBlockEntity (ssname sourceBlock 0))
			; Get the actual name of the source block from the entity
			(setq sourceBlockName (assoc 2 (entget sourceBlockEntity)))
			; Highlight the source block (to show what's being copied)
			(redraw sourceBlockEntity 3)
			
			(prompt "\nSelect blocks to be replaced (press Enter to exit): ")
			; Start a while loop to act every time the user selects a block
			; and exit if the user presses enter
			(while (setq targetBlock (ssget "_:S:E" '((0 . "INSERT"))))
				; Get the info for the target block (so it can be changed)
				(setq targetBlockInfo (entget (ssname targetBlock 0)))
				; Subsitute the source block's name in the target block's info
				(entmod (subst sourceBlockName 
					(assoc 2 targetBlockInfo) targetBlockInfo))

				(prompt "\nSelect blocks to be replaced (press Enter to exit): ") )
		; Remove highlighting on the source block
		(redraw sourceBlockEntity 4) ))
		
   (command "._UNDO" "_End")		; End UNDO group
		
	(princ) )
	
	
	
	
;|=========={ Replace Blocks }==============|;
;| Replaces blocks in selection with the    |;
;| previously selected source block.        |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 02/16/2016 |;
;|------------------------------------------|;
;| Based on: Replace Selected Blocks        |;
;|           (17-IV-2012)                   |;
;|           by Mircea (MSasu)              |;
;|           as posted on CadTutor.net      |;
;|           and accessed on 2016-01-18     |; 
;|==========================================|;


(defun c:ReplaceBlocks ( / sourceBlock  sourceBlockName  sourceBlockEntity   
								  targetBlocks  targetBlockInfo  index)
	(command "._UNDO" "_Begin")		; Start UNDO group
	
	(prompt "\nSelect source block: ")	
   ; Ask user to select a source block and continue only if one is found	
	(if (setq sourceBlock (ssget "_:S:E" '((0 . "INSERT"))))
		(progn
			; Get the entity name of the source block
			(setq sourceBlockEntity (ssname sourceBlock 0))
			; Get the actual name of the source block from the entity
			(setq sourceBlockName (assoc 2 (entget sourceBlockEntity)))
			; Highlight the source block (to show what's being copied)
			(redraw sourceBlockEntity 3)
			
			(prompt "\nSelect blocks to be replaced (press Enter to exit): ")
			; Start a while loop to act every time the user selects a block
			; and exit if the user presses enter
			(while (setq targetBlocks (ssget '((0 . "INSERT"))))
				(setq index 0)
				(while (ssname targetBlocks index) 
					; Get the info for the target block (so it can be changed)
					(setq targetBlockInfo (entget (ssname targetBlocks index)))
					; Subsitute the source block's name in the target block's info
					(entmod (subst sourceBlockName 
						(assoc 2 targetBlockInfo) targetBlockInfo))
					(setq index (1+ index)))

				(prompt "\nSelect blocks to be replaced (press Enter to exit): ") )
		; Remove highlighting on the source block
		(redraw sourceBlockEntity 4) ))
		
   (command "._UNDO" "_End")		; End UNDO group
		
	(princ) )
	
	
	
	
	
;;----------------------------------------------------------------------;;


(princ
    (strcat
        "\n:: MACROS.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::\n"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;