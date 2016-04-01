;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                         ;;;;
;;;;  JD's Macros for Helpful CAD Functions  ;;;;
;;;;                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;|============{ Post Placing }==============|;
;| Automatic end post and total dimension   |;
;| placement for based on offset distances. |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 03/29/2016 |;
;|==========================================|;

(defun C:placeposts (/  systemVariables cornerPostBlock postLayer snapMode 
							   egdeOffsetDistance wallOffsetDistance pointList)
 
   ; Start UNDO group so the entire process can be easily reversed
	(command "._UNDO" "_Begin")
	; setup custom error message?
	(JD:ClearVars 'systemVariables)
	(JD:Save&ChangeVar "cmdEcho" 'systemVariables 0)
   (JD:Save&ChangeVar "attreq" 'systemVariables 0)
   (JD:Save&ChangeVar "blipmode" 'systemVariables 0)

   ; Set block & layer names, & other options
   (setq endPostBlock "KnifePlatePost-Roof")
   (setq cornerPostBlock endPostBlock)
   (setq postLayer "Detail")
	
	; Get user input for this??
   (setq edgeOffsetDistance 3.31294279)
   (setq wallOffsetDistance 4.5)


	(setq pointList (GetPointList))
	
	(PlaceMainPosts pointList)
	(princ "dims")
	(DimExterior pointList)
	
              
	(JD:ResetAllVars 'systemVariables)
   (command "._UNDO" "_End")		; End UNDO group
   
   (princ))	
	
	

; Helper function for above - for now... 3/23/2016
	
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
	
	(JD:Save&ChangeVar "clayer" 'systemVariables postLayer)
	(setq theAngle (angtos lineAngle 0 9))	
   (command "._insert" endPostBlock "s" 1 "r" theAngle Pt1offset)
	
	(foreach Pt3 pointList
		(princ "\nPt1: ")(princ pt1)
		(princ "\nPt2: ")(princ pt2)
		
		(setq incomingAngle (angle Pt2 Pt1))
		(setq outgoingAngle (angle Pt2 Pt3))
		; determine average angle (bisector of the two angles)
		
		(princ "\nincomingAngle: ")(princ incomingAngle)
		(princ "\noutgoingAngle: ")(princ outgoingAngle)
		
		(setq offsetAngle
			(+ (/ (- incomingAngle outgoingAngle) 2) outgoingAngle))
		
		; determine offset distance (trig math)
		(setq offsetDistance
			(/ edgeOffsetDistance (sin (abs (- offsetAngle outgoingAngle)))))
		
		(princ "\noffsetAngle: ")(princ offsetAngle)
		(princ "\noffsetDistance: ")(princ offsetDistance)
				
		(setq Pt2offset (polar Pt2 offsetAngle offsetDistance))
		(setq Pt2offset (list (car Pt2offset) (cadr Pt2offset)))
		
		(princ "\nPt2: ")(princ pt2)
		(princ "\nPt2Offset: ")(princ pt2offset)
				
		(setq theAngle (angtos outgoingAngle 0 9))
		(command "._insert" cornerPostBlock "s" 1 "r" theAngle Pt2offset)
		(princ "\n")
		; Prep for next round
		(setq Pt1 Pt2)
		(setq Pt2 Pt3))
	
	(princ "\nlast pt")
	(setq lineAngle (angle Pt1 Pt2))	
	(setq offsetAngle (+ lineAngle (/ PI 2)))
	
	(princ "\ncalc")
	(setq Pt2offset (polar Pt2 lineAngle (- 0 wallOffsetDistance)))
	(setq Pt2offset (polar Pt2offset offsetAngle edgeOffsetDistance))
	(setq Pt2offset (list (car Pt2offset) (cadr Pt2offset)))
	
	(princ "\ninsert")
	(setq theAngle (angtos lineAngle 0 9))
   (command "._insert" endPostBlock "s" 1 "r" theAngle Pt2offset)
	
	(princ "\nreset")
	(JD:ResetVar "clayer" 'systemVariables)
	
	)	


	
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
	(setq dimOffset 36)
	
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


	
; Helper function version of above - for now... 3/22/2016

(defun DimExterior ( pointList / snapMode  dimOffset lastPoint)

	
	(setq snapMode 191)
	(setq dimOffset 48)
	
	(JD:Save&ChangeVar "osmode" 'systemVariables snapMode)
	
	(setq lastPoint (car pointList))
	(setq pointList (cdr pointList))
	   
   (foreach currentPoint pointList
		
      (setq angleToOffset (- (angle lastPoint currentPoint) (/ PI 2)))
		(setq offsetPoint (polar currentPoint angleToOffset dimOffset))
      
		
      (command "dimaligned" lastPoint currentPoint offsetPoint)
		      
      (setq lastPoint currentPoint))
      
	
   (princ))
	
	
	
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
	
	
	
	
	
(princ)
	
	