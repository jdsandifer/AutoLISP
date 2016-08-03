;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                         ;;;;
;;;;  JD's Macros for Helpful CAD Functions  ;;;;
;;;;                                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;|===={ Parts List Block Conversion }=======|;
;| Converts W,X,Y,Z block tags to more      |;
;| descriptive and consistent tags.         |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/25/2016 |;
;|==========================================|;

(defun C:plbc (/ *error* systemVariables blocksSelSet)
	
   ; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
	; Start UNDO group so the entire function can be easily reversed
	(command "._UNDO" "_Begin")
	(JD:ClearVars 'systemVariables)
	(JD:Save&ChangeVar "cmdEcho" 'systemVariables 0)

   
	
	(setq blocksSelSet (ssget '((0 . "INSERT"))))
	(foreach currentBlock (JD:SelSet->List blocksSelSet)
		(ConvertPLBlock currentBlock))	
	
              
				  
	(JD:ResetAllVars 'systemVariables)
   (command "._UNDO" "_End")		; End UNDO group
   (princ))	
	
	
	
;|===={ Parts List Block Conversion }=======|;
;| Helper function. Converts a block from   |;
;| John's attribute names to JD's. I.e.     |;
;| makes them descriptive and consistent.   |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/25/2016 |;
;|==========================================|;

(defun ConvertPLBlock (block / )
	(cond
		(	(= "John" (getPLBlockType block))
			(ConvertEntities (GetListOfAttributeEntities block))))
	(princ))
	
	
	
;|===={ Entity List Conversion }============|;
;| Helper function. Converts each entity    |;
;| in the supplied entity list for better   |;
;| tags - descriptive & consistent.         |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/27/2016 |;
;|==========================================|;

(defun ConvertEntities (entityList / )
	(setq attNameList
		(mapcar 
			'(lambda (entity) (cdr (assoc 2 (entget entity))))
			entityList))
	(cond
		((filter '(lambda (str) (= str "W")) attNameList)
			(ConvertEntityTag "W" "QTY" entityList)
			(ConvertEntityTag "X" "LEN" entityList)
			(ConvertEntityTag "Y" "QTY2" entityList)
			(ConvertEntityTag "Z" "LEN2" entityList))
		(T
			(ConvertEntityTag "X" "QTY" entityList)))
	(ConvertEntityTag "DESC." "DESC" entityList)
	(princ))
														 
														 
														 
;|===={ Parts List Block Conversion }=======|;
;| Helper function. Converts a single type  |;
;| of attribute entity with the given info. |;
;| *Returns true if at least one conversion |;
;| happened.* - will do eventually          |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/27/2016 |;
;|==========================================|;

(defun ConvertEntityTag ( oldTag newTag entityList / tagData)
	(foreach entity entitylist
		(if (= (cdr (setq tagData (assoc 2 (entget entity)))) oldTag)
			(entmod (subst (cons 2 newTag)
						tagData
						(entget entity)))))
	(princ))
	
	
	
;|===={ Parts List Block Conversion }=======|;
;| Helper function. Returns a list of the   |;
;| block's attribute entities.              |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/25/2016 |;
;|==========================================|;

(defun GetListOfAttributeEntities (block / attributeEntityList  
													    currentEntity currentEntInfo)
	(setq attributeEntityList nil)
	
	(setq currentEntity (entnext block))
	(setq currentEntInfo (entget currentEntity))
	
	(while (= "ATTRIB" (cdr (assoc 0 currentEntInfo)))
		(setq attributeEntityList 
		   (cons currentEntity attributeEntityList))
		(setq currentEntity (entnext currentEntity))
		(setq currentEntInfo (entget currentEntity)))

	attributeEntityList)
		
		
		
;|===={ Parts List Block Conversion }=======|;
;| Helper function. Returns the block type  |;
;| of "John", "JD", or "Unknown".           |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/20/2016 |;
;|==========================================|;

(defun GetPLBlockType (block / blockType blockAttributes index 
										 currentAttributeName)
	(setq blockAttributes (GetListOfBlockAttributes block))
	
	(setq blockType "Unknown")
	(setq index 0)
	
	(while (and (= blockType "Unknown") (< index (length blockAttributes)))
		(setq currentAttributeName (nth index blockAttributes))
		(cond
			((= currentAttributeName "QTY")
				(setq blockType "JD"))
			
			((or
				(= currentAttributeName "X") 
				(= currentAttributeName "Y")
				(= currentAttributeName "Z")
				(= currentAttributeName "W"))
				(setq blockType "John")))
		(setq index (1+ index))) 
	
	blockType)
	
	
	
;|===={ Parts List Block Conversion }=======|;
;| Helper function. Returns a list of the   |;
;| block's attributes' names.               |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/25/2016 |;
;|==========================================|;

(defun GetListOfBlockAttributes (block)
	(mapcar 
		'(lambda (entity) (cdr (assoc 2 (entget entity))))
		(GetListOfAttributeEntities block)))


	
;|========{ Fillet 0 (Corner Join) }========|;
;| Joins lines to form a corner (fillet w/  |;
;| 0" radius).                              |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/12/2016 |;
;|==========================================|;

(defun C:ff ( / *error* firstLine secondLine)
	
   ; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	; Start UNDO group so the entire process can be easily reversed
	(command "._UNDO" "_Begin")
	
	(while (/= nil (setq firstLine (car (entsel))))
		(setq secondLine (car (entsel)))
		(command "._FILLET" "r" 0)
		(command "._FILLET" firstLine secondLine))
   				  					  
	(command "._UNDO" "_End")		; End UNDO group
	
   (princ))	

	
	
;|============{ Post Placing }==============|;
;| Automatic end post and total dimension   |;
;| placement based on offset distances.     |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 06/27/2016 |;
;|==========================================|;

(defun C:pps (/ *error* cornerPostBlock0 postLayer0 endPostBlock0 placePosts0
					 egdeOffsetDistance0 wallOffsetDistance0 
					 dimLayer0 isCableRailing0 dimOffset0 tagScale0
					 tagLayer0 tagBlock0 tagOffsetDistance0 placeDims0)
	; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
   ; Set block & layer names, & other options
   (setq postLayer0 "Detail")
	(setq endPostBlock0 "BP")
   (setq cornerPostBlock0 endPostBlock0)
	(setq dimLayer0 "Dims")
	(setq isCableRailing0 T)
	(setq tagLayer0 "POST-TAG")
	(setq tagBlock0 "POST-DRILLED CALL-OUT")
	(setq placeDims0 T)
	(setq placePosts0 T)
	
	; Set distance options
   (setq edgeOffsetDistance0 4.5)
   (setq wallOffsetDistance0 4.5)
	(setq tagOffsetDistance0 9)
	(setq tagScale0 1)
	(setq dimOffset0 48)

	; Run dialog box to get user input
	; Warn if dialog fails and exit
	; Define a function based on the input so settings are
	;   saved and can be easily repeated without choices needed each time
	
	(setq setupDCLID (load_dialog "PostPlacementSetup.dcl"))
	
	(if (not (new_dialog "PostPlacementSetup" setupDCLID))
		(princ "\nDialog box not found in file!\n")
		;(exit)
		)
	
	(action_tile "surface"
		"(setq endPostBlock0 \"BP\")
		(setq cornerPostBlock0 endPostBlock0)
		(setq edgeOffsetDistance0 4.5)")
	(action_tile "fascia" 
		"(setq endPostBlock0 \"FB\")
		(setq cornerPostBlock0 \"FBO\")
		(setq edgeOffsetDistance0 (- 0 2.4375))")
	(action_tile "stanchion"
		"(setq endPostBlock0 \"BP\")
		(setq cornerPostBlock0 endPostBlock0)
		(setq edgeOffsetDistance0 6.5)")
	(action_tile "core" 
		"(setq endPostBlock0 \"CORE\")
		(setq cornerPostBlock0 \"CORE\")
		(setq edgeOffsetDistance0 5)
		(setq tagScale0 4)")
	
	(action_tile "detail" "(setq postLayer0 \"Detail\")")
	(action_tile "hral-post" "(setq postLayer0 \"A-HRAL-POST\")")
	
	(action_tile "cable" "(setq isCableRailing0 T)")
	(action_tile "noCable" "(setq isCableRailing0 nil)")
		
	(action_tile "accept" "(done_dialog)(setq userChoice T)")
	(action_tile "cancel" "(done_dialog)(setq userChoice nil)")
	
	(start_dialog)
	(unload_dialog setupDCLID)
	
	(if (not userChoice)
		(exit))
	
	(setq functionToDefine (strcat
		"(defun C:pp (/ *error* cornerPostBlock postLayer snapMode placeDims
						egdeOffsetDistance wallOffsetDistance pointList
						dimLayer isCableRailing dimOffset tagScale placePosts
						tagLayer tagBlock tagOffsetDistance endPostBlock)
		(setq *error* ErrorHandler)
	
		(command \"._UNDO\" \"_Begin\")
		(JD:ClearVars 'systemVariables)
		(JD:Save&ChangeVar \"cmdEcho\" 'systemVariables 0)
		(JD:Save&ChangeVar \"osmode\" 'systemVariables 33)
		(JD:Save&ChangeVar \"attreq\" 'systemVariables 0)
		(JD:Save&ChangeVar \"blipmode\" 'systemVariables 0)
				
		(setq postLayer \"" postLayer0 "\")
		(setq endPostBlock \"" endPostBlock0 "\")
		(setq cornerPostBlock \"" cornerPostBlock0 "\")
		(setq dimLayer \"" dimLayer0 "\")
		(setq isCableRailing " (if isCableRailing0 "T" "nil") ")
		(setq tagLayer \"" tagLayer0 "\")
		(setq tagBlock \"" tagBlock0 "\")
		(setq placeDims " (if placeDims0 "T" "nil") ")
		(setq placePosts " (if placePosts0 "T" "nil") ")
		
		(setq edgeOffsetDistance " (rtos edgeOffsetDistance0 2 4) ")
		(setq wallOffsetDistance " (rtos wallOffsetDistance0 2 4) ")
		(setq tagOffsetDistance " (itoa tagOffsetDistance0) ")
		(setq tagScale " (itoa tagScale0) ")
		(setq dimOffset " (itoa dimOffset0) ")
				
		(setq pointList (GetPointList))
		
		(PlaceMainPosts pointList)
		(if placeDims
			(DimExterior pointList dimOffset))
		
		
		(JD:ResetAllVars 'systemVariables)
		(command \"._UNDO\" \"_End\")
   
		(princ))"))	; End of defun string setq
	
	(eval (read functionToDefine))
	
	(eval (read "(c:pp)"))
	
   (princ))	
	
	
	
; Simple way to allow "pp" to load the setup function ("pps") if not
; already setup yet.

(defun C:pp ()
	(eval (read "(c:pps)")))
	
	

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
	
	(setq postTagPt
		(polar 
			(polar Pt1offset offsetAngle tagOffsetDistance) 
			(- offsetAngle (/ PI 2)) 
			tagOffsetDistance))
	(setq postTagPt (list (car postTagPt) (cadr postTagPt)))
	
	(setq theAngle (angtos lineAngle 0 9))	
   (cond (placePosts
			(JD:Save&ChangeVar "clayer" 'systemVariables postLayer)
			(command "._insert" endPostBlock "s" 1 "r" theAngle Pt1offset)))
	
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
		(if placePosts
			(command "._insert" cornerPostBlock "s" 1 "r" theAngle Pt2offset))
		
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
	
	(setq postTagPt
		(polar
			(polar Pt2offset offsetAngle tagOffsetDistance) 
			(+ offsetAngle (/ PI 2)) 
			tagOffsetDistance))
	(setq postTagPt (list (car postTagPt) (cadr postTagPt)))
	
	(setq theAngle (angtos lineAngle 0 9))
	(setvar "clayer" postLayer)
   (if placePosts
		(command "._insert" endPostBlock "s" 1 "r" theAngle Pt2offset))
	
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
;| Author: J.D. Sandifer    Rev: 06/22/2016 |;
;|==========================================|;


(defun C:DLICont ( / *error* snapMode  dimOffset    systemVariables 
							lastPoint currentPoint angleToOffset   offsetPoint)

	; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
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

(defun DimExterior ( pointList dimOffset / snapMode lastPoint)

	
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
;| Author: J.D. Sandifer    Rev: 06/22/2016 |;
;|==========================================|;


(defun c:DougsDimLabels (/ *error*);( / dimSelSet      index      selSetLength 
							;  dimEntityInfo  dimEntityText)
	; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
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
;| Author: J.D. Sandifer    Rev: 06/22/2016 |;
;|==========================================|;


(defun c:DrawHandrail (/ *error* point0
								 point1     point2    point3 point4)
	; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
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
;| Author: J.D. Sandifer    Rev: 06/22/2016 |;
;|==========================================|;

;; Note: Only works in world/standard UCS.

(defun c:RotateBlock90 (/ *error* block selSet 
								  entityInfo point )
	; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
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
;| Author: J.D. Sandifer    Rev: 06/22/2016 |;
;|------------------------------------------|;
;| Based on: Replace Selected Blocks        |;
;|           (17-IV-2012)                   |;
;|           by Mircea (MSasu)              |;
;|           as posted on CadTutor.net      |;
;|           and accessed on 2016-01-18     |; 
;|==========================================|;


(defun c:ReplaceBlock ( / *error* sourceBlock  sourceBlockName  
								  sourceBlockEntity    targetBlock  
								  targetBlockInfo )
	; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
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
;| Author: J.D. Sandifer    Rev: 06/22/2016 |;
;|------------------------------------------|;
;| Based on: Replace Selected Blocks        |;
;|           (17-IV-2012)                   |;
;|           by Mircea (MSasu)              |;
;|           as posted on CadTutor.net      |;
;|           and accessed on 2016-01-18     |; 
;|==========================================|;


(defun c:ReplaceBlocks ( / *error* sourceBlock  sourceBlockName  
								   sourceBlockEntity    targetBlocks  
									targetBlockInfo  index)
	; Sets the default error handler to a custom one, localization above
	; causes it to be reset after this function finishes
	(setq *error* ErrorHandler)
	
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
        " ::"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;