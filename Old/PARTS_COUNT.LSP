;;;;;;;[  Parts Count  ];;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Counts hardware for parts list .            ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 11/11/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  11/11/2015 - Version 1.0                    ;;
;;  - Calculates and displays hardware, but     ;;
;;    only for items necessary for my current   ;;
;;    drawing.                                  ;;
;;                                              ;;
;;  11/17/2015 - Version 1.5                    ;;
;;  - Calculates and displays hardware, but     ;;
;;    only for items necessary for my recent    ;;
;;    drawings.                                 ;;
;;                                              ;;
;;  12/02/2015 - Version 1.6 (Demo)             ;;
;;  - Commented out questions for unsupported   ;;
;;    items.                                    ;;
;;  - Added apply values to parts list demo.    ;;
;;                                              ;;
;;  12/03/2015 - Version 1.7                    ;;
;;  - Turned prompts into one mapcar/lambda     ;;
;;    with lists for variables and prompts.     ;;
;;                                              ;;
;;  01/20/2016 - Version 2.0                    ;;
;;  - Added menu for rail info, counting,       ;;
;;    calculating, inputing, and                ;;
;;    displaying parts.                         ;;
;;                                              ;;
;;  01/23/2016 - Version 2.0 (no change)        ;;
;;  - Added menu for rail info, counting,       ;;
;;    calculating, inputing, and                ;;
;;    displaying parts.                         ;;
;;                                              ;;
;;  01/26/2016 - Version 3.0                    ;;
;;  - Started major re-vamp                     ;;
;;  - Added menu for all counting and plan to   ;;
;;    include filling in blocks as part of      ;;
;;    each calc/counting function.              ;;
;;  - Menu tests ok.                            ;;
;;  - Separated system variable setting &       ;;
;;    re-setting into functions, but that       ;;
;;    leaves variables globalized or I have to  ;;
;;    pass around a lot of variables - to the   ;;
;;    error function, too. Better option?       ;;
;;                                              ;;
;;  01/28/2016 - Version 3.1                    ;;
;;  - CountPosts correctly counts posts and     ;;
;;    gets an assoc list for cable and an       ;;
;;    integer for glass/picket.                 ;;
;;  - CountPosts fills in the glass/picket      ;;
;;    count (albeit in the wrong space).        ;;
;;                                              ;;
;;  01/29/2016 - Version 3.2                    ;;
;;  - Count&FillPostBlocks counts each post     ;;
;;    type, calculates related hardware, and    ;;
;;    puts the counts in the correct blocks     ;;
;;    (more or less). Renamed from CountPosts.  ;;
;;  - Calculate&FillHardware adds "x" items if  ;;
;;    needed.                                   ;;
;;                                              ;;
;;  01/31/2016 - Version 3.3                    ;;
;;  - Added automatic general info gathering.   ;;
;;                                              ;;
;;  02/01/2016 - Version 3.4                    ;;
;;  - Changed hash calls to new names.          ;;
;;  - Removed some old code that's been         ;;
;;    replaced.                                 ;;
;;                                              ;;
;;  02/15/2016 - Version 3.5                    ;;
;;  - Added infill counting and filling.        ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - "AutoFill" variable to choose if the app  ;;
;;    fills in blocks as it gets quantities.    ;;
;;    Could also choose other options: add a    ;;
;;    text element with values, print to        ;;
;;    command line, dialog box, etc.            ;;
;;  - Test infill counting.                     ;;
;;  - Remove hard coding of block names in      ;;
;;    CountPosts.                               ;;
;;  - Fix pl block fill-in - correct space,     ;;
;;    label filling (multiple functions?)       ;;
;;  - Better option for system variable         ;;
;;    setting and restoring. Functions?         ;;
;;  - Add calcs for all hardware.               ;;
;;  - More versatile block fill-in function     ;;
;;    (PLSetAmt) or multiple functions.         ;;
;;    Also, check for the block in the dwg.     ;;
;;  - Revise CountPosts to deal with first      ;;
;;    for cable railing.                        ;;
;;  - Revise CountPosts to change correct size  ;;
;;    lag bolts by railing height.              ;;
;;  - Write Count&FillFasciaBlocks.             ;;
;;  - Add conditions to Calculate&FillHardware  ;;
;;    for wood adapter, 100, and 500.           ;;
;;  - Add stanchion support.                    ;;
;;  - Add Wood Adapter top rail support.        ;;
;;    (GeneralInfo)                             ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:PartsCount (/ blockNameDatabase systemVariables timeToQuit)	

	; Start UNDO group so the entire process can be easily reversed
	(command "._UNDO" "_Begin")
	(JD:ClearVars 'systemVariables)
	(Save&ChangeSystemVariables)
	(SetupBlockNameDatabase)
		
	;; Main menu
	;; First, setup the variables
	
   (if (not *option*)
      (setq *option* "Info"))
	;(setq postQuantity nil) ; until they're local variables
	;(setq endPlateQuantity nil) ; until they're local variables
	
	; get top rail & fill-in parts list
	; get infill/bottom rail & fill-in parts list
	; base plates & fill-in parts list
	; calculate & fill-in parts list extra hardware	
	; quit
		
   (while (/= timeToQuit "True")
		(initget "Info Posts Toprail inFill Hardware Quit")
		(setq *option*
			(cond
				((getkword
					(strcat "\nChoose menu option "
							  "[Info/Posts/Toprail/inFill/Hardware/Quit] "
							  "<" *option* ">:") ))
				(*option*) ))
		
		(if (= *option* "inFill")
			(setq *option* "Infill"))
   
		(cond
			(  (= *option* "Quit")
				(setq timeToQuit "True")
				(setq *option* "Info"))
		
			(  (= *option* "Info")
				(setq *option* "Posts")
				(GetGeneralInfo))

			(	(= *option* "Posts")
				(setq *option* "Toprail")
				(setq postQuantity (Count&FillPostBlocks)))

			(  (= *option* "Toprail")
				(setq *option* "Infill")
				(Count&FillToprail))

			( 	(= *option* "Infill")
				(setq *option* "Hardware")
				(Count&FillInfill))
		
			( 	(= *option* "Hardware")
				(setq *option* "Quit")
				(cond
					(  (and (/= postQuantity nil)	(/= endPlateQuantity nil) )
						(Calculate&FillHardware postQuantity endPlateQuantity)  )
						
					(  (or (= postQuantity nil) (= endPlateQuantity nil) )
						(princ "Don't have post and end plate counts yet...")
						)))))
		 			
   (ResetSystemVariables)
	; End UNDO group
	(command "._UNDO" "_End")
	
   (princ))				; Clean exit (hide last return value)

		
	
;;; Sets up the big database of items and their parts list blocks

(defun SetupBlockNameDatabase ()
   
	;; Create the giant block name database (association list) for use
   (setq blockNameDatabase '(("PostCable2" . "32-20-BLOCK-PRE-DRILLED-2")
										 ("PostCable1" . "FILLER-1-PRE-DRILLED")
										 ("Post" . "32-20-BLOCK-1")))
	(princ "Block name database is setup.")
	
	(princ))
	
	

;;; Save current system variables and then change them

(defun Save&ChangeSystemVariables ()

   (setq oldCmdEcho (getvar "cmdecho"))
	(setq oldAttributesRequiredSwitch (getvar "attreq"))
	
	; Save old error handler and transfer calls to the custom one
	(setq oldError *error*		
		*error* ErrorHandler)

   (princ "\nSystem variables saved")
   (princ "\n")
	
	; Turn off command printing to command line
	(setvar "cmdecho" 0)
	;(setvar "attreq" 0)
		
	(princ))



;;; Resets system variables

(defun ResetSystemVariables ()

   (setvar "attreq" oldAttributesRequiredSwitch)
   (setvar "cmdecho" oldCmdEcho)
	(setq *error* oldError)

   (princ "\nSystem variables reset")
   (princ "\n")
	
	(princ))


	
;;; Error handling function - prints error message nicely and resets system variables

(defun ErrorHandler (msg)
   (princ "\nThere's a slight problem: ")
   (princ msg)

   (ResetSystemVariables)
	
	(command-s "._UNDO" "_End")		; End UNDO group
   ;(command "._U")			; Undo whatever got done so far
   
   (princ))
		
	
	
			
;;; Gets basic railing info from title block and stores it
;;; in a global hash

(defun GetGeneralInfo ( / temp)


	(JD:ClearHash '*railingInfo*)
	
	;; Get title block entity
	(setvar 'ctab (car (layoutlist)))
	
   (setq blockSelection (ssget "x" (list '(0 . "INSERT")
			       (cons 2 "HAS D size Title Block-New-Commercial"))))
	(setq blockEntity (ssname blockSelection 0))
   (setq blockAttributes (entget blockEntity))
	
	(while (/= (cdr (assoc 0 blockAttributes)) "SEQEND")
		(cond
			(  (= (cdr (assoc 2 blockAttributes)) "HEIGHT-PROFILE-LEVEL")
				(setq temp (cdr (assoc 1 blockAttributes)))

				(JD:PutHash "Height" (substr temp 1 2) '*railingInfo*)
				
				(JD:PutHash "Toprail" (substr temp 14) '*railingInfo*))
				
			(  (= (cdr (assoc 2 blockAttributes)) "MOUNTING")
				(setq temp (substr (cdr (assoc 1 blockAttributes)) 1 4))
				
				(cond
					(  (= temp "SURF")
						(setq temp "Surface"))
						
					(  (= temp "FASC")
						(setq temp "Fascia"))
						
					(  (= temp "CORE")
						(setq temp "Core")))
				
				(JD:PutHash "Mounting" temp '*railingInfo*))
								
			(  (= (cdr (assoc 2 blockAttributes)) "INFILL-TYPE")
				(setq temp (cdr (assoc 1 blockAttributes)))
				(setq temp (strcase temp T))
				(setq firstLetterCap (chr (- (ascii temp) 32)))
				(setq temp (strcat firstLetterCap (substr temp 2)))
				(JD:PutHash "Infill" temp '*railingInfo*)))
		
		(setq blockEntity (entnext blockEntity))
		(setq blockAttributes (entget blockEntity)))
	
	(setvar 'ctab "Model")
	
   (princ))



	
;;; Counts posts in drawing and fills in parts list blocks if present

(defun Count&FillPostBlocks ( / infillType postHeight postQuantity)
	; post count - pco_count & fill-in parts list
	;; Decide how to count posts based on railing info
	
	(setq infillType (JD:GetHash "Infill" '*railingInfo*))
	(setq postHeight (+ 6 (atoi (JD:GetHash "Height" '*railingInfo*))))
	(setq postQuantity 0)
	
	;prep for post counting function
	(setq postCallOut "POST-DRILLED CALL-OUT")
   (setq postCallOut2 "PICKET PANEL CALL-OUT")
	
	(cond
		(  (or (= infillType "Cable") (= infillType "Spreaders"))
			(setq postCount (CountPostCallOuts))
			(foreach postType postCount
				(cond 
					(  (= (car postType) "A")
						(ChangeBlockQty "32-20-BLOCK-PRE-DRILLED-2"
							(cdr postType))
						(ChangeBlockAtt "32-20-BLOCK-PRE-DRILLED-2"
							"LEN" postHeight))
						
					(  (= (car postType) "B")
						(ChangeBlockAtt "32-20-BLOCK-PRE-DRILLED-2"
							"QTY2"
							(cdr postType))
						(ChangeBlockAtt "32-20-BLOCK-PRE-DRILLED-2"
							"LEN2" postHeight))
							
					(  (= (car postType) "C")
						(ChangeBlockQty "FILLER-1-PRE-DRILLED"
							(cdr postType))
						(ChangeBlockAtt "FILLER-1-PRE-DRILLED" "LEN" postHeight)))
							
				(setq postQuantity (+ postQuantity (cdr postType)))  ))
		
		(  (or (= infillType "Glass") (= infillType "Picket"))
			(setq postQuantity (CountBasicPosts))
			(ChangeBlockQty "32-20-BLOCK-1" postQuantity)
			(ChangeBlockAtt "32-20-BLOCK-1" "LEN" postHeight)
			))
			
	(setq mountingType (JD:GetHash "Mounting" '*railingInfo*))
			
	(cond
		(  (= mountingType "Core")  
			; do nothing - no mounting parts...
			)
		(  (= mountingType "Stanchion")  
			; is there stuff to do here? maybe...
			)
		(  (= mountingType "Surface")  
			(ChangeBlockQty "04-02-BLOCK-1" postQuantity)
			(ChangeBlockQty "38-06-BLOCK-1" postQuantity)
			(ChangeBlockQty "16-54-BLOCK-1" (* 4 postQuantity))
			(ChangeBlockQty "16-08-BLOCK-1" (* 6 postQuantity))
			(ChangeBlockQty "16-26-BLOCK-1" (* 4 postQuantity))
			; figure out what color it is and fill correct button cap qty
			)
		(  (= mountingType "Fascia")
			;(Count&FillFasciaBlocks)
			(ChangeBlockQty "16-54-BLOCK-1" (* 4 postQuantity))
			(ChangeBlockQty "16-49-BLOCK-1" (* 2 postQuantity))
			(ChangeBlockQty "16-25-BLOCK-1" (* 2 postQuantity))
			(ChangeBlockQty "16-26-BLOCK-1" (* 4 postQuantity))
			; figure out what color it is and fill correct button cap qty
			))
			
	(princ "Posts counted and parts list blocks filled.")
	postQuantity) 
		  
		  
		  
		  
;;; Counts toprail lengths and fills in parts list

(defun Count&FillToprail ( / topRailParts railType)
	(setq topRailParts (RailCountSub))
	
	(setq railType (JD:GetHash "Toprail" '*railingInfo*))
	(cond
		(	(= railType "200X")
			(ChangeBlockQty "36-03-BLOCK-1" topRailParts))
			
		(	(= railType "300X")
			(ChangeBlockQty "36-06-BLOCK-1" topRailParts))
			
		(	(= railType "Wood")
			(ChangeBlockQty "36-15-BLOCK-1" topRailParts)))
	
	(princ "Top rail block filled.")
	(princ))		  
		  
		  
		  
;;; Counts infill lengths and fills in parts list (from infill cut list)

(defun Count&FillInfill ( / infillParts)
	;; Counting for parts list handled in this function
	(setq infillParts (CountRails *infillCutList* 180))
							
	;; Display handled in these functions
	(PrintCutList *fullCutList*)
	(princ "Infill stock lengths needed: ")
	(DisplayCount infillParts)
	
	(ChangeBlockQty "20-02-BLOCK-1" infillParts)
	(princ "Infill & bottom rail blocks filled.")
	(princ))

	
;;; Calculates hardware and fills in parts list blocks if present

(defun Calculate&FillHardware (postQuantity endPlateQuantity / toprailType)
				
	(setq toprailType (JD:GetHash "Toprail" '*railingInfo*))
			
	;; Check for "X" type railing to add X brackets and truss screws
	(cond
		(  (or   (= toprailType "200X")
					(= toprailType "300X")
					(= toprailType "320X")
					(= toprailType "350X"))
			; Calculate total number of x brackets needed
			(setq totalXBrackets
					(- (* 2 postQuantity) endPlateQuantity)  )
			; Need half that are left brackets (rounded up)
			(setq leftXBrackets (RoundUpBy 1 (/ totalXBrackets 2)))
			(ChangeBlockQty "08-01-BLOCK-1" leftXBrackets)
			; and half that are right brackets (rounded down)
			(setq rightXBrackets (fix (/ totalXBrackets 2)))
			(ChangeBlockQty "08-02-BLOCK-1" rightXBrackets)
			;; Twice as many gen. use and truss screws as x brackets
			(ChangeBlockQty "16-12-BLOCK-1" (* 2 totalXBrackets))
			(ChangeBlockQty "16-14-BLOCK-1" (* 2 totalXBrackets))))
	(cond
		(	(/= toprailType "Wood")
			(ChangeBlockQty "14-02-BLOCK" endPlateQuantity)
			(ChangeBlockQty "16-07-BLOCK-1" (* 2 endPlateQuantity))))
	(princ "Hardware counted and blocks filled.\n"))
		  
		  
		  
		  
;;; Changes the given attribute of the given block
;;; blockToChange [string] - name of block
;;; attribute [string] - name of attribute to change
;;; newValue [integer or string] - value to insert 				
				
(defun ChangeBlockAtt ( blockToChange   attribute newValue /
								blockSelection  blockEntity
								blockAttributes quantityItem
								isDone )
   ;; Find the block by name
   (setq blockSelection (ssget "x" (list '(0 . "INSERT")
			       (cons 2 blockToChange))))
	(setq blockEntity (ssname blockSelection 0))
   (setq blockAttributes (entget blockEntity))
	
	(while (and (/= (cdr (assoc 0 blockAttributes)) "SEQEND")
					(/= isDone "True"))
		(cond
			(  (= (cdr (assoc 2 blockAttributes)) attribute)
				(setq quantityItem (assoc 1 blockAttributes))
   
				;; Assign the new value
				(if (= (type newValue) 'INT)
					(setq newValue (itoa newValue)))
				(entmod (subst (cons 1 newValue)
						quantityItem
						blockAttributes))
				(setq isDone "True"))
			(  (/= (cdr (assoc 2 blockAttributes)) attribute)
				(setq blockEntity (entnext blockEntity))
				(setq blockAttributes (entget blockEntity)))))
				   
   (princ)  )
		
		
		
		
;;; Changes the quantity of the given block
;;; blockToChange [string] - name of block
;;; newQuantity [integer > 0] - quantity to insert 				
				
(defun ChangeBlockQty ( blockToChange newQuantity / )
	(ChangeBlockAtt blockToChange "QTY" newQuantity)
   ;| Superfluous when then function above works
   (setq blockSelection (ssget "x" (list '(0 . "INSERT")
			       (cons 2 blockToChange))))
	(setq blockEntity (ssname blockSelection 0))
   (setq blockAttributes (entget blockEntity))
	
	(while (and (/= (cdr (assoc 0 blockAttributes)) "SEQEND")
					(/= isDone "True"))
		(cond
			(  (= (cdr (assoc 2 blockAttributes)) "QTY")
				(setq quantityItem (assoc 1 blockAttributes))
   
				;; Assign the new value
				(entmod (subst (cons 1 (itoa newQuantity))
						quantityItem
						blockAttributes))
				(setq isDone "True"))
			(  (/= (cdr (assoc 2 blockAttributes)) "QTY")
				(setq blockEntity (entnext blockEntity))
				(setq blockAttributes (entget blockEntity)))))|;
				   
   (princ)  )
		
		
		
		
;;; CalculateHardware - Calculates the hardware list and displays it
;;; on the command line

(defun CalculateHardware ()
   
   (princ "\n")
   (princ "Post Caps: ")(princ postCaps)

   (princ "\n")
   (princ "RCBs (with midrail): ")
   (princ (setq RCBs(- (* 2 posts) endPlates)))
   (princ (strcat " (" (itoa (* 2 RCBs)) ")"))

   (princ "\n")
   (princ "Splice Plates: ")(princ splicePlates)
   (PLSetAmt "08-18-BLOCK-1" splicePlates)

   (princ "\n")
   (princ "End plate screws: ")(princ (* 2 (+ endPlates endPlates4Hole)))

   (princ "\n")
   (princ "RCB Screws (with midrail): ")
   (princ (+ (* 4 RCBs) (* 2 endPlates4Hole)))
   (princ (strcat " (" (itoa (+ (* 4 RCBs) (* 4 endPlates4Hole))) ")"))

   
   (princ "\n")
   (princ "General use screws (with midrail): ")
   (princ (+
  	     (* 4 splicePlates)
	     (* 2 postCaps)
	     (* 2 RCBs)
	     (* 2 (- (* 2 posts) endPlates))))
   (princ " (")
   (princ (+
  	     (* 4 splicePlates)
	     (* 2 postCaps)
	     (* 4 RCBs)
	     (* 2 (- (* 2 posts) endPlates))))
   (princ ")")

   
   (princ "\n")
   (princ "X Bracket screws: ")(princ (* 2 (- (* 2 posts) endPlates)))

   (princ "\n")
   (princ "3/8\" button cap washers: ")(princ (* 4 posts))

   (princ "\n")
   (princ "Button caps: ")(princ (* 4 posts)))
	
	
	
	
;;; Gets basic railing info and stores it in a global hash

(defun GetGeneralInfoFromUser ( / temp)


	(JD:ClearHash '*railingInfo*)
	
	;; Get railing height and add it to the hash table
	(initget 1 "36 42")
	(setq temp (getkword (strcat "\nChoose railing height "
										  "[36/42]:")))
	(JD:PutHash "Height" temp '*railingInfo*)
	
	;; Get mounting type and add it to the hash table
	(initget 1 "Fascia Surface Stanchion Core")
	(setq temp (getkword (strcat "\nChoose mounting type "
										  "[Fascia/Surface/Stanchion/Core]:")))
	(JD:PutHash "Mounting" temp '*railingInfo*)
		
	;; Get infill type and add it to the hash table
	(initget 1 "Cable Glass Picket Spreaders")
	(setq temp (getkword (strcat "\nChoose infill type "
										  "[Cable/Glass/Picket/Spreaders]:")))
	(JD:PutHash "Infill" temp '*railingInfo*)
	
	;; Get toprail type and add it to the hash table
	(initget 1 "100 200 200X 300 300X 320X 350X 500 Wood")
	(setq temp (getkword (strcat "\nChoose top rail type "
										  "[100/200/200X/300/300X/320X" 
										  "/350X/500/Wood]:")))
	(JD:PutHash "Toprail" temp '*railingInfo*)	
	
   
   (princ) )



(princ)	 				; Clean file load (hide last return value)




