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
;;  Todo:                                       ;;
;;  - Add top rail counting?                    ;;
;;  - Add choice of post spacing with default.  ;;
;;    Ditto on rail width.                      ;;
;;  - Figure out and correct conflicting        ;;
;;    helper functions. (Combine all into one   ;;
;;    file? Or one helper file?)                ;;
;;  - Revise copy on prompts.                   ;;
;;  - Create list of blocks, layers, etc. on    ;;
;;    which this app relies. (With a check for  ;;
;;    them?)                                    ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:plandraw (/ oldAttributesRequiredSwitch oldSnapMode oldBlipMode
		     oldLayer oldCmdEcho intPostBlock ctrLineLayer
		     postLayer dimLayer dimOffset)
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
   (setq intPostBlock "BP")
   (setq endPlateBlock "END_PLATE")
   (setq ctrLineLayer "Center")
   (setq postLayer "Detail")
   (setq dimLayer "Dims")
   (setq railLayer "1")
   (setq dimOffset "18")		; String means easy concatination later
   (setq railWidth 3.5)

   ;(ResetCutList)
   
   (PlanDrawTool)			; Actual work done in this function

   ; Reset system variables      
   (setvar "attreq" oldAttributesRequiredSwitch)
   (setvar "osmode" oldSnapMode)
   (setvar "blipmode" oldBlipMode)
   (setvar "clayer" oldLayer)
   (setvar "cmdecho" oldCmdEcho)

   (command "._UNDO" "_End")		; End UNDO group
   
   (princ)			; Hide last return value (clean exit)
)




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

(defun PlanDrawTool (/ postSpacing Pt1 Pt2 centerLine
		       spaces lineLength infillLength
		       lineAngle isFirstTime pointList
		       railWidth)

   ; Get user input 
   (setq postSpacing (getreal "\nMax post spacing in inches:"))
   (setq railWidth (getreal "\nRail width in inches:"))
   (setvar "osmode" 32)
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
      ; Got the angle earlies - lineAngle (in radians)
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
  	 ;; Otherwise, just label it "EQ".
	 (if (= dimIndex 1)
	   (progn
	      (setq label (strcat "<>\\X(" (itoa spaces) "X)"))
	      (entmod (subst (cons 1 label) (assoc 1 lastDim) lastDim)))
	      (entmod (subst (cons 1 "EQ") (assoc 1 lastDim) lastDim)))
	 
	 (setq currentPt nextPt)
	 (setq dimIndex (1- dimIndex)))
      
      (setq Pt1 Pt2)		; Makes the 1st point equal the current 2nd point
      (setvar "osmode" 32)      ; Prep for next loop
   )

   (setvar "osmode" 0)
   
   (setvar "clayer" railLayer)
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
   (princ)
   
) ; End function

   


;;; RoundUpTo
;;; Rounds the value up to the next multiple of "multiple" and returns it as an integer.

(defun RoundUpTo (multiple value)
   (if
      (= (rem value multiple) 0)
      ; Then
      (fix value)
      ; Else
      (fix (+ (- value (rem value multiple)) multiple))
   )
)




;;; Assoc+Qty  -  J.D. (based/relies on Assoc++ by Lee Mac)
;;; Adds to the value of a key in an association list if present, else adds key to the list.
;;; key - [any] Key of an element in the list
;;; lst - [lst] Association list (may be nil)
;;; qty - [int] Number of key to add

(defun Assoc+Qty ( key lst qty / itm )
   
    (while (> qty 0)
       (setq lst (assoc++ key lst))
       (setq qty (1- qty))
    )

    lst
)




;; Assoc++  -  Lee Mac
;; Increments the value of a key in an association list if present, else adds key to the list.
;; key - [any] Key of an element in the list
;; lst - [lst] Association list (may be nil)

(defun assoc++ ( key lst / itm )
   
    (if (setq itm (assoc key lst))
        (subst (cons key (1+ (cdr itm))) itm lst)
        (cons  (cons key 1) lst)
    )
   
)




; Assoc--  -  J.D. (based on Assoc++ by Lee Mac)
; Decrements list item by one or removes it if qty is zero
; key - [any] Key of an element in the list
; list - [list] Association list (may be nil)

(defun assoc-- ( key theList / item )
   
    (if (setq item (assoc key theList))
	(progn
	   (setq theList (subst (cons key (1- (cdr item))) item theList))
	   (setq item (assoc key theList))
	   (if (<= (cdr item) 0)
	      (setq theList (vl-remove item theList))
	   )
	   
	)        
    )

    (setq theList theList)
   
)



;;; ResetCutList - Resets global cut list.

(defun ResetCutList ()
   (setq *infillCutList* nil)
   (princ))




;;; OrderList - List ordering function directly from AutoCAD AutoLISP help online
;;; theList - [association list] Sorts in descending (alphanumerical) order by keys. 

(defun OrderList (theList)
   
   (vl-sort
      theList
      (function
	 (lambda
	    (e1 e2)
	    (> (car e1) (car e2) )))))




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

   (eval stockLengthsNeeded)
)
 


;; DisplayCount - Displays the count list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun DisplayCount (result)

   (princ result)
   (princ "\n")
   (princ "\n")
   (princ)
)




;;; Rounding up helper function (legacy support - uses new RoundUpTo function)

(defun RoundUpInt (value)
   (RoundUpTo 1 value)
)




(princ)		; Clean load