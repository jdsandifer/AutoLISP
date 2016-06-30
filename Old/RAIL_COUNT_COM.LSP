;;;;;;;[  Top Rail Count (Commercial)  ];;;;;;;;;;
;;                                              ;;
;;  Does cut list count based on measurements   ;;
;;  of center lines. Prompts for stock length   ;;
;;  and multiplier and stores in global         ;;
;;  variables.                                  ;;
;;    (forked from RAIL_COUNT_SPLIT)            ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 11/17/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;  - MVP best practice - cuts to a list to     ;;
;;    pass to a display function. Where else    ;;
;;    do I need to separate things like that?   ;;
;;  - Re-org everything for easy reading &      ;;
;;    logic, and add comments.                  ;;
;;  - Add post spacing prompt to better figure  ;;
;;    out remainder lengths on chops.           ;;
;;  - Add stair function. And manual mode.      ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Global variables referenced:                ;;
;;    gStockLength       *cutList*              ;;
;;    gPostSpacing       *floorsMultiplier*     ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:railcountcom (/ oldCmdEcho resultList
			   oldSnapMode timeToQuit)		; Will be main UI and system variable control only
   (setq oldCmdEcho (getvar "cmdecho"))
   (setvar "cmdecho" 0)				; Turn off command line output (of certain things)

   ;; Save system variables
   (setq oldSnapMode (getvar "osmode"))
   (setq oldDimzin (getvar "dimzin"))

   
   ;; Set std stock length and single floor if global variable isn't set already
   (if (= gStockLength nil)		
      (setq gStockLength 242))

   (if (= *floorsMultiplier* nil)		
         (setq *floorsMultiplier* 1))

   (if (or (= *ctrLineLayer* nil) (= *ctrLineLayer* ""))		
      (setq *ctrLineLayer* "Center"))
   
   (if (not *option*)
      (setq *option* "Add"))

   (while (/= timeToQuit "True")
   
   (initget "Stock Multiplier Layer Add Count RESET Quit")
   (setq *option*
      (cond
	 ((getkword
	    (strcat "\nChoose cut list option [Stock/Multiplier/Layer/Add/Count/RESET/Quit] <" *option* ">:")))
	 (*option*)))
   
   (cond
      ( (= *option* "RESET") (ResetCutList) (setq *option* "Add") )

      ( (= *option* "Quit") (setq timeToQuit "True") (setq *option* "RESET") )

      ( (= *option* "Stock")
        (setq *option* "Add")
        (initget (+ 2 4))				; Bitcode 6 to prevent 0 or negative value for stock length.
        (setq gStockLength
	   (cond
	      ((getint (strcat "\nStock length (in inches) <" (itoa gStockLength) ">:")))
	      (gStockLength))) )

      ( (= *option* "Multiplier")
        (setq *option* "Add")
        (initget (+ 2 4))				; Bitcode 6 to prevent 0 or negative value for stock length.
        (setq *floorsMultiplier*
	   (cond
	      ((getint (strcat "\nNumber of identical floors (multiplier) <" (itoa *floorsMultiplier*) ">:")))
	      (*floorsMultiplier*))) )

      ( (= *option* "Layer")
        (setq *option* "Add")
        (setq temp (getstring (strcat "\nCenter line layer <" *ctrLineLayer* ">:")))
        (if (/= temp "")
	   (setq *ctrLineLayer* temp)))
      
      ( (= *option* "Add")
        (setq *cutList* (OrderList (append *cutList* (MeasureCenterLines))))
        (princ "\nCutlist: ")
        (DisplayCount *cutList*) )
   
      ( (= *option* "Count")
        (setq *option* "Quit")
	;; Function to chop lengths larger than stock length
        (setq *cutList* (OrderList (ChopLongLengths *cutList* gStockLength)))
        (princ "\nCutlist: ")
        (DisplayCount *cutList*)
   
        ;; Counting for parts list handled in this function
        (setvar "dimzin" 8)
        (setq resultList (CountRails *cutList* gStockLength))
        (princ)
       
        ;; Display handled in this function
        (princ "\nStock lengths: ")
        (DisplayCount resultList) )))

   ;; Restore changed system variables
   (setvar "osmode" oldSnapMode)
   (setvar "dimzin" oldDimzin)
   (setvar "cmdecho" oldCmdEcho)	; Restore command line output to original value
   (princ)				; Hide last return value (clean exit)
)

(princ)				; Clean load




;;; Error handling function - prints error message nicely and resets system variables

(defun *error* (msg)
   (princ "\n")
   (princ msg)

   ;; Reset system variables      
   (setvar "osmode" oldSnapMode)
   (setvar "cmdecho" oldCmdEcho)
   (setvar "dimzin" oldDimzin)

   (princ "\nSystem variables reset")
   (princ "\n")
   (princ))			; Hide last return value (clean exit)




; ResetLengths - Resets global cut list.

(defun ResetCutList ()
   (setq *cutList* nil)
   (princ "Cut list has been reset.")
   (princ))



; GetLengths - Prompts user to measure each rail length and returns the list.
; no arguments - just local variables

(defun GetLengths (/ listOfLengths x cutLength cutQuantity Point1 Point2 oldSnapMode)

   (while (/= (setq cutLength (getint "\nLength of cut (in inches):")) nil)
      ; ark
      (setq listOfLengths (assoc++ cutLength listOfLengths)))
   
   ;Save variable & set to new value
   (setq oldSnapMode (getvar "osmode"))
   (setvar "osmode" 163)

   ;|(while
      (and
	 (/= (setq Point1 (getpoint "\nChoose start of next measurement:")) nil)
         (/= (setq Point2 (getpoint "\nChoose end of measurement:")) nil)
      )
      (princ Point1)
      (princ Point2)
      (princ)
   )|;

   ;Restore variable
   (setvar "osmode" oldSnapMode)

   ;this returns the list
   (OrderList listOfLengths)
)


; MeasureCenterLines
; Created: 10/29/2015 by J.D. Sandifer
; Purpose: Measures lengths of lines on "A-HRAL-CNTR" layer and returns a list of their lengths.
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
      (setq cutList (Assoc+Qty centerLinelength cutList *floorsMultiplier*))
	 ; and add it to cutList
      (setq index (1+ index))
         ; increment counter (very important)
   )
      ; end of while loop

   (OrderList cutlist)
   
)


   
; RoundUpTo
; Rounds the value up to the next multiple of "multiple" and returns it as an integer.

(defun RoundUpTo (multiple value)
   (if
      (= (rem value multiple) 0)
      ; Then
      (fix value)
      ; Else
      (fix (+ (- value (rem value multiple)) multiple))
   )
)


;; ChopLongLengths - Cuts all lengths longer than stock length and adds back parts. 
;; cutList - [association list] The cut list.

(defun ChopLongLengths (cutList stockLength / currentCutIndex currentCutLength
			currentCutQuantity multiplier remainder)

   (princ "\nStock length: ")
   (princ stockLength)
   (princ "\n")

   (setq currentCutIndex 0)

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
	       ; how many stock lengths do we need (per long length)?
	    (princ ", ")
	    (setq remainder (RoundUpTo 2 (rem currentCutLength stockLength)))
	    (princ remainder)
	      ; what's left over after the chop?
	    (setq cutList (vl-remove (assoc currentCutLength cutList) cutList))
	      ; remove the long piece
	    (setq cutList (Assoc+Qty stockLength cutList (* multiplier currentCutQuantity)))
	      ; add the stock lengths
	    (if (< remainder 57)
	       (setq cutList (Assoc+Qty 81 cutList (* multiplier currentCutQuantity)))
	         ; if it's too small, make it long enough (69")
               (setq cutList (Assoc+Qty (+ remainder 24) cutList (* multiplier currentCutQuantity))) ))
	         ; add the remainder lengths (if they're long enough)
          (setq currentCutIndex (1+ currentCutIndex)))
      (princ) )

   cutList)
   

;; OrderList - List ordering function directly from AutoCAD AutoLISP help online
;; theList - [association list] Sorts in descending (alphanumerical) order by keys. 

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
   (setq cutPattern "")
   (setq lastCutPattern "")
   (setq numberOfThisPattern 1)
   
   (princ "\n")
   (princ "\nStock length cuts")
   (princ "\n--------------------")
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
            ;print out the length for this cut
	    (setq cutPattern (strcat cutPattern (itoa currentCutLength)))
	    (setq cutPattern (strcat cutPattern " / "))
            ;decrement cut length quantity (or remove from list) - function
	    (setq cutList (assoc-- currentCutLength cutList))))

      ;;end of cut list
      (cond ((or (>= currentCutIndex (length cutList))(<= stockLengthLeft 0)(<= (length cutList) 0))
         (setq currentCutIndex 0)
	 (setq cutPattern (strcat cutPattern "("))
	 (if (= stockLengthLeft -0.125)
	    (setq cutPattern (strcat cutPattern (itoa 0)))
	    (setq cutPattern (strcat cutPattern (rtos stockLengthLeft 2))))
	 (setq cutPattern (strcat cutPattern ")"))
	 (if (= cutPattern lastCutPattern)
	    (setq numberOfThisPattern (1+ numberOfThisPattern))
	    (progn
	       (if (> numberOfThisPattern 1)
	          (princ (strcat "   x" (itoa numberOfThisPattern))))
	       (princ "\n")
	       (princ cutPattern)
	       (setq numberOfThisPattern 1)))
	 (setq stockLengthLeft 0)
         (setq lastCutPattern cutPattern)
	 (setq cutPattern "") ))
	    

      ;|if length left over
	- stockLength - length left over = odd length
	- decrement stock length qty|;

      ) ;end of while loop
   (princ "\n--------------------")

   stockLengthsNeeded)



;; DisplayCount - Displays the count list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun DisplayCount (result)

   (princ result)
   (princ)
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



;; Assoc+Qty  -  J.D. (based on Assoc++ by Lee Mac)
;; Adds to the value of a key in an association list if present, else adds key to the list.
;; key - [any] Key of an element in the list
;; lst - [lst] Association list (may be nil)
;; qty - [int] Number of key to add

(defun Assoc+Qty ( key lst qty / itm )
   
    (while (> qty 0)
       (setq lst (assoc++ key lst))
       (setq qty (1- qty))
    )

    lst
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
	      (setq theList (vl-remove item theList)) )))
    theList )

