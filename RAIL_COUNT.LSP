; Top Rail Counting                                  
; Does cut list count based on measurements of center lines.
; Prompts for stock length and stores in global variable.               
;
; Global variables referenced: gStockLength *cutList* gPostSpacing
;
; Author: J.D. Sandifer                              
; Written: 10/19/2015                                
;
; Rev: 10/28/2015                                    
; Comments: Working counting algorithm!!!
;
; Rev: 10/30/2015                                    
; Comments: Added Center Line counting function so counting is mostly automated.
;
; Rev: 11/03/2015
; Comments: Added function to chop long lengths in cut list. No more infinite loop in that case...
;
; Rev: 11/08/2015
; Comments: Added menu system.
;
; Rev: 11/09/2015
; Comments: Revised menu system so RESET and Quit are not kept as default options.
;
; Rev: 11/10/2015
; Comments: Added error handling function.
;
; Todo:
;       Re-org everything for easy reading & logic, and add comments.
;       Add stair function.
;       Add case to catch too long lengths in calc function.
;       Refine chop function calc.



(defun c:railcount (/ oldCmdEcho resultList
		      oldSnapMode)		; Will be main UI and system variable control only
   (setq oldCmdEcho (getvar "cmdecho"))
   (setvar "cmdecho" 0)				; Turn off command line output (of certain things)

   ;Save variable
   (setq oldSnapMode (getvar "osmode"))

   
   ;; Ask for stock length if global variable isn't set already
   (initget (+ 1 2 4))				; Bitcode 7 to prevent 0 or negative value (or ENTER) for stock length.
   (if (= gStockLength nil)		
      (setq gStockLength (getint "\nStock length (in inches):")) )

   (if (not *option*)
      (setq *option* "Add"))
   
   (initget "Add RESET Quit")
   (setq *option*
      (cond
	 ((getkword
	    (strcat "\nChoose cut list option [Add/RESET/Quit] <" *option* ">:")))
	 (*option*)))
   
   (cond
      ( (= *option* "RESET") (ResetCutList) (setq *option* "Add") )

      ( (= *option* "Quit") (setq *option* "Add") )
      
      ( (= *option* "Add")
        (progn
	  (setq *cutList* (OrderList (append *cutList* (MeasureCenterLines))))
          (princ "\nCutlist: ")
          (DisplayCount *cutList*)
   
          ;; Function to chop lengths larger than stock length
          (setq *cutList* (OrderList (ChopLongLengths *cutList* gStockLength)))
   
          ;; Counting for parts list handled in this function
          (setq resultList (CountRails *cutList* gStockLength))
          (princ) )))

	  
   ;; Display handled in this function
   (princ "\nStock lengths: ")
   (DisplayCount resultList)
  
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

   (princ "\nSystem variables reset")
   (princ "\n")
   (princ))			; Hide last return value (clean exit)




; ResetLengths - Resets global cut list.

(defun ResetCutList ()
   (setq *cutList* nil)
   (princ))



; GetLengths - Prompts user to measure each rail length and returns the list.
; no arguments - just local variables

(defun GetLengths (/ listOfLengths x cutLength cutQuantity Point1 Point2 oldSnapMode)

   (while (/= (setq cutLength (getint "\nLength of cut (in inches):")) nil)
      (setq listOfLengths (assoc++ cutLength listOfLengths))
   )
   
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
; Purpose: Measures lengths of lines on "Center" layer and returns a list of their lengths.
; Input: User selects area contaning the lines.
; Returns: A list of lengths.
; Revision History:
; 10/30/15 - Measuring works! (for one line)

(defun MeasureCenterLines (/ buffer selSet centerline
			     centerLineInfo centerLinelength cutList)

   (setq buffer 6)
   (setq selSet (ssget '((8 . "Center") (0 . "line")) ))
      ; have the user select an area that includes the lines to measure
      ; and filter in only lines on "Center" layer (not case-sensitive)
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
      (setq centerLineLength (RoundUpTo 6 centerLineLength))
         ; round up to the nearest 6"
      (setq cutList (Assoc++ centerLinelength cutList))
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
	    (setq remainder (rem currentCutLength stockLength))
	    (princ remainder)
	      ; what's left over after the chop?
	    (setq cutList (vl-remove (assoc currentCutLength cutList) cutList))		; WARNING!!! Might remove multiple entries
	      ; remove the long piece
	    (setq cutList (Assoc+Qty stocklength cutList (* multiplier currentCutQuantity)))
	      ; add the stock lengths
	    (if (< remainder 57)
	       (setq cutList (Assoc+Qty 81 cutList currentCutQuantity))
	         ; if it's too small, make it long enough (81")
               (setq cutList (Assoc+Qty (+ remainder 24) cutList currentCutQuantity)) )))
	         ; add the remainder (+ 24") lengths (if they're long enough)

      (setq currentCutIndex (1+ currentCutIndex))
      (princ) )

   cutList

)
   

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

   stockLengthsNeeded
)



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
	      (setq theList (vl-remove item theList))
	   )
	   
	)        
    )

    (setq theList theList)
   
)



; GetElementAtIndex  -  J.D. Sandifer
; Retrieves the element at the specified index.
; [index] - index (int) at which to find element
; [aList] - the list to search

