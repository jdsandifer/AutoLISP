;;;;;;;[  Parts Count  ];;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Counts most railing hardware based on       ;;
;;  simple text input of key values.            ;;
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
;;    (Only five items so far - no checks)      ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Add calcs for all hardware.               ;;
;;  - Add baseplate/fascia choice.              ;;
;;    (Or always output both possibilities.)    ;;
;;  - Convert display lines to helper           ;;
;;    function calls.                           ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:partscount (/ oldCmdEcho)	
   (setq oldCmdEcho (getvar "cmdecho"))
   (setvar "cmdecho" 0)			; Turn off command printing to command line

   (initget 7)				; Require positive number (and not ENTER) at next prompt
   (setq posts (getint "How many posts (total)?: "))

   (initget 7)				; Require positive number (and not ENTER) at next prompt
   (setq endPlates (getint "How many end plates (std)?: "))

   (initget 5)				; Require positive number (and not ENTER) at next prompt
   (setq endPlates4Hole (getint "How many end plates (4-hole)?: "))

   (initget 5)				; Require positive number or zero (but not ENTER) at next prompt
   (setq postCaps (getint "How many post caps?: "))

   (initget 5)				; Require positive number or zero (but not ENTER) at next prompt
   (setq splicePlates (* 2 (getint "How many top rail splices?: ")))

   ;|(initget 5)				; Require positive number or zero (but not ENTER) at next prompt
   (setq grabSplices (getint "How many grab rail splices?: "))

   (initget 5)				; Require positive number or zero (but not ENTER) at next prompt
   (setq grabBrackets (getint "How many grab rail brackets?: "))|;
	 
   (CalculateHardware)			; Calculates the hardware list and displays on the command line
  
   (setvar "cmdecho" oldCmdEcho)
   (princ))				; Clean exit (hide last return value)




;;; CalculateHardware - Calculates the hardware list and displays on the command line

(defun CalculateHardware ()
   (princ "\n")
   (princ "\nHardware List")
   (princ "\n--------------------")
   (princ "\nTotal Posts: ")(princ posts)

   (princ "\n")
   (princ "Base Plates: ")(princ posts)
   (PLSetAmt "04-02-BLOCK-1" posts)

   (princ "\n")
   (princ "End Plates (std): ")(princ endPlates)
   (PLSetAmt "14-02-BLOCK" endPlates)

   (princ "\n")
   (princ "End Plates (4-hole): ")(princ endPlates4Hole)

   (princ "\n")
   (princ "Post Caps: ")(princ postCaps)

   (princ "\n")
   (princ "X Brackets (Left): ")(princ (RoundUpBy 1 (/ (- (* 2 posts) endPlates) 2)))
   (PLSetAmt "08-01-BLOCK-1" (RoundUpBy 1 (/ (- (* 2 posts) endPlates) 2)))

   (princ "\n")
   (princ "X Brackets (Right): ")(princ (fix (/ (- (* 2 posts) endPlates) 2)))
   (PLSetAmt "08-02-BLOCK-1" (fix (/ (- (* 2 posts) endPlates) 2)))

   (princ "\n")
   (princ "RCBs (with midrail): ")
   (princ (setq RCBs(- (* 2 posts) endPlates)))
   (princ (strcat " (" (itoa (* 2 RCBs)) ")"))

   (princ "\n")
   (princ "Splice Plates: ")(princ splicePlates)
   (PLSetAmt "08-18-BLOCK-1" splicePlates)

   (princ "\n")
   (princ "Neoprene Gaskets: ")(princ posts)
   (PLSetAmt "38-06-BLOCK-1" posts)

   (princ "\n")
   (princ "Anchors or lag screws: ")(princ (* 4 posts))

   (princ "\n")
   (princ "End plate screws: ")(princ (* 2 (+ endPlates endPlates4Hole)))

   (princ "\n")
   (princ "Base plate screws: ")(princ (* 6 posts))

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




;;; RoundUpBy
;;; Rounds the value up to the next multiple of "multiple" and returns it as an integer.

(defun RoundUpBy (multiple value)
   (if
      (= (rem value multiple) 0)
      ; Then
      (fix value)
      ; Else
      (fix (+ (- value (rem value multiple)) multiple))
   )
)



(defun PLSetAmt ( blk amountToBe / )
   (setvar "cmdecho" 0)			; Turn off command line output


   ;; Get the parts list block we want to change (ename)
   (setq selectedPLBlocks (ssget "x" (list '(0 . "INSERT")
			       (cons 2 blk))))
   (setq selectedPLBlock (ssname selectedPLBlocks 0))
   
   
   ;; Get the attribute property to modify
   (setq selectedPLBlockAtt (entnext selectedPLBlock))
   (setq selectedPLBlockInfo (entget selectedPLBlockAtt))
   (setq amountItem (assoc 1 selectedPLBlockInfo))
   
   
   ;; Assign the new value
   (entmod (subst (cons 1 (itoa amountToBe)) amountItem selectedPLBlockInfo))
   

   
   (setvar "cmdecho" 1)		; Command line back on
   (princ))



(princ)	 				; Clean load (hide last return value)




