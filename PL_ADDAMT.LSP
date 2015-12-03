;;;;;;;[  Parts List - Add Amount  ];;;;;;;;;;;;;;
;;                                              ;;
;;  User can select a parts list block and      ;;
;;  then enter an amount to add to the count.   ;;
;;  The quantity is increased by that amount.   ;;
;;  (The amount can be negative or 0.)          ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 11/30/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  11/30/2015                                  ;;
;;  - Began initial code writing.               ;;
;;                                              ;;
;;  12/01/2015                                  ;;
;;  - Finished work on initial version.         ;;
;;    replaces the value if qty is the first    ;;
;;    attribute.                                ;;
;;  - Added conversion of attribute value to    ;;
;;    a number for proper addition.             ;;
;;                                              ;;
;;  12/02/2015                                  ;;
;;  - Added ability to add amount to all        ;;
;;    blocks selected.                          ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Need to add a check to verify that the    ;;
;;    the sub-entity is a qty entity before     ;;
;;    trying to add to it (or find it).         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:pladdamt ( / selectedPLBlocks
		      selectedPLBlock              amountToAdd)
   (setvar "cmdecho" 0)			; Turn off command line output

   
   ;; Ask for the new amount
   (initget 1)
   (setq amountToAdd (getint "Enter an amount to add:"))


   ;; Ask for the parts list block(s) to which to add an amount and step through them adding the amount
   (setq selectedPLBlocks (ssget))
   (setq selectedPLBlocks (SSToList selectedPLBlocks))
   (princ "\nbefore foreach")
   (foreach x selectedPLBlocks (AddAmountToQty x amountToAdd))


   (setvar "cmdecho" 1)		; Command line back on
   (princ))			; Clean exit (hide last return value)




;;; AddAmountToQty - Adds the provided amount to the "QTY" field in the provided block.

(defun AddAmountToQty (selectedPLBlock amountToAdd / selectedPLBlockAtt
		                                     selectedPLBlockInfo  amountItem
		                                     currentAmount        newAmount)
   (princ "\naddamounttoqty")
      
   ;; Find the right amount attribute
   
   (setq selectedPLBlockAtt (entnext selectedPLBlock))
   (setq selectedPLBlockInfo (entget selectedPLBlockAtt))                                 
   
   
   ;; Get the current amount
   (setq amountItem (assoc 1 selectedPLBlockInfo))
   (setq currentAmount (cdr amountItem))
   (setq currentAmount (atoi currentAmount))	; convert to number (0 if NAN)
   
   
   ;; Add the amount (already converted to 0 if not a number)
   (setq newAmount (+ currentAmount amountToAdd))
      

   ;; Assign the new value
   (entmod (subst (cons 1 (itoa newAmount)) amountItem selectedPLBlockInfo))
   (princ "\naddamounttoqty done")
   (princ))




;;; SSToList - By Jim Claypool - Converts a selection set to a list.

(defun SSToList (ss / sscnt sslist)
   (princ "\nsstolist")
   (setq sscnt 0
	 sslist nil)
   (repeat (sslength ss)
      (setq sslist (cons (ssname ss sscnt) sslist))
      (setq sscnt (1+ sscnt)))
   (princ "\nsstolist done")
   sslist)




(princ)		; Clean load (no output)


