
;;;;;;;[  Post Call Out Counting  ];;;;;;;;;;;;;;;
;;                                              ;;
;;  Counts post call-outs within a selected     ;;
;;  area based on 2 hard-coded block names.     ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 10/28/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  01/19/2016 - JD                             ;;
;;  - Removed shared helper functions.          ;;
;;                                              ;;
;;  12/02/2015 - JD                             ;;
;;  - Added back "POST-DRILLED CALL-OUT" for    ;;
;;    residential demo.                         ;;
;;                                              ;;
;;  11/17/2015                                  ;;
;;  - Added option to choose block names.       ;;
;;                                              ;;
;;  11/09/2015                                  ;;
;;  - Fixed ssget to use postCallOut            ;;
;;    correctly.                                ;;
;;  - Changed ssget to use two block names -    ;;
;;    postCallOut & postCallOut2.               ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Add options for names of blocks to        ;;
;;    count?                                    ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun c:pcocount (/ resultList)	; Define function (inc. local variables)
   (setvar "cmdecho" 0)			; Turn off command line output

   ; Set block & layer names
   (setq postCallOut "POST-DRILLED CALL-OUT")
   (setq postCallOut2 "PICKET PANEL CALL-OUT")

   ; Counting handled in this function
   (setq resultList (CountPostCallOuts))	
   (princ "Counting...")

   ;Ordering the list in this function
   (setq resultList (OrderListAscending resultList))
   (princ "ordering list...\n")

   ; Display handled in this function
   (DisplayCount resultList)		
   (princ "\nComplete.")

   
   (setvar "cmdecho" 1)		; Command line back on
   (princ)	)		; Hide last return value (clean exit)




;; CountPostCallOuts - Counts post call-out blocks within a user selection and returns an association list of (labels . quantities).
;; no arguments - just local variables

(defun CountPostCallOuts (/ postList i blockSelSet key)
   (setq blockSelSet (ssget (list '(0 . "INSERT")
			       '(-4 . "<or")
			       (cons 2 postCallOut)
			       (cons 2 postCallOut2)
			       '(-4 . "or>"))))
   (setq i 0)
   (while (< i (sslength blockSelSet))
      (progn
			(setq key  (cdr (assoc 1  (entget (entnext (ssname blockSelSet i))) )) ) 
         (setq postList (assoc++ key postList))
			(setq i (1+ i)) ))
   ;this returns the list
   (setq i postList) )

	


;; DisplayCount - Displays the count list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

;(defun DisplayCount (result)
;   (princ "\n=#===Qty=\n")
;   (foreach item result
;      (progn
;			(princ " ")
;			(princ (car item))
;			(princ " . ")
;			(princ (cdr item))
;			(princ "\n") ))
;   (princ "=========\n") )

	
	

(princ)				; Clean load

