
;;;;;;;[  Post & Cable Counting  ];;;:;;;;;;;;;;;;
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
;;  04/12/2016 - JD                             ;;
;;  - Added cable tag text counting.            ;;
;;  - Both methods are in the main function     ;;
;;    right now. Comment the new one out if     ;;
;;    desired.                                  ;;
;;                                              ;;
;;  04/05/2016 - JD                             ;;
;;  - Added cable tag counting (new block).     ;;
;;  - Changed name of print function so it      ;;
;;    could be used to neaten the results.      ;;
;;  - Moved sorting into the count function     ;;
;;    (for now). May move out later.            ;;
;;  - Added a multiplier for cable runs to the  ;;
;;    print function (for now).                 ;;
;;  - Added local variables to list.            ;;
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



(defun c:countposts (/ resultList postCallOut postCallOut2 postCallOut3
							cableMultiplier layer1)
   (setvar "cmdecho" 0)			; Turn off command line output

   ; Set block & layer names
   (setq postCallOut "POST-DRILLED CALL-OUT")
   (setq postCallOut2 "PICKET PANEL CALL-OUT")
	(setq postCallOut3 "CABLE TAG")
	(setq layer1 "Cable")
	(setq cableMultiplier 11)

   (DisplayCountTable (CountPostCallOuts))
	(DisplayCountTable (CountCableTextLabels))

   (setvar "cmdecho" 1)		; Command line back on
   (princ)	)		; Hide last return value (clean exit)
	
	
	
;; CountCableTextLabels - Counts cable labes (text) within a user 
;; selection and returns an association list of (labels . quantities).

(defun CountCableTextLabels ( / cableList index textSelSet key)
   (setq textSelSet 
		(ssget 
			(list '(0 . "TEXT") 
				'(8 . "Cable"))))
   (setq index 0)
   (while (< index (sslength textSelSet))
      (progn
			(setq key 
				(cdr 
					(assoc 1 
						(entget 
							(ssname textSelSet index))))) 
         (setq cableList 
				(assoc++ key cableList))
			(setq index 
				(1+ index))))
			
   ;this returns the list (after sorting it)
   (OrderListAscending cableList))


	
;; CountEndPlates - Counts endplate blocks within a user selection 
;; and returns the total.

(defun CountEndPlates (/ postList i blockSelSet key)
   (setq blockSelSet (ssget (list '(0 . "INSERT")
			       '(8 . "A-HRAL-RAIL"))))
   (setq i 0)
   (while (< i (sslength blockSelSet))
      (progn
			(setq key  (cdr (assoc 1  (entget (entnext (ssname blockSelSet i))) )) ) 
         (setq postList (assoc++ key postList))
			(setq i (1+ i)) ))
			
   ;this returns the list (after sorting it)
   (OrderListAscending postList) )
	
	
	
;; CountPostCallOuts - Counts post call-out blocks within a user selection 
;; and returns an association list of (labels . quantities).

(defun CountPostCallOuts (/ postList i blockSelSet key)
   (setq blockSelSet (ssget (list '(0 . "INSERT")
			       '(-4 . "<or")
			       (cons 2 postCallOut)
			       (cons 2 postCallOut2)
					 (cons 2 postCallOut3)
			       '(-4 . "or>"))))
   (setq i 0)
   (while (< i (sslength blockSelSet))
      (progn
			(setq key  (cdr (assoc 1  (entget (entnext (ssname blockSelSet i))) )) ) 
         (setq postList (assoc++ key postList))
			(setq i (1+ i)) ))
			
   ;this returns the list (after sorting it)
   (OrderListAscending postList) )


	
;; DisplayCountTable - Displays the count list as a table: label then quantity
;; result - [association list] Labels paired with quantities.

(defun DisplayCountTable (result)
   (princ "\n=#===Qty=\n")
   (foreach item result
      (progn
			(princ " ")
			(princ (car item))
			(princ " . ")
			(if (= (substr (car item) 1 3) "RUN")
				(princ (* cableMultiplier (cdr item)))
				(princ (cdr item)))
			(princ "\n") ))
   (princ "=========\n") )
	
	

(princ)				; Clean load

