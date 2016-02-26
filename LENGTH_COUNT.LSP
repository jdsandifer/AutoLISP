;;;;;;;[  Length Count  ];;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Counts total length of center lines.        ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 12/16/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  12/16/2015                                  ;;
;;  - Chopped RAIL_COUNT_COM to make it just    ;;
;;    count length of all centerlines.          ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  -                                           ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:lengthcount (/ oldCmdEcho temp timeToQuit)		; Will be main UI and system variable control only
   (setq oldCmdEcho (getvar "cmdecho"))
   (setvar "cmdecho" 0)				; Turn off command line output (of certain things)

   (if (= *floorsMultiplier* nil)		
         (setq *floorsMultiplier* 1))
   
   (if (= *totalLength* nil)		
         (setq *totalLength* 0))

   (if (or (= *ctrLineLayer* nil) (= *ctrLineLayer* ""))		
      (setq *ctrLineLayer* "Center"))
   
   (if (not *option*)
      (setq *option* "Add"))

   (while (/= timeToQuit "True")
   
   (initget "Multiplier Layer Add Count RESET Quit")
   (setq *option*
      (cond
	 ((getkword
	    (strcat "\nChoose cut list option [Multiplier/Layer/Add/Count/RESET/Quit] <" *option* ">:")))
	 (*option*)))
   
   (cond
      ( (= *option* "RESET") (setq *totalLength* 0) (setq *option* "Add") (princ "\nTotal of centerlines was reset.") )

      ( (= *option* "Quit") (setq timeToQuit "True") (setq *option* "RESET") )

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
        (setq *option* "Quit")
        (setq *totalLength* (+ *totalLength* (MeasureCenterLinesTotal)))
        (princ "\nTotal of centerlines on this floor: ")
        (princ (rtos *totalLength*)) )
   
      ( (= *option* "Count")
        (setq *option* "Quit")
	(princ "\nTotal of centerlines on all floors: ")
        (princ (rtos (* *floorsMultiplier* *totalLength*))) )))

   ;; Restore changed system variables
   (setvar "cmdecho" oldCmdEcho)	; Restore command line output to original value
   (princ)				; Hide last return value (clean exit)
)




;;; Error handling function - prints error message nicely and resets system variables

(defun *error* (msg)
   (princ "\n")
   (princ msg)

   ;; Reset system variables      
   (setvar "cmdecho" oldCmdEcho)

   (princ "\nSystem variables reset")
   (princ "\n")
   (princ))			; Hide last return value (clean exit)




; MeasureCenterLinesTotal
; Created: 12/16/2015 by J.D. Sandifer
; Purpose: Measures lengths of lines on *ctrLineLayer* and returns a total length.
; Input: User selects area contaning the lines.
; Returns: A total length.

(defun MeasureCenterLinesTotal (/ selSet          index             centerline
			          centerLineInfo  centerLineLength  totalLength )

   (setq selSet (ssget (list (cons 8 *ctrLineLayer*) '(0 . "line"))))
      ; have the user select an area that includes the lines to measure
      ; and filter in only lines on *ctrLineLayer* (not case-sensitive)
   (setq index 0)
   (setq totalLength 0)
   (while (< index (sslength selSet))
      ; loop through the selection set
      (setq centerLine (ssname selSet index))
         ; get the next center line from the list
      (setq centerLineInfo (entget centerLine))
         ; get the entity info list for the line
      (setq centerLineLength
         (distance
            (cdr (assoc 10 centerLineInfo))
            (cdr (assoc 11 centerLineInfo))
               ; access the info at keys 10 & 11 (start and end points),
   	       ; chop off the key so its just the points,
         )
            ; measure the distance between the two points,	 
      )
         ; and assign it to centerLineLength
      (setq totalLength (+ centerLineLength totalLength))
	 ; and add it to the total
      (setq index (1+ index))
         ; increment counter (very important)
   )
      ; end of while loop
   
   totalLength
)




(princ)				; Clean load