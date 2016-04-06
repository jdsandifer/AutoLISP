;;;;;;;[  Helper Functions  ];;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  All shared helper functions for drawings.   ;;
;;  (Drawing, measuring, tables, etc.)          ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 02/22/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  02/22/2016                                  ;;
;;  - Added MeasureLines function.              ;;
;;  - Now measures polylines, too.              ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Turn return value into true quantity      ;;
;;    list through additional helper            ;;
;;    functions.                                ;;
;;  - Refactor more. Maybe take in a selection  ;;
;;    set as an argument? EntName is passed to  ;;
;;    helper functions instead of EntInfo?      ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; MeasureLines
; Gets selection set and picks out only the lines before passing them to
; MeasureLinear add then sums the results (functionally).

;(defun MeasureLines (/)
;	get selection set
;  loop through it and select lines
;  sum results of passing selection set to MeasureLinear
;  (or sum result of passing each object...?)
;)



; MeasureLinear
; Created: 02/22/2016 by J.D. Sandifer
; Purpose: Measures lengths of lines or plines on a specific layer and 
; returns a list of their lengths.
; Input: User selects area contaning the lines.
; Returns: A sum of their lengths.

(defun MeasureLinear ( / selSet lineEntity lineInfo 
							   linelength cutList totalLength)
	;(setq totalLength 0)
	(setq linelength 0)
   (setq selSet (ssget (list (cons 8 "Center") 
									 '(-4 . "<or")
									 	'(0 . "line")
									   '(0 . "*polyline")
									 '(-4 . "or>"))))
      ; have the user select an area that includes the lines to measure
      ; and filter in only lines on "A-HRAL-CNTR" layer (not case-sensitive)
   (setq index 0)
   (while (< index (sslength selSet))
      ; loop through the selection set
      (setq lineEntity (ssname selSet index))
         ; get the next line entity from the list
		(cond
			(	(= (cdr (assoc 0 (entget lineEntity))) "LINE")
				(setq totalLength (append totalLength (list 
					(GetLengthOfLine lineEntity))))
				(princ totalLength)
				(princ "-a\n")
					)
			(	(= (cdr (assoc 0 (entget lineEntity))) "LWPOLYLINE")
				(setq totalLength (append totalLength (list 
					(GetLengthOfPolyLine lineEntity))))
				(princ totalLength)
				(princ "-b\n")
					)
			(T (princ "\nUnknown line type snuck in...")))
				; add it to the sum if it's a line we can measure
      (setq index (1+ index)))
         ; increment counter (very important)
	(setq totalLength (vl-sort totalLength '>))
	(princ "\nDifferent length of lines: ")
	(foreach aLength totalLength
		(princ "\n")
		(princ aLength))
	(princ "\n")
   totalLength)
	

	
(defun GetLengthOfLine ( lineEntity / lineEntityInfo)
	(setq lineEntityInfo (entget lineEntity))
		; get line entity info list
	(distance
		(cdr (assoc 10 lineEntityInfo))
      (cdr (assoc 11 lineEntityInfo)))
		; access the info at keys 10 & 11 (start and end points),
   	; chop off the key so its just the points,
	; measure the distance between the two points,
	)
	
	
	
(defun GetLengthOfPolyLine ( lineEntity / )
	(vl-load-com)
   (vlax-curve-getDistAtParam lineEntity
		(vlax-curve-getEndParam lineEntity)))


		
(princ)		; Clean load