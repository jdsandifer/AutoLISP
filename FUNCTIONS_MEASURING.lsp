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
;;  04/06/2016                                  ;;
;;  - Added pline segmen measuring.             ;;
;;  - Renamed MeasureLinear to                  ;;
;;    MeasureLineSegments.                      ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Turn return value into true quantity      ;;
;;    list through additional helper            ;;
;;    functions.                                ;;
;;  - Refactor more. Maybe take in a selection  ;;
;;    set as an argument?                       ;;
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



; MeasureLineSegments
; Created: 02/22/2016 by J.D. Sandifer
; Purpose: Measures lengths of lines, plines, or mlines on a specific layer
; and returns a list of their lengths.
; Input: User selects area contaning the lines.
; Returns: A sum of their lengths.

(defun MeasureLineSegments ( layerToCount roundingFactor fudgeFactorToAdd /
									  selSet lineEntity lineInfo 
							   linelength cutList lineList)
	(setq lineList nil)
   (setq selSet (ssget (list (cons 8 layerToCount) 
									 '(-4 . "<or")
									 	'(0 . "*line")
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
				(setq lineList (append lineList (list 
					(GetLengthOfLine lineEntity)))))
			(	(= (cdr (assoc 0 (entget lineEntity))) "MLINE")
				(setq lineList (append lineList 
					(GetLengthOfEachMultilineSegment lineEntity))))
			(	(= (cdr (assoc 0 (entget lineEntity))) "LWPOLYLINE")
				(setq lineList (append lineList 
					(GetLengthOfEachPLineSegment lineEntity))))
			(T (princ "\nUnknown line type snuck in...")
				(princ (assoc 0 (entget lineEntity)))))
				; add it to the list if it's a line we can measure
      (setq index (1+ index)))
         ; increment counter (very important)
	;(setq lineList (OrderListBy lineList '>))
	;; turn simple list into qty assoc list
	(setq lineQtyList nil)
	(foreach segmentLength lineList
		(setq segmentLength (RoundUpTo roundingFactor
			(+ fudgeFactorToAdd segmentLength)))
		(setq lineQtyList (Assoc++ segmentLength lineQtyList)))
	(OrderList lineQtyList))
	

	
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
	
	

(defun GetLengthOfEachMultilineSegment ( lineEntity /
													  lineEntityInfo segmentList)
	(setq lineEntityInfo (entget lineEntity))
		; get line entity info list
	(setq segmentList nil)
	(foreach infoItem lineEntityInfo
		(cond
			(  (= (car infoItem) 11)
				(setq segmentList (append segmentList (list (cdr infoItem))))
				)))
	(setq firstPoint (car segmentList))
	(setq segmentList (cdr segmentList))
	(setq lengthsList nil)
	(foreach secondPoint segmentList
		(setq lengthsList 
			(append lengthsList (list (distance firstPoint secondPoint))))
		(setq firstPoint secondPoint))
	lengthsList)
	
	

(defun GetLengthOfPolyline ( lineEntity / )
	(vl-load-com)
   (vlax-curve-getDistAtParam lineEntity
		(vlax-curve-getEndParam lineEntity)))


		
(defun GetLengthOfEachPLineSegment ( lineEntity / lengthsList)
	(setq pntList (ReadPline lineEntity))
	(setq ptCntr 0)
	(setq lengthsList nil)
	(repeat (1- (length pntList))
			(setq fpoint(nth ptCntr pntList))
			(setq epoint(nth (1+ ptCntr) pntList))
			(setq thr(distance fpoint epoint))
			(setq ptCntr (1+ ptCntr))
			(setq lengthsList (append lengthsList (list thr)))
			))
	
	
	
;Function to Read Vertices of Selected Lines.
(defun ReadPline(imp_Ent)
	(setq glb_obj(vlax-ename->vla-object imp_Ent))
	(setq glb_PntCnt(vlax-curve-getEndParam glb_obj))
	(setq returnPTList '())
	(setq ptCntr 1)
	(setq glb_oName(vlax-get-property glb_obj 'ObjectName))
	(setq glb_OClosed nil)
	(if (= glb_oName "AcDbLine")
		(progn
			(setq glb_EnDetails(entget imp_Ent))
			(setq big_Point3d(cdr (assoc 10 glb_EnDetails)))
			(setq end_Point3d(cdr (assoc 11 glb_EnDetails)))
			(setq returnPTList(append returnPTList (list big_Point3d)))
			(setq returnPTList(append returnPTList (list end_point3d))))
		(progn
			(setq glb_OClosed(vlax-curve-isClosed glb_obj))
			(setq glb_2dDist 0)
			(setq old_Point nil)
			(repeat (1+ (fix glb_PntCnt))
				(setq cur_Point3d(vlax-curve-getPointAtParam glb_obj (1- ptCntr)))
				(setq returnPTList(append returnPTList (list cur_Point3d)))
				(setq ptCntr(1+ ptCntr)))))
	(setq return returnPTList))


		
(princ)		; Clean load