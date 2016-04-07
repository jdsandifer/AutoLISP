;;;;;;;[  Helper Functions - User Input  ];;;;;;;;
;;                                              ;;
;;  Shared helper functions for user input.     ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 04/06/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  04/06/2016                                  ;;
;;  - File creation.                            ;;
;;  - Added Input-SelectionSet().               ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Refactor out more functions.              ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Input-SelectionSet - Has the user select objects and applies the 
;; provided filter to the selection.
;; Returns the selection set.

(defun Input-SelectionSet ( varToSet filterList )
	(set varToSet (ssget filterList)))


		
(princ)		; Clean load