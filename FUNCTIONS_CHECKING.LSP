;;;;;;;[  Checking Functions  ];;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  All shared checking functions.              ;;
;;  (Block, layer, etc. verification)           ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 02/29/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  02/29/2016                                  ;;
;;  - Added isInserted - first function - to    ;;
;;    to check if a block is in the drawing.    ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Functions to check for layers.            ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; isInserted
; Created: 02/29/2016 by J.D. Sandifer
; Purpose: Checks if a block is in the drawing.
; Arguments: Block name (string)
; Returns: T/nil

(defun isInserted ( blockName )
	(/= (tblsearch "block" blockName) nil))


		
(princ)		; Clean load