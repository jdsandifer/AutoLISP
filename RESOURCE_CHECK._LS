;;;;;;;[  Resource Check ];;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Checks for various resources needed for     ;;
;;  the main plan drawing function.             ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 12/09/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  12/09/2015                                  ;;
;;  - Began initial code writing (MLSTYLE).     ;;
;;    Success!                                  ;;
;;                                              ;;
;;  12/14/2015                                  ;;
;;  - Began initial code writing (block).       ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  -                                           ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:resourcecheck ( / mlstyleToCheck mlstyleDictionary topRailMLStyle)
   (setvar "cmdecho" 0)			; Turn off command line output


   (setq blockToCheck "END_PLATE")
   (setq mlstyleToCheck "TOP_RAIL")

   ;; Search fo block needed in the block table


   ;; Display result of search
   

   ;; Search for MLSTYLE needed in the style dictionary.
   (setq mlstyleDictionary (dictsearch (namedobjdict) "ACAD_MLINESTYLE"))
   (setq topRailMLStyle (dictsearch (cdr (assoc -1 mlstyleDictionary)) mlstyleToCheck))


   ;; Display result of search
   (cond ((/= topRailMLStyle nil)
	  (princ (strcat "\nMLStyle \"" mlstyleToCheck "\" found!\n"))
	  (princ topRailMLStyle))
   
	 ((= topRailMLStyle nil)
	  (princ (strcat"\nMLStyle \"" mlstyleToCheck "\" not found.\n"))
	  (princ "Cannot complete function: Exiting...")))
	     

   (setvar "cmdecho" 1)		; Command line back on
   (princ))			; Clean exit (hide last return value)




(princ)		; Clean load (no output)


