;;;;;;;[  Dimension to "EQ"  ];;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Changes dimension text to "EQ".             ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 12/08/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  12/08/2015                                  ;;
;;  - Began initial code writing. Success!      ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Add option for different text?            ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:dimtoeq ( / loopCounter dimEntity dimEntityInfo
		     dimTextUnit)
   (setvar "cmdecho" 0)			; Turn off command line output

   (setq loopCounter 0)			; Anti-infinite-loop device
   
   ;; Ask for the dimension to which to change to "EQ"
   (while (and (setq dimEntity (car (entsel))) (< loopCounter 50))
      (setq dimEntityInfo (entget dimEntity))
      (setq dimTextUnit (assoc 1 dimEntityInfo))
      (entmod (subst (cons 1 "EQ") dimTextUnit dimEntityInfo))
      (setq loopCounter (1+ loopCounter)))

   (setvar "cmdecho" 1)		; Command line back on
   (princ))			; Clean exit (hide last return value)




(princ)		; Clean load (no output)


