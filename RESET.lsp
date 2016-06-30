;;;;;;;[   Reset Variables   ];;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Simple function to reset system variables   ;;
;;  helper functions. Unit testing only.        ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 06/27/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  06/27/2016                                  ;;
;;  - Started.                                  ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Resets system variables to my defaults

(defun C:Reset nil
	(setvar "cmdecho" 1)
	(setvar "osmode" 191)
	(princ "Reset finished.")
	(print))


; Silent load
(princ)