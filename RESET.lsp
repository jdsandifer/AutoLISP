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

(defun C:Reset nil ( / systemVariables)
	(setq systemVariables (("cmdecho" . 1)
								  ("osmode" . 191)))
	(JD:ResetAllVars 'systemVariables)
	(princ "Reset finished.")
	(print))


; Silent load
(princ)