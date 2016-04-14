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
;;  04/13/2016                                  ;;
;;  - Added LM:ssget and endfile.               ;;
;;  - Shortened file name to INPUT.             ;;
;;                                              ;;
;;  04/06/2016                                  ;;
;;  - File creation.                            ;;
;;  - Added Input-SelectionSet().               ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Refactor out more functions.              ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; SelectionSet - Has the user select objects and applies the 
;; provided filter to the selection.
;; Returns the selection set.

(defun JD:InputSelectionSet ( varToSet filterList )
	(set varToSet (ssget filterList)))


	
;; ssget  -  Lee Mac
;; A wrapper for the ssget function to permit the use of a custom selection prompt
;; msg - [str] selection prompt
;; arg - [lst] list of ssget arguments

(defun LM:ssget ( msg arg / sel )
    (princ msg)
    (setvar 'nomutt 1)
    (setq sel (vl-catch-all-apply 'ssget arg))
    (setvar 'nomutt 0)
    (if (not (vl-catch-all-error-p sel)) sel)
)



;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: INPUT.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;