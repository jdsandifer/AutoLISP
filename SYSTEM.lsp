;;;;;;;[  Resetting System Variables  ];;;;;;;;;;;
;;                                              ;;
;;  All functions needed to save and restore    ;;
;;  AutoCAD system variables. Built on a hash.  ;;
;;  (requires HASH)                             ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 02/04/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  04/13/2016                                  ;;
;;  - Added LM:startundo, endundo (inc. acdoc)  ;;
;;    and endfile.                              ;;
;;  - Shortened filename to SYSTEM.             ;;
;;                                              ;;
;;  03/22/2016                                  ;;
;;  - Small edit - newline in text output for   ;;
;;    reset function.                           ;;
;;                                              ;;
;;  03/15/2016                                  ;;
;;  - Added ChangeVar(iable).                   ;;
;;  - Added Save&ChangeVar.						   ;;
;;                                              ;;
;;  02/04/2016                                  ;;
;;  - Added SaveVar(iable).                     ;;
;;  - Added ClearVar(iable)s.                   ;;
;;  - Added ResetVar(iable).                    ;;
;;  - Added ResetAllVar(iable)s.                ;;
;;  - Did preliminary testing.                  ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Success messages or return values?        ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                  
;;; JD:ClearVar(iable)s - Copyright 2016 J.D. Sandifer
;;; Clears the list of system variables.
;;; variableList [symbol] - name of variable list to clear (nil value ok)

(defun JD:ClearVars ( variableList / )
	(JD:ClearHash variableList)
	(princ))
	
	
	
;;; JD:SaveVar(iable) - Copyright 2016 J.D. Sandifer
;;; Saves the selected system variable if not already saved.
;;; variable [string] - system variable to store
;;; variableList [symbol] - name of the variable list to use

(defun JD:SaveVar ( variable variableList / )
	(if (not (JD:GetHash variable variableList))
		(JD:PutHash variable (getvar variable) variableList))
	(princ))
		
	
	
;;; JD:ChangeVar(iable) - Copyright 2016 J.D. Sandifer
;;; Changes the selected system variable.
;;; variable [string] - system variable to store
;;; newValue [varies] - the value to which to set the variable

(defun JD:ChangeVar ( variable newValue / )
	(setvar variable newValue)
	(princ))
		

	
;;; JD:Save&ChangeVar(iable) - Copyright 2016 J.D. Sandifer
;;; Saves and changes the selected system variable.
;;; variable [string] - system variable to store
;;; variableList [symbol] - name of the variable list to use
;;; newValue [varies] - the value to which to set the variable

(defun JD:Save&ChangeVar ( variable variableList newValue / )
	(JD:SaveVar variable variableList)
	(JD:ChangeVar variable newValue)
	(princ))
		

	
;;; JD:ResetVar(iable)s - Copyright 2016 J.D. Sandifer
;;; Restores a single system variable from the list (without removing it).
;;; variable [string] - system variable to store
;;; variableList [symbol] - name of the variable list to use

(defun JD:ResetVar ( variable variableList / )
	(setvar variable (JD:GetHash variable variableList))
	(princ))
		
	
	
;;; JD:ResetAllVar(iable)s - Copyright 2016 J.D. Sandifer
;;; Restores all system variables in the list.
;;; variable [string] - system variable to store
;;; variableList [symbol] - name of the variable list to use (nil value ok)

(defun JD:ResetAllVars ( variableList / )
	(foreach variable (eval variableList)
			(JD:ResetVar variable variableList))
	(JD:ClearVars variableList)
	(princ))
	
	

;;; Error handling function - prints error message nicely and resets system variables

(defun ErrorHandler (errorMessage)
   (if (not (member errorMessage '("Function cancelled" "quit / exit abort")))
		(princ (strcat "\nThere's a slight problem: " errorMessage)))

   (JD:Save&ChangeVar "cmdecho" 'systemVariables 0)
	
	(command-s "._UNDO" "_End")		; End UNDO group
	
	(JD:ResetAllVars 'systemVariables)
	; Relies on global variable, but nil works...so maybe ok
	
   (princ))
	
	
	
;; Start Undo  -  Lee Mac
;; Opens an Undo Group.

(defun LM:startundo ( doc )
    (LM:endundo doc)
    (vla-startundomark doc)
)



;; End Undo  -  Lee Mac
;; Closes an Undo Group.

(defun LM:endundo ( doc )
    (while (= 8 (logand 8 (getvar 'undoctl)))
        (vla-endundomark doc)
    )
)



;; Active Document  -  Lee Mac
;; Returns the VLA Active Document Object

(defun LM:acdoc nil
    (eval (list 'defun 'LM:acdoc 'nil (vla-get-activedocument (vlax-get-acad-object))))
    (LM:acdoc)
)



;;----------------------------------------------------------------------;;

(vl-load-com)
(princ
    (strcat
        "\n:: SYSTEM.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::"))
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;