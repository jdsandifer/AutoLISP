;;;;;;;[  Resetting System Variables  ];;;;;;;;;;;
;;                                              ;;
;;  All functions needed to save and restore    ;;
;;  AutoCAD system variables. Built on a hash.  ;;
;;  (requires FUNCTIONS_HASH)                   ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 02/04/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  03/16/2016                                  ;;
;;  - Small edits - spelling, etc.   				;; ;;                                              ;;
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
;;; variableList [symbol] - name of the variable list to clear

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
;;; variableList [symbol] - name of the variable list to use

(defun JD:ResetAllVars ( variableList / )
	(foreach var (eval variableList)
		(setvar (car var) (cdr var)))
	(JD:ClearVars variableList)
	(princ "System variables have been reset.")
	(princ))
	
	

;;; Error handling function - prints error message nicely and resets system variables

(defun ErrorHandler (msg)
   (princ "\nThere's a slight problem: ")
   (princ msg)

   (JD:ResetAllVars 'systemVariables)
	; NEED TO FIX THIS! How can I untangle the variable...?
	
	(command-s "._UNDO" "_End")		; End UNDO group
   ;(command "._U")			; Undo whatever got done so far
   
   (princ))
	
	
	
; Silent load	
(princ)