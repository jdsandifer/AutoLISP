;;;;;;;[  Reload App  ];;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Uses global variable *appToLoad* to load    ;;
;;  (or reload) an AutoLISP app and uses        ;;
;;  global variable *appToRun* to run (or       ;;
;;  re-run) an AutoLISP app.                    ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 10/20/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  02/08/2016 - J.D.                           ;;
;;  - Changed load behavior to default to last  ;;
;;    choice - regardless of its success.       ;;
;;                                              ;;
;;  12/08/2015 - J.D.                           ;;
;;  - Changed run behavior to default to last   ;;
;;    choice - regardless of its success.       ;;
;;                                              ;;
;;  11/28/2015                                  ;;
;;  - Revised for cleaner, more functional      ;;
;;    code and documentation.                   ;;
;;                                              ;;
;;  11/17/2015                                  ;;
;;  - Added back global variable *appToRun*     ;;
;;    and app running function.                 ;;
;;    Default = PLANDRAW.                       ;;
;;                                              ;;
;;  11/18/2015                                  ;;
;;  - Revised layout to look better and         ;;
;;    restructured code for readability.        ;;
;;                                              ;;
;;  11/09/2015                                  ;;
;;  - Changed global variable to *appToLoad*    ;;
;;    and formatting to new standard.           ;;
;;  - Added simple app name prompt with         ;;
;;    default value.                            ;;
;;  - Added success message.                    ;;
;;  - Added simple error handler that displays  ;;
;;    a load error nicely and resets            ;;
;;    *appToLoad* to "PLANDRAW".                ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C:RELOAD (/ input promptString newApp commandString)

   (setq oldError *error*
	 *error* *errorHandler*)
   
   (if (or (= *appToLoad* nil) (= *appToLoad* ""))
      (setq *appToLoad* "PLANDRAW"))
		
	;; Prompt for LISP app to load, but only assign a new value to *appToLoad*.
   (setq promptString (strcat "\nChoose LISP to reload <" *appToLoad* ">: "))
   (setq input (getstring promptString))
   (setq input (strcase input))		; To ALL CAPS
   (if (and (/= input "") (/= input nil))
      (setq *appToLoad* input))
	
   (load *appToLoad*)
   (princ (strcat *appToLoad* " loaded."))
   ;(setq *appToLoad* newApp)		; Only make this app the new default if the load worked. (Else error on load.)


   (if (or (= *appToRun* nil) (= *appToRun* ""))
      (setq *appToRun* "PLANDRAW"))

   ;; Prompt for LISP app to run, but only assign a new value to *appToRun*.
   (setq promptString (strcat "\nChoose LISP to run or Q for quit <" *appToRun* ">: "))
   (setq input (getstring promptString))
   (setq input (strcase input))
   (cond ((/= input "Q")
      (if (/= input "")
         (setq *appToRun* input))
      (setq commandString (strcat "(C:" *appToRun* ")"))
      (eval (read commandString))))		; Run commandString like in the command line
   
   (setq *error* oldError)
   
   ; Keep command line clean
   (princ))




;;; Simple error message handler that resets defaults.

(defun *errorHandler* (msg)
   (princ msg)
   (setq *error* oldError)
   (princ))




; Keep command line clean
(princ)