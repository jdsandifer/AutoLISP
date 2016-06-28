;;;;;;;[   Test   ];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  Simple file for testing stuff.              ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2016)     ;;
;;  Written: 02/04/2016                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  06/27/2016                                  ;;
;;  - Moved stuff from TEST to here.            ;;
;;  - Some success with block creation, but     ;;
;;    still a lot I don't understand about      ;;
;;    what is required in the definitions and   ;;
;;    what can be omitted. Would like to omit   ;;
;;    as much as possible...                    ;;
;;                                              ;;
;;  02/04/2016                                  ;;
;;  - Testing variable scope - local variables  ;;
;;    as hash tables for my hash functions.     ;;
;;    Works great!                              ;;
;;  - Local variables w/out passing. Works!     ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun test ( / *error*)
	(setq *error* ErrorHandler)
	;(entmake ') - for copying...
	(entmake '((0 . "BLOCK") (8 . "0") (70 . 0) (10 0.0 0.0 0.0) (2 . "TEST1")))
   (entmake '((0 . "CIRCLE") (8 . "0") (10 1.875 -1.875 0.0) (40 . 0.549375)))
	(entmake '((0 . "CIRCLE") (8 . "0") (10 1.875 -1.875 0.0) (40 . 0.31875)))
	(entmake '((0 . "LWPOLYLINE") (8 . "0") (90 . 8) (70 . 1) (10 -2.5 -2.125) (42 . 0) (91 . 0) (10 -2.5 2.125) (42 . -0.414214) (91 . 0) (10 -2.125 2.5) (42 . 0) (91 . 0) (10 2.125 2.5) (42 . -0.414214) (91 . 0) (10 2.5 2.125) (42 . 0) (91 . 0) (10 2.5 -2.125) (42 . -0.414214) (91 . 0) (10 2.125 -2.5) (42 . 0) (91 . 0) (10 -2.125 -2.5) (42 . -0.414214) (91 . 0)))
	(entmake '((0 . "CIRCLE") (8 . "0") (10 1.875 1.875 0.0) (40 . 0.549375)))
	(entmake '((0 . "CIRCLE") (8 . "0") (10 1.875 1.875 0.0) (40 . 0.31875)))
	(entmake '((0 . "CIRCLE") (8 . "0") (10 -1.875 1.875 0.0) (40 . 0.549375)))
   (entmake '((0 . "ENDBLK")))
		
	)

; Silent load
(princ)