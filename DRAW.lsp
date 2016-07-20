;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                          ;;;;
;;;;  JD's Drawing Functions  ;;;;
;;;;                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;|=========={ JD:Arc }======================|;
;| Draws and arc using supplied center,     |;
;| start, and end points. Uses current      |;
;| settings for everything else.            |;
;|------------------------------------------|;
;| Author: J.D. Sandifer    Rev: 07/12/2016 |;
;|==========================================|;

(defun JD:Arc ( centerPt startPt endPt / )
	(setq FUZZ_FACTOR 0.001)
	(if (not (equal startPt endPt FUZZ_FACTOR))		; Check pts are valid.
   	(command "._arc" "c" centerPt startPt endPt))
	nil)

	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ
    (strcat
        "\n:: DRAW.lsp loaded. | \\U+00A9 J.D. Sandifer "
        (menucmd "m=$(edtime,0,yyyy)")
        " ::"))
(princ)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;;
;;                             End of File                               ;;
;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;