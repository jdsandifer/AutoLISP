;;;;;;;[  Parts List - Find Block & Set Amt  ];;;;
;;                                              ;;
;;  Finds a specific block and places the       ;;
;;  amount entered in the QTY attribute.        ;;
;;                                              ;;
;;::::::::::::::::::::::::::::::::::::::::::::::;;
;;                                              ;;
;;  Author: J.D. Sandifer  (Copyright 2015)     ;;
;;  Written: 12/02/2015                         ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;  12/02/2015 - JD                             ;;
;;  - Added a simple version of this function   ;;
;;    to PARTS_COUNT for the demo. I may just   ;;
;;    leave it there and stop working on this   ;;
;;    separate app.                             ;;
;;  - Got the basic function to work without    ;;
;;    dealing with dynamic blocks! Guess that   ;;
;;    was just needed for counting blocks that  ;;
;;    that have been rotated. I just commented  ;;
;;    out the anonymous block functions in      ;;
;;    case they are needed later.               ;;
;;                                              ;;
;;  Todo:                                       ;;
;;  - Need to add a check to verify that the    ;;
;;    the sub-entity is a qty entity before     ;;
;;    trying to add to it (or find it).         ;;
;;  - Add prompt for block name (prep for       ;;
;;    automation).                              ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun c:plfasamt ( / )
   (setvar "cmdecho" 0)			; Turn off command line output


   ;; Ask for name of block
   ;(setq blk (getstring t "\nSpecify name of block to select: "))
   (setq blk "04-02-BLOCK-1")


   ;; Ask for the parts list block(s) where the correct block is
   (setq selectedPLBlocks (ssget "x" (list '(0 . "INSERT")
			       (cons 2 blk))))
   (setq selectedPLBlock (ssname selectedPLBlocks 0))
   
   
   ;; Check for the block and assign to a variable if it exists
   ;(if (tblsearch "block" blk)
   ;   (setq blockToChange (getblockselection blk))
   ;   (princ (strcat "\n" blk " doesn't exist.")))

   ;; Check for the block and assign first occurence to a variable
   ;(if (tblsearch "block" blk)
   ;   (setq blockToChange (car (LM:getanonymousreferences blk)))
   ;   (princ (strcat "\n" blk " doesn't exist.")))
   
   ;; Ask for the new amount
   (initget 1)
   (setq amountToBe (getint "Enter an amount for the block:"))


   ;; Set the new value
   ;(LM:setdynpropvalue blockToChange "QTY" amountToBe)

   (setq selectedPLBlockAtt (entnext selectedPLBlock))
   (setq selectedPLBlockInfo (entget selectedPLBlockAtt))
   (setq amountItem (assoc 1 selectedPLBlockInfo))
   
   
   ;; Assign the new value
   (entmod (subst (cons 1 (itoa amountToBe)) amountItem selectedPLBlockInfo))
   

   
   (setvar "cmdecho" 1)		; Command line back on
   (princ))			; Clean exit (hide last return value)




;;; Select the correct block and return it.
;|
(defun getblockselection ( blk )
    (princ "\nblock selection")
    (ssget "_X"
        (list '(0 . "INSERT")
            (cons 2
                (apply 'strcat
                    (cons blk
                        (mapcar '(lambda ( x ) (strcat ",`" x))
                            (LM:getanonymousreferences blk)
                        )
                    )
                )
            )
        )
    ))




;;; Set Dynamic Block Property Value  -  Lee Mac
;;; Modifies the value of a Dynamic Block property (if present)
;;; blk - [vla] VLA Dynamic Block Reference object
;;; prp - [str] Dynamic Block property name (case-insensitive)
;;; val - [any] New value for property
;;; Returns: [any] New value if successful, else nil

(defun LM:setdynpropvalue ( blk prp val )
   (princ "\nset value")
    (setq prp (strcase prp))
    (vl-some
       '(lambda ( x )
            (if (= prp (strcase (vla-get-propertyname x)))
                (progn
                    (vla-put-value x (vlax-make-variant val (vlax-variant-type (vla-get-value x))))
                    (cond (val) (t))
                )
            )
        )
        (vlax-invoke blk 'getdynamicblockproperties)
    )
)




;; Get Anonymous References  -  Lee Mac
;; Returns the names of all anonymous references of a block.
;; blk - [str] Block name/wildcard pattern for which to return anon. references

(defun LM:getanonymousreferences ( blk / ano def lst rec ref )
   (princ "\nget anon")
    (setq blk (strcase blk))
    (while (setq def (tblnext "block" (null def)))
        (if
            (and (= 1 (logand 1 (cdr (assoc 70 def))))
                (setq rec
                    (entget
                        (cdr
                            (assoc 330
                                (entget
                                    (tblobjname "block"
                                        (setq ano (cdr (assoc 2 def)))
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (while
                (and
                    (not (member ano lst))
                    (setq ref (assoc 331 rec))
                )
                (if
                    (and
                        (entget (cdr ref))
                        (wcmatch (strcase (LM:al-effectivename (cdr ref))) blk)
                    )
                    (setq lst (cons ano lst))
                )
                (setq rec (cdr (member (assoc 331 rec) rec)))
            )
        )
    )
    (reverse lst)
)




;; Effective Block Name  -  Lee Mac
;; ent - [ent] Block Reference entity

(defun LM:al-effectivename ( ent / blk rep )
   (princ "\neffective name")
    (if (wcmatch (setq blk (cdr (assoc 2 (entget ent)))) "`**")
        (if
            (and
                (setq rep
                    (cdadr
                        (assoc -3
                            (entget
                                (cdr
                                    (assoc 330
                                        (entget
                                            (tblobjname "block" blk)
                                        )
                                    )
                                )
                               '("acdbblockrepbtag")
                            )
                        )
                    )
                )
                (setq rep (handent (cdr (assoc 1005 rep))))
            )
            (setq blk (cdr (assoc 2 (entget rep))))
        )
    )
    blk
)



|;
(princ)		; Clean load (no output)


