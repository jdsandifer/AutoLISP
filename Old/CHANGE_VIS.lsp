(defun c:changevis ( / blk idx obj sel vis )
(setq blk "ISO_FC_MULT" ;; Block Name
vis "300" ;; New Visibility State
)
(if (setq sel (ssget (list '(0 . "INSERT"))))
(repeat (setq idx (sslength sel))
(if (= (strcase blk) (strcase (LM:blockname (setq obj (vlax-ename->vla-object (ssname sel (setq idx (1- idx))))))))
(LM:SetVisibilityState obj vis)
)
)
)
(princ)
)

;; Block Name - Lee Mac
;; Returns the true (effective) name of a supplied block reference
(defun LM:blockname ( obj )
(if (vlax-property-available-p obj 'effectivename)
(defun LM:blockname ( obj ) (vla-get-effectivename obj))
(defun LM:blockname ( obj ) (vla-get-name obj))
)
(LM:blockname obj)
)

;; Set Dynamic Block Visibility State - Lee Mac
;; Sets the Visibility Parameter of a Dynamic Block (if present) to a specific value (if allowed)
;; blk - [vla] VLA Dynamic Block Reference object
;; val - [str] Visibility State Parameter value
;; Returns: [str] New value of Visibility Parameter, else nil
(defun LM:SetVisibilityState ( blk val / vis )
	(if
		(and
			(setq vis (LM:getvisibilityparametername blk))
			(member (strcase val) (mapcar 'strcase (LM:getdynpropallowedvalues blk vis))) )
		(LM:setdynpropvalue blk vis val) ))

;; Set Dynamic Block Property Value - Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil
(defun LM:setdynpropvalue ( blk prp val )
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

;; Get Dynamic Block Property Allowed Values - Lee Mac
;; Returns the allowed values for a specific Dynamic Block property.
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
;; Returns: [lst] List of allowed values for property, else nil if no restrictions
(defun LM:getdynpropallowedvalues ( blk prp )
	(setq prp (strcase prp))
	(vl-some 
	   '(lambda ( x )
			(if (= prp (strcase (vla-get-propertyname x)))
				(vlax-get x 'allowedvalues)))
		(vlax-invoke blk 'getdynamicblockproperties) ))

;; Get Visibility Parameter Name - Lee Mac
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil
(defun LM:getvisibilityparametername ( blk / vis )
	(if
		(and
			(vlax-property-available-p blk 'effectivename)
			(setq blk
				(vla-item
					(vla-get-blocks (vla-get-document blk))
					(vla-get-effectivename blk)))
			(= :vlax-true (vla-get-isdynamicblock blk))
			(= :vlax-true (vla-get-hasextensiondictionary blk))
			(setq vis
				(vl-some
					'(lambda ( pair )
						(if
							(and
								(= 360 (car pair))
								(= "BLOCKVISIBILITYPARAMETER" (cdr (assoc 0 (entget (cdr pair))))) )
							(cdr pair) ))
					(dictsearch
						(vlax-vla-object->ename (vla-getextensiondictionary blk))
						"ACAD_ENHANCEDBLOCK") )))
		(cdr (assoc 301 (entget vis))) ))
		
(vl-load-com)
(princ)