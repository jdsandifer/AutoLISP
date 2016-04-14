;NAME: graphcFx.lsp
;PROJECT: General
;OBJECTIVE: Graphic functions
;                     Included parametric functions
;ACAD VERSION: AutoLISP for AutoCAD
;VERSION: April 24 2001
;AUTOR: Hector  Monroy, M.S. Civil Eng. , M.S. Computer and Systems Eng.
;hmonroy@ieee.org


;-------------------------------------------------------------------------
;FUNCTION: GraphFx
;OBJECTIVE: Graphic functions type F[x]=y
;                    plane XY [Z=0]
;PARAMETERS: function fltStart fltEnd fltStep
;function:non-evaluated list
;             use prefix mathematic notation [AutoLisp]
;             x is the variable
;             example '(+ (* 2 x x) x 3)
;fltStart: float [real] variable
;             first x value
;fltEnd:   float [real] variable
;             last x value
;fltStep:  float [real] variable
;             Increasing the x variable             
;WARNING: does not identify indeterminations
;RETURN: a list type [[fltstart y][x y] [x y]...[fltEnd y]] with all evaluated points

(defun GraphFx (function fltStart fltEnd fltStep / function fltStart fltEnd fltStep x y lstPoints)
    (setq lisPoints nil)
    (if (<=  fltStart fltEnd)
        (progn
            (setq x fltStart)
            (while (< x  fltEnd)
                (setq y(eval function))
                (setq lstPoints (append lstPoints (list (list x y 0))))
                (setq x(+ x fltStep))
            );endwhile
            (setq x fltEnd)
            (setq y(eval function))
            (setq lstPoints (append lstPoints (list (list x y 0))))
        );endprogn
    );endif
    lstPoints
)
;-------------------------------------------------------------------------


;-------------------------------------------------------------------------
;FUNCTION: GraphPar
;OBJECTIVE: Graphic parametric function 
; q is the parameter in the function
;PARAMETERS:FunctionX FunctionY FunctionZ fltStart fltEnd fltStep
;function_:non-evaluated list
;             use prefix mathematic notation [AutoLisp]
;             q is the variable
;             example '(+ (* 2 q q) q 3)
;             functionX generate x variable
;             functionY generate y variable
;             functionZ generate z variable
;fltStart: float [real] variable
;             first q value
;fltEnd:   float [real] variable
;             last q value
;fltStep:  float [real] variable
;             Increasing the q variable             
;WARNING: does not identify indeterminations
;RETURN: a list type [[x y][x y] [x y]...[x y]] with all evaluated points

(defun GraphPar (functionx functiony functionz  fltStart fltEnd fltStep / functionx
                functiony functionz  fltStart fltEnd fltStep x y z q lstPoints)
    (setq lstPoints nil)
    (if (<=  fltStart fltEnd)
        (progn
            (setq q fltStart)
            (while (< q  fltEnd)
                (setq x(eval functionx))
                (setq y(eval functiony))
                (setq z(eval functionz))
                (setq lstPoints (append lstPoints (list (list x y z))))
                (setq q(+ q fltStep))
            );endwhile
            (setq q  fltEnd)
            (setq x(eval functionx))
            (setq y(eval functiony))
            (setq z(eval functionz))
            (setq lstPoints (append lstPoints (list (list x y z))))
        );endprogn
    );endif
    lstPoints
);endDefun
;-------------------------------------------------------------------------

;-------------------------------------------------------------------------
;FUNCTION: Printpntlst
;OBJECTIVE: printing a point list
;                    use 3DPOLY
;PARAMETERS: lstPoints
;lstPoints: list with points
;point is a list type [x y z] or [x y]
;x,y,z are type float
;WARNING: no error capture
;RETURN nil

(defun Printpntlst(lstPoints / lstPoints pnt)
   (setq echoCurrent(cmdechoOFF))
   (command "3DPOLY")
   (foreach pnt lstPoints (command pnt))
   (command "")
   (setvar "CMDECHO" echoCurrent)
   nil
);endDefun
;-------------------------------------------------------------------------


;-------------------------------------------------------------------------
;FUNCTION: cmdechoOFF
;OBJECTIVE: OFF the cmdecho enviromental variable
;RETURN: integrer
;                current variable status

(defun cmdechoOFF( / echoCurrent)
   (setq echoCurrent(getvar "CMDECHO"))
   (setvar "CMDECHO" 0)
   echoCurrent
);endDefun
;-------------------------------------------------------------------------