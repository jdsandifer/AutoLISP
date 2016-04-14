;;-------------------------=={ 2D Projection }==------------------------;;
;;                                                                      ;;
;;  This program provides the user with a means of projecting a         ;;
;;  selected set of planar objects from one reference frame to another. ;;
;;                                                                      ;;
;;  Upon issuing the command syntax '2dpro' at the AutoCAD              ;;
;;  command-line, the user is prompted to select a set of 2D planar     ;;
;;  objects to be projected. This selection is restricted to Arcs,      ;;
;;  Circles, Elipses, Lines, LWPolylines, 2D (Heavy) Polylines,         ;;
;;  2D Splines & Points.                                                ;;
;;                                                                      ;;
;;  The user is then prompted to select a source reference frame &      ;;
;;  a destination reference frame. For each of these prompts, the       ;;
;;  program requires the user to select a closed LWPolyline with four   ;;
;;  non-collinear vertices. Following each selection, the program       ;;
;;  will ensure the points are counter-clockwise oriented with the      ;;
;;  points ordered such that the lower-left vertex appears first.       ;;
;;                                                                      ;;
;;  Following valid user responses, the program will then convert the   ;;
;;  four 2D points defining each reference frame into homogeneous       ;;
;;  coordinates, and will calculate the transformation matrix to map    ;;
;;  from the source reference frame (or projective space) to the        ;;
;;  destination reference frame.                                        ;;
;;                                                                      ;;
;;  The program will then iterate over the set of selected objects      ;;
;;  and, for each object, will calculate a 2D point set describing or   ;;
;;  (in the case of curved objects) approximating the object.           ;;
;;                                                                      ;;
;;  Each point is then converted to homogeneous coordinates and mapped  ;;
;;  to the destination reference frame using the tranformation matrix,  ;;
;;  before being converted back to cartesian coordinates.               ;;
;;                                                                      ;;
;;  The program will then generate either a Point, Line or LWPolyline   ;;
;;  from the mapped point(s) with properties matching those of the      ;;
;;  original object.                                                    ;;
;;----------------------------------------------------------------------;;
;;  Author:  Lee Mac, Copyright © 2014  -  www.lee-mac.com              ;;
;;----------------------------------------------------------------------;;
;;  Version 1.0    -    2014-10-10                                      ;;
;;                                                                      ;;
;;  First release.                                                      ;;
;;----------------------------------------------------------------------;;

(defun c:2dpro ( / *error* des ent enx idx lst mat ocs sel src typ )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )

    (LM:startundo (LM:acdoc))
    (if
        (and
             (setq sel
                 (LM:ssget "\nSelect objects to project: "
                    '(   "_:L"
                         (
                             (-4 . "<OR")
                                 (-4 . "<AND")
                                     (0 . "ARC,CIRCLE,ELLIPSE,LINE,*POLYLINE,POINT")
                                     (-4 . "<NOT")
                                         (-4 . "<AND")
                                             (0 . "POLYLINE") (-4 . "&") (70 . 88)
                                         (-4 . "AND>")
                                     (-4 . "NOT>")
                                 (-4 . "AND>")
                                 (-4 . "<AND")
                                     (0 . "SPLINE") (-4 . "&=") (70 . 8)
                                 (-4 . "AND>")
                             (-4 . "OR>")
                         )
                     )
                 )
             )
             (setq src (2dprojection:getreferenceframe "\nSelect source reference frame: "))
             (setq des (2dprojection:getreferenceframe "\nSelect destination reference frame: "))
             (setq mat (2dprojection:getmatrix src des))
             (setq ocs (trans '(0.0 0.0 1.0) 1 0 t))
        )
        (repeat (setq idx (sslength sel))
            (setq ent (ssname sel (setq idx (1- idx)))
                  enx (entget ent)
                  typ (cdr (assoc 0 enx))
            )
            (cond
                (   (= "POINT" typ)
                    (entmake
                        (vl-list*
                           '(0 . "POINT")
                            (cons 10 (trans (2dprojection:mappoint mat (trans (cdr (assoc 10 enx)) 0 ocs)) ocs 0))
                            (LM:defaultprops enx)
                        )
                    )
                )
                (   (= "LINE" typ)
                    (entmake
                        (vl-list*
                           '(0 . "LINE")
                            (cons 10 (trans (2dprojection:mappoint mat (trans (cdr (assoc 10 enx)) 0 ocs)) ocs 0))
                            (cons 11 (trans (2dprojection:mappoint mat (trans (cdr (assoc 11 enx)) 0 ocs)) ocs 0))
                            (LM:defaultprops enx)
                        )
                    )
                )
                (   (setq lst (LM:Entity->PointList ent))
                    (entmake
                        (append
                            (list
                               '(000 . "LWPOLYLINE")
                               '(100 . "AcDbEntity")
                               '(100 . "AcDbPolyline")
                                (cons 90 (length lst))
                                (if (vlax-curve-isclosed ent) '(70 . 1) '(70 . 0))
                            )
                            (LM:defaultprops enx)
                            (mapcar '(lambda ( p ) (cons 10 (2dprojection:mappoint mat (trans p 0 ent)))) lst)
                            (list (assoc 210 enx))
                        )
                    )
                )
            )
        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;; 2D Projection: Get Reference Frame  -  Lee Mac
;; Prompts the user to select a closed planar polyline with 4 vertices in
;; order to obtain 4 counter-clockwise oriented non-collinear points
;; defining a reference frame for the transformation.

(defun 2dprojection:getreferenceframe ( msg / ent enx lst tmp )
    (while
        (progn (setvar 'errno 0) (setq ent (car (entsel msg)))
            (cond
                (   (= 7 (getvar 'errno))
                    (princ "\nMissed, try again.")
                )
                (   (null ent) nil)
                (   (or (/= "LWPOLYLINE" (cdr (assoc 0 (setq enx (entget ent)))))
                        (zerop (logand 1 (cdr (assoc 70 enx))))
                        (/= 4 (cdr (assoc 90 enx)))
                        (/= 4 (length (setq lst (2dprojection:uniquefuzz (mapcar 'cdr (vl-remove-if-not '(lambda ( x ) (= 10 (car x))) enx)) 1e-8))))
                        (2dprojection:checkcollinearity (cons (last lst) lst))
                    )
                    (setq lst nil)
                    (princ "\nPlease select a closed polyline with 4 non-collinear vertices.")
                )
            )
        )
    )
    (if lst
        (progn
            (if (2dprojection:clockwise-p lst)
                (setq lst (reverse lst))
            )
            (setq tmp (apply 'mapcar (cons 'min lst)))
            (repeat (car (vl-sort-i lst '(lambda ( a b ) (< (distance a tmp) (distance b tmp)))))
                (setq lst (append (cdr lst) (list (car lst))))
            )
            lst
        )
    )
)

;; 2D Projection: Unique with Fuzz  -  Lee Mac
;; Returns a list with all elements considered duplicate to a given tolerance removed.

(defun 2dprojection:uniquefuzz ( lst fuz )
    (if lst
        (cons (car lst)
            (2dprojection:uniquefuzz
                (vl-remove-if
                    (function (lambda ( x ) (equal x (car lst) fuz)))
                    (cdr lst)
                )
                fuz
            )
        )
    )
)

;; 2D Projection: Check Collinearity  -  Lee Mac
;; Returns T if any three points in a supplied list are collinear.

(defun 2dprojection:checkcollinearity ( lst )
    (and (caddr lst)
        (or (   (lambda ( a b c )
                    (or (equal (+ a b) c 1e-8)
                        (equal (+ b c) a 1e-8)
                        (equal (+ c a) b 1e-8)
                    )
                )
                (distance (car  lst) (cadr  lst))
                (distance (cadr lst) (caddr lst))
                (distance (car  lst) (caddr lst))
            )
            (2dprojection:checkcollinearity (cdr lst))
        )
    )
)

;; 2D Projection: Clockwise-p  -  Lee Mac
;; Returns T if the supplied point list is clockwise oriented.

(defun 2dprojection:clockwise-p ( lst )
    (minusp
        (apply '+
            (mapcar
                (function
                    (lambda ( a b )
                        (- (* (car b) (cadr a)) (* (car a) (cadr b)))
                    )
                )
                lst (cons (last lst) lst)
            )
        )
    )
)

;; 2D Projection: Map Point  -  Lee Mac
;; Converts a supplied 2D point to homogeneous coordinates, applies a
;; matrix transformation & then converts the result back to cartesian coordinates.

(defun 2dprojection:mappoint ( mat pnt )
    (apply (function (lambda ( x y z ) (list (/ x z) (/ y z))))
        (mxv mat (list (car pnt) (cadr pnt) 1.0))
    )
)

;; 2D Projection: Get Matrix  -  Lee Mac
;; Calculates the transformation matrix for transforming
;; homogeneous 2D points from one reference frame to another.

(defun 2dprojection:getmatrix ( l1 l2 / f )
    (mxm
        (
            (setq f
                (lambda ( l / c m )
                    (setq c
                        (mxv
                            (invm
                                (setq m
                                    (trp
                                        (mapcar
                                            (function
                                                (lambda ( a b )
                                                    (list (car a) (cadr a) b)
                                                )
                                            )
                                            l '(1.0 1.0 1.0)
                                        )
                                    )
                                )
                            )
                            (list (car (last l)) (cadr (last l)) 1.0)
                        )
                    )
                    (mapcar '(lambda ( r ) (mapcar '* r c)) m)
                )
            )
            l2
        )
        (invm (f l1))
    )
)

;; Default Properties  -  Lee Mac
;; Returns a list of DXF properties for the supplied DXF data,
;; substituting default values for absent DXF groups
 
(defun LM:defaultprops ( enx )
    (mapcar '(lambda ( x ) (cond ((assoc (car x) enx)) ( x )))
       '(
            (006 . "BYLAYER")
            (008 . "0")
            (039 . 0.0)
            (048 . 1.0)
            (062 . 256)
            (370 . -1)
        )
    )
)

;; Entity to Point List  -  Lee Mac
;; Returns a list of points describing or approximating the supplied entity,
;; else nil if the entity is not supported.

(defun LM:entity->pointlist ( ent / acc der di1 di2 di3 enx fun inc lst par rad typ )
    (setq enx (entget ent)
          typ (cdr (assoc 0 enx))
          acc 35.0
    )
    (cond
        (   (= "POINT" typ)
            (list (cdr (assoc 10 enx)))
        )
        (   (= "LINE" typ)
            (list (cdr (assoc 10 enx)) (cdr (assoc 11 enx)))
        )
        (   (wcmatch typ "CIRCLE,ARC")
            (setq di1 0.0
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam ent))
                  inc (/ di2 (1+ (fix (* acc (/ di2 (cdr (assoc 40 enx)) (+ pi pi))))))
                  fun (if (vlax-curve-isclosed ent) < <=)
            )
            (while (fun di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      di1 (+ di1 inc)
                )
            )
            lst
        )
        (   (or (= typ "LWPOLYLINE")
                (and (= typ "POLYLINE") (zerop (logand (cdr (assoc 70 enx)) 80)))
            )
            (setq par 0)
            (repeat (fix (1+ (vlax-curve-getendparam ent)))
                (if (setq der (vlax-curve-getsecondderiv ent par))
                    (if (equal der '(0.0 0.0 0.0) 1e-8)
                        (setq lst (cons (vlax-curve-getpointatparam ent par) lst))
                        (if (setq rad (distance '(0.0 0.0) (vlax-curve-getfirstderiv ent par))
                                  di1 (vlax-curve-getdistatparam ent par)
                                  di2 (vlax-curve-getdistatparam ent (1+ par))
                            )
                            (progn
                                (setq inc (/ (- di2 di1) (1+ (fix (* acc (/ (- di2 di1) rad (+ pi pi)))))))
                                (while (< di1 di2)
                                    (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                                          di1 (+ di1 inc)
                                    )
                                )
                            )
                        )
                    )
                )
                (setq par (1+ par))
            )
            (if (or (vlax-curve-isclosed ent) (equal '(0.0 0.0 0.0) der 1e-8))
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (= "ELLIPSE" typ)
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  di3 (* di2 (/ (+ pi pi) (abs (- (vlax-curve-getendparam ent) (vlax-curve-getstartparam ent)))))
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      der (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1)))
                      di1 (+ di1 (/ di3 (1+ (fix (/ acc (/ di3 der (+ pi pi)))))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
        (   (= "SPLINE" typ)
            (setq di1 (vlax-curve-getdistatparam ent (vlax-curve-getstartparam ent))
                  di2 (vlax-curve-getdistatparam ent (vlax-curve-getendparam   ent))
                  inc (/ di2 25.0)
            )
            (while (< di1 di2)
                (setq lst (cons (vlax-curve-getpointatdist ent di1) lst)
                      der (/ (distance '(0.0 0.0) (vlax-curve-getsecondderiv ent (vlax-curve-getparamatdist ent di1))) inc)
                      di1 (+ di1 (if (equal 0.0 der 1e-10) inc (min inc (/ 1.0 der (* 10. inc)))))
                )
            )
            (if (vlax-curve-isclosed ent)
                lst
                (cons (vlax-curve-getendpoint ent) lst)
            )
        )
    )
)

;; Matrix Inverse  -  gile & Lee Mac
;; Uses Gauss-Jordan Elimination to return the inverse of a non-singular nxn matrix.
;; Args: m - nxn matrix

(defun invm ( m / c f p r )
    (defun f ( p m ) (mapcar '(lambda ( x ) (mapcar '(lambda ( a b ) (- a (* (car x) b))) (cdr x) p)) m))
    (setq  m (mapcar 'append m (imat (length m))))
    (while m
        (setq c (mapcar '(lambda ( x ) (abs (car x))) m))
        (repeat (vl-position (apply 'max c) c)
            (setq m (append (cdr m) (list (car m))))
        )
        (if (equal 0.0 (caar m) 1e-14)
            (setq m nil
                  r nil
            )
            (setq p (mapcar '(lambda ( x ) (/ (float x) (caar m))) (cdar m))
                  m (f p (cdr m))
                  r (cons p (f p r))
            )
        )
    )
    (reverse r)
)

;; Identity Matrix  -  Lee Mac
;; Args: n - matrix dimension

(defun imat ( n / i j l m )
    (repeat (setq i n)
        (repeat (setq j n)
            (setq l (cons (if (= i j) 1.0 0.0) l)
                  j (1- j)
            )
        )
        (setq m (cons l m)
              l nil
              i (1- i)
        )
    )
    m
)

;; Matrix x Matrix  -  Vladimir Nesterovsky
;; Args: m,n - nxn matrices

(defun mxm ( m n )
    ((lambda ( a ) (mapcar '(lambda ( r ) (mxv a r)) m)) (trp n))
)

;; Matrix x Vector  -  Vladimir Nesterovsky
;; Args: m - nxn matrix, v - vector in R^n

(defun mxv ( m v )
    (mapcar '(lambda ( r ) (apply '+ (mapcar '* r v))) m)
)

;; Matrix Transpose  -  Doug Wilson
;; Args: m - nxn matrix

(defun trp ( m )
    (apply 'mapcar (cons 'list m))
)

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
        "\n:: 2DProjection.lsp | Version 1.0 | \\U+00A9 Lee Mac "
        (menucmd "m=$(edtime,0,yyyy)")
        " www.lee-mac.com ::"
        "\n:: Type \"2dpro\" to Invoke ::"
    )
)
(princ)

;;----------------------------------------------------------------------;;
;;                             End of File                              ;;
;;----------------------------------------------------------------------;;