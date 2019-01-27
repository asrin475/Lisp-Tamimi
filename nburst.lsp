(defun c:nb ( / *error* idx sel )

    (defun *error* ( msg )
        (LM:endundo (LM:acdoc))
        (if (not (wcmatch (strcase msg t) "*break,*cancel*,*exit*"))
            (princ (strcat "\nError: " msg))
        )
        (princ)
    )
    
    (LM:startundo (LM:acdoc))
    (if
        (setq sel
            (LM:ssget "\nSelect blocks to burst: "
                (list "_:L"
                    (cons '(0 . "INSERT")
                        (
                            (lambda ( / def lst )
                                (while (setq def (tblnext "block" (null def)))
                                    (if (= 4 (logand 4 (cdr (assoc 70 def))))
                                        (setq lst (vl-list* "," (cdr (assoc 2 def)) lst))
                                    )
                                )
                                (if lst (list '(-4 . "<NOT") (cons 2 (apply 'strcat (cdr lst))) '(-4 . "NOT>")))
                            )
                        )
                    )
                )
            )
        )
        (repeat (setq idx (sslength sel))
        	(setq ob (vlax-ename->vla-object (ssname sel (setq idx (1- idx)))))
        	; (if (and (> (length (vlax-invoke ob 'getattributes)) 0) (vlax-write-enabled-p ob))
        	; 	(vla-delete ob)
         ;    )
                (progress)
            	(LM:burstnested ob)

        )
    )
    (LM:endundo (LM:acdoc))
    (princ)
)

;; Burst Nested  -  Lee Mac
;; Bursts the supplied block & all nested blocks found within.
;; obj - [vla] VLA Block Reference Object

(defun LM:burstnested ( obj / col idx lay lin lst obj )

	; (if (and (/= obj nil) (/= (vlax-invoke obj 'GetAttributes) nil) (vlax-write-enabled-p obj))
 ;        		(vla-delete obj)
            
 ;        		(progn
	    (if (and (= "AcDbBlockReference" (vla-get-objectname obj))
	             (vlax-write-enabled-p obj)
	             (not (vl-catch-all-error-p (setq lst (vl-catch-all-apply 'vlax-invoke (list obj 'explode)))))
	        )
	        (progn
	            (setq lay (vla-get-layer obj)
	                  col (vla-get-color obj)
	                  lin (vla-get-linetype obj)
	            )
	            (foreach att (vlax-invoke obj 'getattributes)
	                (if (vlax-write-enabled-p att)
	                    (progn
	                        (if (= "0" (vla-get-layer att))
	                            (vla-put-layer att lay)
	                        )
	                        (if (= acbyblock (vla-get-color att))
	                            (vla-put-color att col)
	                        )
	                        (if (= "byblock" (strcase (vla-get-linetype att) t))
	                            (vla-put-linetype att lin)
	                        )
	                    )
	                )
	                (if (= :vlax-false (vla-get-invisible att))
	                    (   (if (and (vlax-property-available-p att 'mtextattribute) (= :vlax-true (vla-get-mtextattribute att)))
	                            LM:burst:matt2mtext 
	                            LM:burst:att2text
	                        )
	                        (entget (vlax-vla-object->ename att))
	                    )
	                )
	            ) ; foreach

	            (foreach new lst
	                (if (vlax-write-enabled-p new)
	                    (if (= "AcDbAttributeDefinition" (vla-get-objectname new))
	                        (vla-delete new)
	                        (progn
	                            (if (= "0" (vla-get-layer new))
	                                (vla-put-layer new lay)
	                            )
	                            (if (= acbyblock (vla-get-color new))
	                                (vla-put-color new col)
	                            )
	                            (if (= "byblock" (strcase (vla-get-linetype new) t))
	                                (vla-put-linetype new lin)
	                            )
	                            (LM:burstnested new)
	                        )
	                    )
	                )
	            )
	            (vla-delete obj)
	        )
	    )
	   ; )
    ;)
    (princ)
)

(defun LM:burst:removepairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (member (car x) itm)) lst)
)

(defun LM:burst:remove1stpairs ( itm lst )
    (vl-remove-if '(lambda ( x ) (if (member (car x) itm) (progn (setq itm (vl-remove (car x) itm)) t))) lst)
)
  
(defun LM:burst:att2text ( enx )
    (entmakex
        (append '((0 . "TEXT"))
            (LM:burst:removepairs '(000 002 070 074 100 280)
                (subst (cons 73 (cdr (assoc 74 enx))) (assoc 74 enx) enx)
            )
        )
    )
)

(defun LM:burst:matt2mtext ( enx )
    (entmakex
        (append '((0 . "MTEXT") (100 . "AcDbEntity") (100 . "AcDbMText"))
            (LM:burst:remove1stpairs  '(001 007 010 011 040 041 050 071 072 073 210)
                (LM:burst:removepairs '(000 002 042 043 051 070 074 100 101 102 280 330 360) enx)
            )
        )
    )
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

(setq ind 0)
(defun progress(/ chars)
    (setq chars '("-" "\\" "|" "\/"))

    ;(repeat 40
        (princ (strcat "\rPlease wait... " (nth ind chars)))
        (setq ind (rem (1+ ind) (length chars)))
    ;)
    (princ)
)

(vl-load-com) (princ)