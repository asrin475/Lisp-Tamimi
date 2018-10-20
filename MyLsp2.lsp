; ***************************************************
; MyLisp.lsp is a collection of tools that make autocad ease and it is created and maintained by Mohamedh Asrin
; &copy All Right Reserved 2017 01 18  12:16 AM
;
;
;
(setq MYVER "v1.3 alpha 7 - bumble bee")
#| ; ***************************************************
;	CHANGES
; ***************************************************
;	Initial Release 1.0
;
;	2017-01-24
;	improved ml trim function - MJ
;	MLine Weld - WA
;
;	1.1.0
;
;	Selection of blocks is always top to bottom
;	No matter how the selection is made
;
;	1.1.2
;
;	fix - Object snap variable is not reverted to backup in command r2
;
;	1.2.1
;
;	** pc command has been redesiged
;	 * fix - r2 command cmdecho and osmode not restoring
;
;	1.2.5
;
;	** p2j - now prints to jpeg
;	 * the same set of models can now print to other printers as well (limit_thrushold more than 5 blocks)
;	 *
;
; 	1.3
;
; 	** I don't know what else has been edited
;	
;	******** stil i can't remember, edited today also
;
; ***************************************************
|#

;LOADS VLA COMPONENTS
(vl-load-com)
(load ".\\progress.lsp")
(setq acApp (vlax-get-acad-object))
(setq acDoc (vla-get-ActiveDocument acApp))
(setq acSpace (vla-get-ModelSpace acDoc)

	;GLOBAL PRINTERS
	pXerox "Xerox.pc3"
	hp7510 "Officejet 7510 series.pc3"
	p8600 "Officejet8600.pc3"
	hp400 "HP400.pc3"
	hp7610 "HP7610.pc3"
	Robot "Robot.pc3")

; PRINTERS AND THE DEFAULT PAPER SIZES
(setq $G_PRINTERS
	'(
		(XEROX ("Xerox.pc3" "A3 (297 x 420mm)"))
		(HP400 ("HP400.pc3" "A4"))
		(HP7510 ("Officejet 7510 series.pc3" "A3 297x420mm - plain paper"))
		(ROBOT ("Robot.pc3" "A3"))
		(P8600 ("Officejet8600.pc3" ""))
		(PDF ("DWG To PDF.pc3" "ISO full bleed A3 (297.00 x 420.00 MM)"))
		(JPEG ("PublishToWeb JPG.pc3" "TO_JPEG_3200X2560")) ; the paper size for jpeg is a userdefined size
	)

	$G_STYLES
	'(
		(COLOR "for print.ctb")
		(BLACK "for black.ctb")
		(JPEG "TO-JPEG.ctb")
		(CROKY "croky.ctb")
	)

	$G_PRINTING_METHOD
	'(
		(FACE_UP "fup")
		(FACE_DOWN "fdown")
	)
)

(setq $TAMIMI "TAMIMI"
		$CORDS "COORDINATES")

;PRINTER SUPPORT FUNCTIONS
(defun $get_printer(printer_name /)
	(car (cadr (assoc printer_name $G_PRINTERS))))

(defun $get_paper_for_printer(printer_name /)
	(cadr (cadr (assoc printer_name $G_PRINTERS))))

(defun $get_printing_style(style_name /)
	(cadr (assoc style_name $G_STYLES)))

(defun $get_priniting_method(method_id /)
	(cadr (assoc method_id $G_PRINTING_METHOD)))

;setting up printer
(setq pHp Robot)

(setq blocksToSelect
    '((-4 . "<OR")
    	(-4 . "<AND")
    		(0 . "INSERT")
		    (-4 . "<OR") ;ADD TITLE BLOCKS NAME WITH THE FOLOWING PATTERN TO BE SELECTED.
		        (2 . "vvvvvvvv") (2 . "ba") (2 . "rerere") (2 . "HEAT_SCH") (2 . "bantha") (2 . "wefgetttttttt") (2 . "ERGWERGERG")
		        (2 . "wergwergwergr") (2 . "TAMIMI-*") (2 . "erhgrethrt")
		        (2 . "tr") (2 . "FI SHEET") (2 . "block") (2 . "sh-2222")
		    (-4 . "OR>")
    	(-4 . "AND>")

    	(-4 . "<AND")
    		(0 . "LWPOLYLINE") (8 . "WALL") (-4 . ">") (43 . 0.06) ; cover page contains this outer line
    	(-4 . "AND>")
	(-4 . "OR>"))	;LIST
) ;SETQ

; ERROR HANDLER FOR THIS LSP
(defun *error* (msg / cancel)
  (if (not (member msg '("Function cancelled" "quit / exit abort")))
    (progn
    	(print msg)
    	(ODCL:closeProgress)
    )

  	(progn
  		(ODCL:closeProgress)
  		;(vla-EndUndoMark acDoc)
  	)
  )
)

(defun c:ql(/ lp alp ch)

	(setq TAG:ATTR "STAIR_NO")
	(setq alp '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))

	(defun p:letter(ch / )
		(princ "Prepating Letters...\n") (princ)
		(if (not *letter_pos*) (setq *letter_pos* 0))
		(if (member ch alp)
			(setq *letter_pos* (get-position ch alp)))

		(princ (strcat ">> Starts from : "  (nth *letter_pos* alp)))

		(while
			(setq ent (car (entsel "\nSelect Attribute: ")))
	 		(if (not ent)
	 			(p:letter (nth *letter_pos* alp))
	 			(progn
	 			;CONVERT ENTITY TO VLA OBJECT
	 			(setq ve (vlax-ename->vla-object ent))
 
	 			(mapcar '(lambda (at)
					(if (= (vla-get-tagstring at) TAG:ATTR)
							(progn
								(vla-put-TextString at (nth *letter_pos* alp))
								(setq *letter_pos* (1+ *letter_pos*))

							(if (= *letter_pos* 26) (setq *letter_pos* 0)))
						))
	 				; applying fucntion to
	 				(vlax-invoke ve 'GetAttributes))
	 		))
 		)
	)



	(defun getLetterPos(/ input)
		(setq input (getstring "Type Letter / Number : "))

		(if (AND (/= (atoi input) 0) (<= (atoi input) 26))
	        (progn
	          (print "number")
	          (setq *letter_pos* (1- (atoi input)))
	          (p:number)
	        )
	        (progn
	        	(print (strcase (substr input 1 1)))
	          (p:letter (strcase (substr input 1 1)))
	        )
		)


	)

	(getLetterPos)
	;(exit)
)

(defun c:E2(/ lp num)

	(setq ATTRTAG:STRING "W1")

	(defun modeContinuous(/ obj)

		(setq num (1- (atoi ATTRNUMBER)))
		(print)
		(while
			(setq block (car (entsel "\rSelect Object <= Incremental: ")))
	 		(if (OR (= nil block)
	 				(/= (vla-get-objectname (setq obj (vlax-ename->vla-object block))) "AcDbBlockReference")) ;OR
	 			(progn
	 				(princ "\nInvalid attibute\n")
	 				(modeContinuous)
	 			)
	 			;else
	 			(progn
	 				(setq num (1+ num))
		 			(mapcar
		 				'(lambda (attr)
							(if (= (vla-get-tagstring attr) ATTRTAG:STRING)
								(vla-put-TextString attr (strcat "W-" (itoa num)))
							)
						)
		 				(vlax-invoke obj 'GetAttributes)
		 			) ;mapcar

		 			(setq ATTRNUMBER (itoa num))
	 			) ;progn
	 		) ;if
 		) ;while
		(exit)
	) ;defun

	(defun modeMultiple()
		(setq winTags (ssget '((0 . "INSERT")(8 . "Win-no"))))
		(if (> (sslength winTags) 0)
			(progn
				(print "foreach")
				(foreach tag (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr (ssnamex winTags))))
					(PasteWord tag)
				);foreach
			);progn
		);if
		(princ)
		(getWord)
	)

	(defun PasteWord(obj /)

		(progn
 			(mapcar
 				'(lambda (attr)
					(if (= (vla-get-tagstring attr) ATTRTAG:STRING)
						(vla-put-TextString attr (strcat "W-" ATTRNUMBER))
					)
				)
 				(vlax-invoke obj 'GetAttributes)
 			) ;mapcar
		) ;progn
	) ;defun

	(defun modeSingle(/)

		(princ)
	)

	(defun getWord(/ input)
		(if (= nil ATTRNUMBER) (setq ATTRNUMBER "1"))
		(setq input (getstring (strcat "\nSpecify Window number <W-" ATTRNUMBER ">: ")))
		(if (/= input "")
			(setq ATTRNUMBER input))
		(getInput)
	)

	(defun getInput(/ ve)
		;(print)
		(initget "C M")
		(setq input (entsel "\rSelect Window Attribute or [Continuous / Multiple]: "))

		(cond
			((= input "C") (modeContinuous))
			((= input "M") (modeMultiple))
			((if (and
		 				(/= input nil)
		 				(setq ve (vlax-ename->vla-object (car input)))
		 				(= (vla-get-objectname ve) "AcDbBlockReference"))
				(PasteWord ve))
			)
		)
		(getInput)
		(princ)
	)

	(getWord)
	;(exit)
)


(defun c:ww(/ window_objects output_file ec)

	(setq output_file (strcat "C:\\Users\\Asrin\\Desktop\\" (getvar 'dwgname))
		ec (getvar 'cmdecho))
	;(setvar 'cmdecho 0)

	(if (setq window_objects (ssget))
		(progn
			(if (not (findfile output_file))
					(progn
						(command "-WBLOCK" output_file "" "0,0" window_objects "")
						(princ (strcat "Block created : " output_file)))
				)
		)
		(progn
			(princ "No objects have been selected!")
			(exit))
	)
	(princ)
)

; *******************************************************************
; COMMAND : DD
; Renaming Electrical Panel Board name when they are too lot to handle
; *******************************************************************

(defun c:dd( / p q lst obj ent i j k apply input ipre)

	(setq alp '( "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
		ipre "")

	(if (not *pre*)
		(setq *pre* "PANEL ")
		(progn
			(princ)
			(princ (strcat "Prefix : " *pre*))
		))

	; checking value of k
	(if (OR (not *k*) (= -1 *k*) (> *k* 25))
		(setq *k* 0))

	(defun getPosition ( / input)
			(setq input (getstring (strcat "\nSEtting \/ Starts with <" (itoa (1+ *k*)) "><" (nth *k* alp) ">: ")))
			(if (= input "")
			    (progn ; no input is detected
					  ;(setq *k* *k*)
			      ;(print "no input")
			    )
			    (progn
			      ;input is a number
			      	(if (= (strcase (substr input 1 2) nil) "SE")
			      		(getPrefix)
			      		(progn
			      			(if (AND (/= (atoi input) 0) (<= (atoi input) 26))
						        (progn
						          (print "number")
						          (setq *k* (1- (atoi input)))
						        )
						        (progn
						          (if (member (strcase input) alp)
						            (setq *k* (get-position (strcase input) alp))
						            (setq *k* 0)
						          )
						        )
						    )
			      		)
			      	)
			    )
			)

		)

	(defun getPrefix ()
			;(initget "PANEL PANEL- P-")
			(setq txt (getstring T (strcat "\nPrefix [Type any text] <" *pre* ">: ")))
			(if (/= txt "")
				(setq *pre* txt))

			(print *pre*) (princ)
			(getPosition)
		)

	;(initget 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)

	;getting input
	(getPosition)

  ; GET NUMBER OR CHAR AS A INPUT

	;(print (strcat "-> " input))
  ;(princ)


	(print (strcat "Begining from: " (itoa (1+ *k*)) " -> " (nth *k* alp)))
	(princ)

	;(prompt "\nSelect objects with order:")
	(if (= 1 1)
		(progn
			;(princ (sslength lst))
			(setq i 0
				;j (sslength lst)
				)

			(while (setq lst (ssget "_+.:E:S" (list '(0 . "TEXT,MTEXT"))))
				(setq obj (ssname lst 0))
				(setq p (vlax-ename->vla-object obj))
				;(setq q (vlax-get p 'TextString))
				;( (  (nth i alp)))
				;get rid of out of bound index
				(if (> *k* 25)
          (progn
              (setq index (/ *k* 25))
              (setq ipre (nth (1- index) alp))
              (setq *k* 0)
            )
					)

				(vlax-put p 'TextString (strcat *pre* "" ipre (nth *k* alp)))
				;(vlax-put p 'TextString (strcat (itoa *k*))) ; REPLACE NUMBERS ONLY
				(setq i (1+ i)
					*k* (1+ *k*))
        ; adding 1 to the k will start
			) ; while
			(princ)
		) ; progn
		(princ)
	) ; if


;(prompt "\nSelect objects with order:")
	; (if (setq lst (ssget (list '(0 . "TEXT,MTEXT"))))
	; 	(progn
	; 		(princ (sslength lst))
	; 		(setq i 0
	; 			j (sslength lst))
	; 		(while (< i j)
	; 			(setq obj (ssname lst i))
	; 			(setq p (vlax-ename->vla-object obj))
	; 			;(setq q (vlax-get p 'TextString))
	; 			;( (  (nth i alp)))
	; 			;get rid of out of bound index
	; 			(if (> *k* 25)
 ;          (progn
 ;              (setq index (/ *k* 25))
 ;              (setq ipre (nth (1- index) alp))
 ;              (setq *k* 0)
 ;            )
	; 				)

	; 			(vlax-put p 'TextString (strcat pre " " ipre (nth *k* alp)))
	; 			(setq i (1+ i)
	; 				*k* (1+ *k*))
 ;        ; adding 1 to the k will start
	; 		) ; while
	; 		(princ)
	; 	) ; progn
	; 	(princ)
	; ) ; if


	(princ)
	(exit)
)

(defun c:ff( / alp a)
  (setq alp '( "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
  (print (get-position "R" alp))
  (princ)
  )

(defun get-position(ch lst / l i ret)
  (setq l (length lst)
      i 0)
  ;(print l)
  (while (< i l)
      (if(= (nth i lst) ch)
        (progn
          (setq ret i)
          ;(print "got it")
        )
      )
      (setq i (1+ i))
    )
  (* ret 1)
  ;(princ)
  )


; *******************************************************************
; Removes All Dimensions / by the specified value
; *******************************************************************

(defun c:rd ( / dims ans num)
	;"Documentation for c:rd."
	;(SETQ dims (ssget '((0 . "DIMENSION"))))
	(SETQ ans (strcase (getstring "\nRemove Dimensions (All / Enter Value) <All>: ")))
  (if (= ans "") (setq ans "A"))
  (cond
    ( ; cond 1
      (= ans "A")
        (progn (SETQ dims (ssget '((0 . "*DIMENSION"))))
            (if dims (command "_erase" dims ""))
        )
    )
    ( ; cond 2
      (> (atof ans) 0.0)
      (progn
         (setq num (atof ans)
         dims (ssget (list '(0 . "*DIMENSION")
          '(-4 . "<AND")
              '(-4 . ">") (cons 42 (- num 0.049))
              '(-4 . "<") (cons 42 (+ num 0.049))
          '(-4 . "AND>")
          )))
            (if dims (command "_erase" dims ""))
        )



      ; (setq num (atof ans)
      ;    dims (ssget (list '(0 . "*DIMENSION")
      ;     '(-4 . "<AND")
      ;         '(-4 . "<OR") '(-4 . "=") (cons 42 num) '(-4 . ">") (cons 42 num) '(-4 . "OR>")
      ;         '(-4 . "<") (cons 42 (+ num 0.001))
      ;     '(-4 . "AND>")
      ;     )))
      ;(PRINT num)
    )
    (progn
      (princ "\n*** Invalid Number on Input")
    )
  )
	(princ)
);defun rd


(defun c:mb(/)


	(defun makeBlock(/ blocks base_point current_block_name)
		(if (and
				(setq blocks (ssget))
				(setq base_point (getpoint "Pick base point:")))
			(progn
				(setq current_block_name (getAvailableBlockName))
				;(princ current_block_name)
				(command "-block" current_block_name base_point blocks "")
				(insertBlock current_block_name base_point)
				(princ (strcat "\nBLOCK NAME: " current_block_name "\n"))
			)
		)
		(princ)
	)


	(defun getAvailableBlockName( / dwg block_name num current_block name_limit_thrushold)

		(setq dwg (getvar 'dwgname)) ; example.dwg
 		(setq block_name (strcat "BLOCK_" (strcase (substr dwg 1 (- (strlen dwg) 4)) nil) "_")) ; BLOCK_EXAMPE_
 		(setq num 1 name_limit_thrushold 10)

		(repeat name_limit_thrushold

			(if (= (blockAvailable (strcat block_name (itoa num))) nil)
				(progn
					(setq current_block (strcat block_name (itoa num)))
					current_block
				)
				(setq num (1+ num))
			)

		)
	)

	(makeBlock)
	(princ)
)

; common lisp routine to use both functions
	(defun blockAvailable(name / available)
		(setq available (tblsearch "block" name))
		available
	)

	(defun insertBlock(name point /)
		(vla-InsertBlock acSpace (vlax-3d-point point) name 1 1 1 0)
	)

(defun c:rb(/ current_block)
	(defun draw_Rectangle()
		;gets the last created block
		(makeBlock)
		(setq current_block (entlast))
		;(print "current block")
		(if (/= nil current_block)
			(progn 
				(setq pointers (getBoundingBox current_block))
				(print pointers)
				(command "_rectangle" "_non" (car pointers) "_non" (cadr pointers))
				(command "_explode" current_block)
			)
		)
	)

	(defun makeBlock(/ objects base_point current_block_name)
		(setq current_block_name "tamimi_temp"
			 objects (ssget '((8 . "WAL"))))
		(setq base_point (getpoint "Pick base point:"))

		(if (/= (blockAvailable current_block_name) nil)
			(command "-purge" "B" current_block_name "N" ""))

		(command "-block" current_block_name base_point objects "")
		(insertBlock current_block_name base_point)
		;now the block is inserted and it can be called by entlast
	)

	(defun getBoundingBox(obj /)
		(vla-getboundingbox (vlax-ename->vla-object obj) 'p1 'p2)
		(setq cords (list p1 p2))
		(setq cords (mapcar 'vlax-safearray->list cords))
		cords
	)

	(draw_Rectangle)
	(princ)
)


(defun c:mb2(/ asset)
	;make the selection
	(setq asset (ssget) back asset QF (getvar 'qaflags))

	(SETQ attbs (ssget "_P" '(
     (-4 . "<OR")
      (0 . "TEXT") (0 . "*DIMENSION")
      	(-4 . "<AND") (0 . "INSERT")
      		(-4 . "<OR") (2 . "BLOCK_STAIR_NO") (2 . "dtag") (2 . "A$C23000B7B") (-4 . "OR>")
		(-4 . "AND>")
     (-4 . "OR>")
    )))

    (if attbs
    	(progn
    		(princ (strcat "Deleting " (itoa (sslength attbs)) " blocks..."))
    		(command "_erase" attbs "")
    	) (print "No Attributes")
    )
    ;(sssetfirst nil asset)


    (setvar "qaflags" 1)

    (princ "Exploding objects... phase 1")
    (command "_explode" asset "")
    (print (itoa (sslength asset)))

    (setvar "qaflags" 0)

    (SETQ doortags (ssget "_P" '(
		     (-4 . "<OR")
		      (0 . "TEXT")
		      (-4 . "<AND") (0 . "INSERT") (8 . "door") (-4 . "AND>")
		     (-4 . "OR>")
		    )))


    (if doortags
    	(progn
    		(princ "Deleting door tags...")
    		(command "erase" doortags "")
    	)
    )

	(setvar "qaflags" 1)
    (princ "Exploding objects... phase 2")
    (sssetfirst nil asset)
    (command "_explode" asset "")
	(setvar "qaflags" 0)

(print (itoa (sslength asset)))
	(princ "Changing Color to 8...")
		     (setq i 0 ss (ssadd))
		     (repeat (sslength asset)
		     	(if (/= (entget (ssname asset i)) nil)

		     				(setq ss (ssadd (ssname asset i) ss))

		     		)

		     	(setq i (1+ i))
		     )
		     	(print (itoa (sslength ss)))
	(command "_chprop" ss "" "_color" 8 "")
	(command "_select" ss)
	(setvar 'qaflags 0)
)


; *******************************************************************
; helps to erase objects like text, door tags when creating block
; for layout other than architecture
; *******************************************************************

(defun c:rx ( / blocks ans)
  ;"Documentation for c:rd."
  	(SETQ blocks (ssget '(
    	(-4 . "<OR")
     		(0 . "TEXT")
     		(8 . "Win-no")
     		(8 . "DTAG")
      		(-4 . "<AND")
      			(0 . "INSERT") (8 . "door")
      		(-4 . "AND>")
      		(-4 . "<AND")
      			(0 . "CIRCLE") (8 . "DIM")
 			(-4 . "AND>")
 		(-4 . "OR>")
    )))



  ;(SETQ ans (strcase (getstring "\nWould you like to Remove <Y>: ")))
  (if blocks (command "_.erase" blocks ""))
  (princ)
);defun


(defun c:ln(/ obj length_all)
	(setq obj (ssget '((0 . "*LINE"))))
	(setq length_all
		(mapcar 'vla-get-length
			(mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr (ssnamex obj)))))
	)
	(princ (strcat "\nTotal Length: " (rtos (apply '+ length_all)) " m."))
	(princ)
)

(defun c:pp(/ area ground perc output)

	(defun get(obj / rtn)
		(princ (vla-get-TextString obj))
		(setq rtn (atof (vl-string-subst "" "\\pxqj;" (vla-get-TextString obj))))
	)

	(if
		(AND
			(vlax-property-available-p (setq area (vlax-ename->vla-object (car (entsel "Area: ")))) 'TextString)
			(vlax-property-available-p (setq ground (vlax-ename->vla-object (car (entsel "Ground: ")))) 'TextString)
			(vlax-property-available-p (setq perc (vlax-ename->vla-object (car (entsel "Output: ")))) 'TextString)
		)
		(progn
			(print (strcat "Area:" (rtos (get area)) "\n" "Ground:" (rtos (get ground)) "\n"))
			(setq output (/ (* (get ground) 100) (get area)))

			(vla-put-TextString perc (strcat (rtos output 2 0) " %"))
		)
	)
	(princ)
)


; *******************************************************************
; Draw polylines along with hatch bounderies
; *******************************************************************

(defun c:hp ( / e i l s lsorted)

    (vla-StartUndoMark acDoc)
    (if (setq s (ssget '((0 . "HATCH"))))
        (repeat (setq i (sslength s))
            (setq e (ssname s (setq i (1- i)))
                  l (entget e)
                  lsorted (vl-remove-if-not '(lambda (x)
                  	(and
                  		(= 10 (car x)) (not (= (car (cdr x)) 0.0))))
                  	 l) ; it's been sorted to eleminate other eliments
            	;lsorted (reverse (cdddr (reverse lsorted)))
            )

            ;(princ (cdr (car lsorted)))
            (princ lsorted)
            ;(princ (car (cdr (car lsorted))))

            (apply 'command
                (append '("_.pline")
                    (apply 'append
                        (mapcar
                            (function
                                (lambda ( x )
                                    (list "_non" (trans (cdr x) e 1))
                                )
                            )
                            lsorted
                        )
                    )
                    (list (cdr (car lsorted)))
                    (if (= 1 (logand 1 (cdr (assoc 70 l)))) '("_C") '(""))
                )

            )
        )
    )
    (vla-EndUndoMark acDoc)
    (princ)
)


; *******************************************************************
; Draw Multilines over polylines
; *******************************************************************

(defun c:mh ( / e i l s acadObj doc)

    (vla-StartUndoMark acDoc)
    (if (setq s (ssget '((0 . "LWPOLYLINE") (-4 . "<NOT") (1 . "ARC") (-4 . "NOT>"))))
        (repeat (setq i (sslength s))
            (setq e (ssname s (setq i (1- i)))
                  l (entget e)
            )
            (apply 'command
                (append '("_.mline")
                    (apply 'append
                        (mapcar
                            (function
                                (lambda ( x )
                                    (list "_non" (trans (cdr x) e 1))
                                )
                            )
                            (vl-remove-if-not '(lambda ( x ) (= 10 (car x))) l)
                        )
                    )
                    (if (= 1 (logand 1 (cdr (assoc 70 l)))) '("_C") '(""))
                )
            )
        )
    )
    (vla-EndUndoMark acDoc)
    (princ)
)


(defun c:mj ( / cm e1 e2 i1 i2 il m1 m2 o1 o2 p1 ss ml_start ml_end ml_cir_int ml_cross_line)
    #| (setq acApp (vlax-get-acad-object))
    (setq acDocs (vla-get-ActiveDocument acApp))
    (setq acSpace (vla-get-ModelSpace acDocs)) |#

    ; start undo mark
    (vla-StartUndoMark acDoc)
    (if (setq ss (ssget "_:L" '((0 . "MLINE"))))
        (progn
            (setq cm (getvar 'cmdecho))
            (setvar 'cmdecho 0)
            (repeat (setq i1 (sslength ss))
                (setq e1 (ssname ss (setq i1 (1- i1)))
                      o1 (vlax-ename->vla-object e1)
                      ml_start (car (group3 (vlax-get o1 'coordinates))) ; ml start coordinates
                      ml_end (last (group3 (vlax-get o1 'coordinates)))
                )
                (repeat (setq i2 i1)
                    (setq e2 (ssname ss (setq i2 (1- i2)))
                          o2 (vlax-ename->vla-object e2)
                          il (group3 (vlax-invoke o1 'intersectwith o2 acextendnone))
                    )

                    (cond
                        (   (= 1 (length il))
                            (command "_.-mledit" "_CJ" (list e1 (car il)) (list e2 (car il)) "")
                        )
                        (   (= 4 (length il))
                            (command "_.-mledit" "_OC" (list e1 (car il)) (list e2 (cadr il)) "")
                        )
                        (   (= 2 (length il))
                            (setq p1 (car il)
                                  m1 (group3 (vlax-get o1 'coordinates))
                                  m2 (group3 (vlax-get o2 'coordinates))
                            )
                            (if (<  (min (distance p1 (car m1)) (distance p1 (last m1)))
                                    (min (distance p1 (car m2)) (distance p1 (last m2)))
                                )
                                (command "_.-mledit" "_OT" (list e1 (car il)) (list e2 (cadr il)) "")
                                (command "_.-mledit" "_OT" (list e2 (car il)) (list e1 (cadr il)) "")
                            )
                        )
                    )
                )
            )
            (setvar 'cmdecho cm)
        )
    )
    (vla-EndUndoMark acDoc)
    (princ)
)


; Trims Multilines less intelligently -MJ
(defun c:mmj ( / cm e1 e2 i1 i2 il m1 m2 o1 o2 p1 ss ml_start ml_end ml_cir_int ml_cross_line ml_cord)

    (vla-StartUndoMark acDoc)
    (if (setq ss (ssget "_:L" '((0 . "MLINE"))))
        (progn
            (setq cm (getvar 'cmdecho))
            (setvar 'cmdecho 0)
            (repeat (setq i1 (sslength ss))
                (setq e1 (ssname ss (setq i1 (1- i1)))
                      o1 (vlax-ename->vla-object e1)
                      ml_start (car (group3 (vlax-get o1 'coordinates))) ; ml start coordinates
                      ml_end (last (group3 (vlax-get o1 'coordinates)))
                      ml_cord '()
                      ml_cord (cons ml_start ml_cord)
                      ml_cord (cons ml_end ml_cord)
                )
                (repeat (setq i2 i1)
                    (setq e2 (ssname ss (setq i2 (1- i2)))
                          o2 (vlax-ename->vla-object e2)
                          il (group3 (vlax-invoke o1 'intersectwith o2 acextendnone))
                    )

                    ;(princ (strcat "\nINTERSECT: " )) (princ il)
                    (princ ml_cord)
                    (foreach cor ml_cord
                      (progn
                        (if (AND (/= il nil) (not (member cor il)) (>= (distance cor (car il)) 0.15))
                            (progn
                              (setq tempcircle (vla-addcircle acSpace (vlax-3d-point (trans cor 0 1)) 0.1)
                                    ml_cir_int (group3 (vlax-invoke o1 'intersectwith tempcircle acextendnone))
                                )
                             (if ml_cir_int
                                (setq ml_cross_line (vla-addline acSpace (vlax-3d-point (car ml_cir_int)) (vlax-3d-point (last ml_cir_int))))
                              )
                              ;(princ (car ml_cir_int))
                              ;(princ (last ml_cir_int))
                              (vla-delete tempcircle)
                            )
                          )


                        )
                    )

                    (cond
                        (   (= 1 (length il))
                            (command "_.-mledit" "_CJ" (list e1 (car il)) (list e2 (car il)) "")
                        )
                        (   (= 4 (length il))
                            (command "_.-mledit" "_OC" (list e1 (car il)) (list e2 (cadr il)) "")
                        )
                        (   (= 2 (length il))
                            (setq p1 (car il)
                                  m1 (group3 (vlax-get o1 'coordinates))
                                  m2 (group3 (vlax-get o2 'coordinates))
                            )
                            (if (<  (min (distance p1 (car m1)) (distance p1 (last m1)))
                                    (min (distance p1 (car m2)) (distance p1 (last m2)))
                                )
                                (command "_.-mledit" "_OT" (list e1 (car il)) (list e2 (cadr il)) "")
                                (command "_.-mledit" "_OT" (list e2 (car il)) (list e1 (cadr il)) "")
                            )
                        )
                    )
                )
            )
            (setvar 'cmdecho cm)
        )
    )
    (vla-EndUndoMark acDoc)
    (princ)
)

; function used by the mline trim command
(defun group3 (l / r)
    (repeat (/ (length l) 3)
        (setq r (cons (list (car l) (cadr l) (caddr l)) r)
              l (cdddr l)
        )
    )
    (reverse r)
)


; *******************************************************************
; Weld Multiline when they're broken
; *******************************************************************

(defun c:wa(/ ss i1 e1 o1 m1)
  (if (setq ss (ssget "_:L" '((0 . "MLINE"))))
    (progn
        ;(setq cm (getvar 'cmdecho))
        ;(setvar 'cmdecho 0)
        (repeat (setq i1 (sslength ss))
          ;(print (itoa i1))
            (setq e1 (ssname ss (setq i1 (1- i1)))
              o1 (vlax-ename->vla-object e1)
              m1 (group3 (vlax-get o1 'coordinates))
            )

            ;(princ
            (command "_.-mledit" "_WA" (list e1 (car m1)) (list e1 (last m1)) "")
        );repeat
    );progn
  )
  (princ)
)




; *******************************************************************


; *******************************************************************
; change the dimension style and the layer.
(defun ld(ans / prev text ans layer)
	;(initget "i o")
	;(setq ans (strcase(getstring "\nChoose [Inner/Outer] <Inner>: ")))
  (setq layer "DIM")
	(if (= ans "") (setq ans "I"))

	(if (= ans "I")
      (progn
        (princ "\n*** Inner Dimension")
        (_SetDimStyleCurrent "al-tamimi- new dim")
  		) ; progn

  		(progn
        (princ "\n*** Outer Dimension")
        (_SetDimStyleCurrent "NO XL")
      )
	)
	(setq prev (getvar "CLAYER"))
	(setvar "CLAYER" layer)
	(command "_.-layer" "ON" layer "")
	(princ (strcat "\n*** LAYER CHANGED: " prev "->" layer))
	(princ)
)

(defun c:ldl(/ dims dimstyle)

	(setvar 'cmdecho 0)
	(defun selDims()
		(prompt "Select Dimension")
		(if (setq dims (ssget ":S" '((0 . "DIMENSION"))))
			(progn
				(setq ent (entget (ssname dims 0)))
				(setq dimstyle (cdr (assoc 3 ent)))
					(princ (strcat "*** DIMESION STYLE : "  dimstyle))
				(_SetDimStyleCurrent dimstyle)
				(command "_.DIMLINEAR")
			)
			(progn
				(princ "*** Select a valid Dimension")
				(selDims)
			)
		)
	)
	(selDims)
	(princ)
)

(defun c:ldi()
  (ld "I")
)(defun c:ldo()
  (ld "O")
)
(defun _SetDimStyleCurrent (dim / )
  (if (tblsearch "DIMSTYLE" dim)
    (vla-put-activeDimstyle
      acDoc
      (vla-item (vla-get-Dimstyles acDoc) dim)
    )
    (alert "Invalid Dimension Style")
  )
  (princ)
)
; *******************************************************************


; *******************************************************************

;executes 3 most important commands on the go
(defun C:PAL (/ file old now)

	(command ".-LAYER" "Set" '0 "")

	(setq old 0 now old
		t_file_has_access T)

	(if (not (setq old (get_file_size))) ;if the file unable to access this makes the var old nil, so arithmatic throws error
		(progn
			(setq t_file_has_access nil)
			(setq old 0)) ; making it nil to zero
	)

	(command ".AUDIT" "Y")
	(princ "*** PURGING...") ; NO USE WONT SHOW IN THE COMMAND LINE
	(command ".PURGE" "All" "" "N")
	(PRINC (strcat "\n*** LOCATION : " (getvar 'dwgprefix)))
	(PRINC (strcat "\n*** SAVING CURRENT DRAWING : " (getvar 'dwgname) " ...")) ; SHOWING CURRENT DRAWING NAME
	(PRINC)
	(command ".QSAVE")
	(princ "\n*** DONE.")

	(if (/= t_file_has_access nil)
		(setq now (get_file_size)))


	(if (> old now)
		(princ (strcat "\n*** FILE REDUCED : " (rtos (- old now)) " KB \\U+2193"))
		(princ (strcat "\n*** FILE INCREASED : " (rtos (- now old)) " KB \\U+2191"))
	)
;(command ".ZOOM" "Extents")
 (princ)) ;Purges all unused symbols






; *******************************************************************
; Converts Text to MText
; *******************************************************************

 (defun c:t2m()
   (if (setq tx (ssget '((0 . "TEXT"))))
     (progn
       (command "txt2mtxt" (ssname tx 0) "")
     )
   )
 )



; *******************************************************************
; CONVERTS EDUCATIONAL DWG -> DXF -> DWG
; Unfortunately it works only on single document mode sdi=1
; *******************************************************************

 (defun c:dxf(/ dxffilename dwg dxf)
 	;(vl-load-com)
 	(setvar "lispinit" 0)         ;LISP-Prg. mem. resistent
 	(setq dwgpath "D:\\DWG\\"
 		dxfpath "D:\\DXF\\")

 	(setq dwg (getvar 'dwgname))
 	(setq dxf (strcat dxfpath (substr dwg 1 (- (strlen dwg) 4)) ".dxf"))
 	(setq dxffilename (strcat (substr dwg 1 (- (strlen dwg) 4)) ".dxf"))

 	(princ)
 	(princ (strcat "\n*** SAVING CURRENT FILE: " dwg))
 	(command "_.QSAVE")

 	(command "dxfout" dxf "16")
 	(princ (strcat  "\n*** DXF FILE HAS BEEN SAVED : " dxf))

 	;(setvar 'sdi 0)
 	(command "_FILEOPEN" dxf)

 	; (setq acApp (vlax-get-acad-object))
 	; (setq acDocs (vla-get-documents acApp))
 	; (vla-open acDocs dxf)


 	(if (= (getvar 'dwgname) dxffilename)
 		(command "_.SAVEAS" "2010" (strcat dwgpath dwg))
 	)
 	;(setvar 'sdi 1)
 	(princ)
 )



; *******************************************************************
; PRINT WITH AL TAMIMI WATERMARK OVER THE CROOCKY
; *******************************************************************

(defun c:pc(/ filewatermark zstyle *error*)

  	(setq
  		acadObj (vlax-get-acad-object)
    	doc (vla-get-ActiveDocument acadObj)
    	modelSpace (vla-get-ModelSpace doc)
    	filewatermark "watermark.dwg")

  	; (defun *error* (msg)
  	; 	(if (wm)
  	; 		(vla-delete wm)
  	; 		)
  	; 	(princ "\nRollback is done!")
  	; 	)

  	(setq
  		orientation "l"
  		styleColor "for print.ctb"
    	styleCroky "croky.ctb")

    ; (setq zstyle "croky.ctb"
    ;     zprinter "DWG To PDF.pc3"
    ;     zpaper "ISO full bleed A3 (297.00 x 420.00 MM)")

    ; DEFAULT VALUES
    (setq zstyle styleCroky
        zprinter pXerox
        zpaper "A3 297x420mm - plain paper")

   (if (setq color (getstring "\nColor Options [Color or Black] <Black>: "))
		(progn
			(if (= (strcase (substr color 1 1) nil) "C")
				(setq zstyle styleColor
					zprinter pHp)
			)
		)
	)

  (if (setq ps (getstring "\nSelect paper size [A4, A3] <A4>: "))
    (if(= (strcase ps nil) "A3")
    	(if (= zstyle styleColor)
    		(setq zpaper "A3 297x420mm - plain paper") ; hp
      		(setq zpaper "A3 (297 x 420mm)") ; Xerox
      	)
      	; else
      	(if (= zstyle styleColor)
      		(setq zpaper "A4") ; for hp
      		(setq zpaper "A4 (210 x 297mm)") ; for xerox
      	)
    )
  )

  (if (setq land (getstring "\nOrientation [Potrait or Landscape] <Landscape>: "))
		(progn
			(if (= (strcase (substr land 1 1) nil) "P")
				(setq orientation "p")
			)
		)
	)

  (princ (strcat "\n" zprinter " | " zpaper " | "  zstyle " | " (if (= orientation "l") "Landscape" "Potrait")))

  ;(if (setq box (ssget))
  (setq minExt (getpoint "\nSelect First Point: "))
  (print minExt)
  	;(initget 32)
      (setq maxExt (getcorner minExt "\nSelect Second Point: "))
  (print maxExt)

  (if (= 1 1)
    (Progn
      ;(setq box (ssname box 0))
      ;(command "zoom" "_object" box "")
      ;(setq bc (getvar 'viewctr))
      ;(command "zoom" "p")
      ;(princ bc)

      (if (not (findfile filewatermark))
        (progn
          (alert "Watermark dwg file is not available in the working directory.")
          (exit)
        )
      )

      ;GETTING UPPER AND LOWER POINTS

      ;CENTER OF BOTH POINTS
      (setq cpoints (mapcar '(lambda (p1 p2) (/ (+ p1 p2) 2.0)) minExt maxExt))
      (print cpoints)

      (setq wm (vla-InsertBlock modelSpace (vlax-3d-point cpoints) filewatermark 1 1 1 0))
      (command "zoom" "_object" (vlax-vla-object->ename wm) "")

      ;WATER MARK CENTER POINT
      (setq wmc (getvar 'viewctr))

      ;(print wmc)
      (command "zoom" "p")
      (command "_.DRAWORDER" (vlax-vla-object->ename wm) "" "_back")
      (vla-move wm (vlax-3d-point wmc) (vlax-3d-point cpoints))
      ;(vla-rotate wm (vlax-3d-point bc) 0.7853981) ; value for 45 deg
      (vla-put-layer wm "WATERMARK")

      ;(command "rotate" (vlax-vla-object->ename wm) "" bc 45)

      ;(setq vlbox (vlax-ename->vla-object box))
      ;(vla-GetBoundingBox vlbox 'minExt 'maxExt)
        ;(setq minExt (vlax-safearray->list minExt)
          ;   maxExt (vlax-safearray->list maxExt))
        ; box distance
        (setq dbox (distance minExt maxExt))
        (print (rtos dbox))

        (print minExt)
        (print maxExt)

        ;Watermark region
        (vla-GetBoundingBox wm 'wminExt 'wmaxExt)
        (setq wminExt (vlax-safearray->list wminExt)
             wmaxExt (vlax-safearray->list wmaxExt))

        (setq dwm (distance wminExt wmaxExt))
        (print (rtos dwm))
        (vla-rotate wm (vlax-3d-point cpoints) (angle minExt maxExt))
        (setq sf (/ (- dbox 10) dwm))
        (print (rtos sf))
        (vla-scaleentity wm (vlax-3d-point cpoints) sf)

        ;PLOT
	    (plotter2 zprinter zpaper zstyle minExt maxExt "N" orientation)
    )
  )
  (vla-delete wm)
  (princ "*** PRINT COMPLETED")
  (princ)
)

(defun c:p2j(/ filewatermark zstyle *error* *UniNum* genFilename tofile)

  	(if (not *UniNum*)
  		(setq *UniNum* 0))

  	(defun genFilename()

  		(princ (itoa *UniNum*))
  		(setq loc (getvar 'dwgprefix)
  			i 1
  			fn "")

  		(while (> i *UniNum*)
  			(if (not (findfile (strcat loc (itoa i) ".jpg")))
  				(progn
  					(setq fn (strcat loc (itoa i) ".jpg")
  						*UniNum* i)

  					fn
  				)
  				(setq i (1+ i))
  			)
  		)
  	)

  	(setq tofile (genFilename))

  	(setq
  		acadObj (vlax-get-acad-object)
    	doc (vla-get-ActiveDocument acadObj)
    	modelSpace (vla-get-ModelSpace doc)
    	filewatermark "watermark.dwg")

  	; (defun *error* (msg)
  	; 	(if (wm)
  	; 		(vla-delete wm)
  	; 		)
  	; 	(princ "\nRollback is done!")
  	; 	)

  	(setq
  		orientation "l"
  		styleColor "for print.ctb"
    	styleCroky "croky.ctb"
    	styleJpeg "TO-JPEG.ctb"
    	jpegprinter "PublishToWeb JPG.pc3")

    ; DEFAULT VALUES
    (setq zstyle styleJpeg
        zprinter jpegprinter
        zpaper "TO_JPEG_3200X2560")  ; TO_JPEG_3200X2560 - is predefined under autocad
       ; zpaper "Sun Hi-Res (1600.00 x 1280.00 Pixels)")

  (princ (strcat "\n" zprinter " | " zpaper " | "  zstyle " | " (if (= orientation "l") "Landscape" "Potrait")))

  ;(if (setq box (ssget))
  (setq minExt (getpoint "\nSelect First Point: "))
  (print minExt)
  	;(initget 32)
      (setq maxExt (getcorner minExt "\nSelect Second Point: "))
  (print maxExt)

  (if (= 1 1)
    (Progn
      ;(setq box (ssname box 0))
      ;(command "zoom" "_object" box "")
      ;(setq bc (getvar 'viewctr))
      ;(command "zoom" "p")
      ;(princ bc)

      (if (not (findfile filewatermark))
        (progn
          (alert "Watermark dwg file is not available in the working directory.")
          (exit)
        )
      )

      ;GETTING UPPER AND LOWER POINTS

      ;CENTER OF BOTH POINTS
      (setq cpoints (mapcar '(lambda (p1 p2) (/ (+ p1 p2) 2.0)) minExt maxExt))
      (print cpoints)

      (setq wm (vla-InsertBlock modelSpace (vlax-3d-point cpoints) filewatermark 1 1 1 0))
      (command "zoom" "_object" (vlax-vla-object->ename wm) "")

      ;WATER MARK CENTER POINT
      (setq wmc (getvar 'viewctr))

      ;(print wmc)
      (command "zoom" "p")
      (command "_.DRAWORDER" (vlax-vla-object->ename wm) "" "_back")
      (vla-move wm (vlax-3d-point wmc) (vlax-3d-point cpoints))
      ;(vla-rotate wm (vlax-3d-point bc) 0.7853981) ; value for 45 deg
      (vla-put-layer wm "WATERMARK")

      ;(command "rotate" (vlax-vla-object->ename wm) "" bc 45)

      ;(setq vlbox (vlax-ename->vla-object box))
      ;(vla-GetBoundingBox vlbox 'minExt 'maxExt)
        ;(setq minExt (vlax-safearray->list minExt)
          ;   maxExt (vlax-safearray->list maxExt))
        ; box distance
        (setq dbox (distance minExt maxExt))
        (print (rtos dbox))

        (print minExt)
        (print maxExt)

        ;Watermark region
        (vla-GetBoundingBox wm 'wminExt 'wmaxExt)
        (setq wminExt (vlax-safearray->list wminExt)
             wmaxExt (vlax-safearray->list wmaxExt))

        (setq dwm (distance wminExt wmaxExt))
        (print (rtos dwm))
        (vla-rotate wm (vlax-3d-point cpoints) (angle minExt maxExt))
        (setq sf (/ (- dbox 10) dwm))
        (print (rtos sf))
        (vla-scaleentity wm (vlax-3d-point cpoints) sf)

        ;PLOT
        (princ (strcat "####################" tofile))
        (princ)
	    (plotterj zprinter zpaper zstyle minExt maxExt tofile)
    )

  )
  (vla-delete wm)
  (princ "*** PRINT COMPLETED")
  (princ)
)

; *******************************************************************
; FINAL PRINTINGS
; *******************************************************************

(defun a2(zprinter zpaper zstyle plottype / msg plotOrder)
  (setq plotOrder "")
  (cond
      ((OR (eq plottype 'pdf) (eq plottype 'black)) (setq plotOrder "BOTTOM - TOP"))
      ((= plottype 'colour) (setq plotOrder "TOP - BOTTOM"))
  )

	(setq msg (strcat "*** BALADIYA CHECK LIST ***\n"
		"\n1. Site Plan Values & Scale (1:150)"
		"\n2. Section Lines & Section (a-a),(b-b)"
		"\n3. Elevation (4)"
		"\n4. Title Block & Titles"
		"\n\n"

   		"*** Customer Name ***\n"
     	"___________________________________________\n"
		"\n| Printer: " zprinter
		"\n| Paper: " zpaper
		"\n| Style: " zstyle
     	"\n___________________________________________"
		))
	(alert msg)
)
; PDF
(defun c:p2p(/ zpaper zstyle)
	;Current_printer_identifier
	(setq cpi 'PDF)

	(if (setq color (getstring "\nColor or Black <Color>: "))
		(progn
			(setq color (strcase color nil)) ; making uppercase

			(if (OR (= color "B") (= color "BLACK"))
				(setq zstyle ($get_printing_style 'black))
				(setq zstyle ($get_printing_style 'color))
			)
		)
	)

	;(princ "\n**************************** [ BOTTOM -> TOP ] ****************************")
	(p2 ($get_printer cpi) ($get_paper_for_printer cpi) zstyle 'pdf nil :vlax-false :vlax-true)
)

(defun c:pjpg(/ zprinter zpaper zstyle)
	(setq zstyle "for print.ctb"
		zprinter "PublishToWeb JPG.pc3"
		zpaper "Sun Hi-Res (1600.00 x 1280.00 Pixels)")

	(if (setq color (getstring "\nColor or Black <Color>: "))
		(progn
			(setq color (strcase color nil)) ; making uppercase

			(if (OR (= color "B") (= color "BLACK"))
				(setq zstyle "for black.ctb")
			)
		)
	)

	;(princ "\n**************************** [ BOTTOM -> TOP ] ****************************")
	(p2 zprinter zpaper zstyle 'jpg nil :vlax-false :vlax-true)
)

;BLACK
(defun c:p2B(/)
	;Current_printer_identifier
	(setq cpi 'XEROX)

	;**************************
	;*******- FACE DOWN- ********
	;**************************

	;(princ "\n**************************** [ BOTTOM -> TOP ] ****************************")
	(p2 ($get_printer cpi) ($get_paper_for_printer cpi) ($get_printing_style 'BLACK) 'black nil :vlax-false :vlax-true)
)

;COLOUR
(defun c:p2C(/)
	;Current_printer_identifier
	(setq cpi 'ROBOT)

	;**************************
	;*******- FACE UP- ********
	;**************************

	;(princ "\n**************************** [ TOP -> BOTTOM ] ****************************")
	(p2 ($get_printer cpi) ($get_paper_for_printer cpi) ($get_printing_style 'COLOR) 'colour nil :vlax-false :vlax-true)
)

;COLOUR
(defun c:p24(/)
	;;Current_printer_identifier
	(setq cpi 'HP400)

	;**************************
	;*******- FACE DOWN -******
	;**************************

	;(princ "\n**************************** [ TOP -> BOTTOM ] ****************************")
	(p2 ($get_printer cpi) ($get_paper_for_printer cpi) ($get_printing_style 'COLOR) 'black nil :vlax-false :vlax-true)
)


(defun p2 (zprinter zpaper zstyle plottype ss isHistory needSort / gc:UcsBoundingBox gc:TMatrixFromTo cm dwgn object llpt lst ss urpt zstyle zprinter zpaper f_cpi)

	(setq cm (getvar 'cmdecho))
        (setvar 'cmdecho 0)

  ;; by Gilles Chanteau
  ;; gc:UcsBoundingBox
  ;; Returns the UCS coordinates of the object bounding box about current UCS
  ;;
  ;; Arguments
  ;; obj: an entity (ENAME or VLA-OBJCET)
  ;; _OutputMinPtSym: a quoted symbol (output)
  ;; _OutputMaxPtSym: a quoted symbol (output)
	(defun gc:UcsBoundingBox (obj _OutputMinPtSym _OutputMaxPtSym)
	    (and (= (type obj) 'ename)
	         (setq obj (vlax-ename->vla-object obj))
	    )

	    (vla-transformby obj (vlax-tmatrix (gc:TMatrixFromTo 1 0)))
	    (vla-getboundingbox obj _OutputMinPtSym _OutputMaxPtSym)
	    (vla-transformby obj (vlax-tmatrix (gc:TMatrixFromTo 0 1)))
	    (set _OutputMinPtSym (vlax-safearray->list (eval _OutputMinPtSym)))
	    (set _OutputMaxPtSym (vlax-safearray->list (eval _OutputMaxPtSym)))
	)

  ;; gc:TMatrixFromTo
  ;; Returns the 4X4 transformation matrix from a coordinate system to an other one
  ;;
  ;; Arguments
  ;; from to: same arguments as for the 'trans' function
  (defun gc:TMatrixFromTo (from to)
    (append
      (mapcar
        (function
          (lambda (v o)
            (append (trans v from to t) (list o))
          )
        )
        '(
          (1.0 0.0 0.0)
          (0.0 1.0 0.0)
          (0.0 0.0 1.0)
         )
        (trans '(0.0 0.0 0.0) to from)
      )
      '((0.0 0.0 0.0 1.0))
    )
  )

  (defun printSet(/)
	(initget "C B P")
	(setq cont (getstring (strcat "\n::: Print a Copy of (Color/Black/cPDF/Esc to Cancel) <Esc>: ")))

		(setq ans (strcase (substr cont 1 1) nil))

		(cond
	    	((= ans "B")
		    	(setq
	    			f_cpi 'XEROX
	    			zstyle ($get_printing_style 'BLACK)
					zprinter ($get_printer f_cpi)
					zpaper ($get_paper_for_printer f_cpi)
					plottype 'black)
			)
	   		((= ans "C")
				(setq
	    			f_cpi 'ROBOT
	    			zstyle ($get_printing_style 'COLOR)
					zprinter ($get_printer f_cpi)
					zpaper ($get_paper_for_printer f_cpi)
					plottype 'colour)
			)
			((= ans "P")
				(setq
	    			f_cpi 'PDF
	    			zstyle ($get_printing_style 'COLOR)
					zprinter ($get_printer f_cpi)
					zpaper ($get_paper_for_printer f_cpi)
					plottype 'pdf)
			)
			(exit)
	   )
		(doPrint)
  )

  (defun doPrint(/ cords folder dwgname fn)
		(if ss
		    (progn
		    	;(setvar 'filedia 0) ; to do

		      	(setq vc (getvar "VIEWCTR")
			        vs (getvar "VIEWSIZE")
			        ve (trans '(0 0 1) 0 1)
			        increment 1
              		cords '())

		      	;the output of the paper from hp printer is 'faced'
		      	;so the last paper should be ordered first
		      	;but the black xerox will print the paper off faced - thus it should print the first page first
		    	(if (eq plottype 'colour)
		    		(setq i (sslength ss) delta -1)
		    		(setq i -1 delta 1)
		    	)

		    	;ss
		    	; sorting ename blocks
		    	(setq sorting_thrushold 3)
		    	(if (and (<= sorting_thrushold (sslength ss)) (OR (= :vlax-false isHistory) (= :vlax-true needSort)))
		    		(progn
		    			(setq cont (getstring "\n::: Sort-Blocks? (Yes/No) <N>: "))
		      			(if (= (strcase cont nil) "Y")
		      				(setq block_entities (sortBlocks (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))) ; sort blocks
		      				(setq block_entities (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))) ;un- sorted
		      			)
		    		)
		    		(setq block_entities (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))) ;un- sorted
		    	)

		    	(setq progress_thrushold 3)
		    	(if (<= progress_thrushold (length block_entities))
		    		(progn
		    			;(load ".\\progress.lsp")
		    			(ODCL:showProgress zprinter (length block_entities)) ; initiate progress with total blocks count
		    		)
		    	)

		    	;FILENAME FOR PLOT TO PDF
		      	(setq dwgname "n")
		      	;Preparing the folder to save the PDF or JPEG
				(if (OR (= plottype 'pdf) (= plottype 'jpg))
			      	(progn
			      		(setq fn (getvar "dwgname") fn (cadr (fnsplitl fn)))
			      		(setq folder (getDefualtPDFLocation fn))
			  		)
			  	)

		    	;print loop
		    	(repeat (length block_entities)
		        	;(setq hnd (ssname ss (setq i (+ i delta))))
		        	(setq object (nth (setq i (+ i delta)) block_entities))
		        	(gc:UcsBoundingBox object 'llpt 'urpt)

		        	(setq lst (cons llpt lst)
		              lst (cons urpt lst))

			        (setq llpt (apply 'mapcar (cons 'min lst))
			            urpt (apply 'mapcar (cons 'max lst)))

			        ;(print lst)
		        	(setq lst nil)


		        	; **********************;
					; ---   PLOTTING   ---- ;
		        	; **********************;

		        	; if the plot type is in one of these, then the filename will an incremental number inside that folder
		        	(if (OR (= plottype 'pdf) (= plottype 'jpg))
				      	(setq dwgname (strcat folder "\\" (itoa increment)))
				  	)

					(command "zoom" "_object" object "")
			      	(princ (strcat "\n*** Printing " (itoa increment) " of " (itoa (sslength ss)) " ***"))

			      	;increment progress
					(ODCL:setProgress increment)

			      	; PAUSING THE SECOND PRINT
			      	(if (= increment 2)
			      		(progn
			      			(setq cont (getstring "\n::: Continue Printing (Yes/No) <Y>: "))
			      			(if (= (strcase cont nil) "N")
			      				(Progn
			      					(command "_ZOOM" "_C" (trans vc ve 1) vs) ; RESTORING ZOOM LEVEL
			      					(princ (strcat "\n*** Plot to \"" zprinter "\" has been cancelled!"))
			      					(exit)) ; EXIT
			      			)
			      		)
			      	)

			      	;PLOT
			      	(if (= plottype 'jpg)
			      		(plotterj zprinter zpaper zstyle llpt urpt dwgname)
			      		(plotter zprinter zpaper zstyle llpt urpt dwgname)
              		)

              	;getting cords
              	(setq cords (cons (list llpt urpt) cords))
			  	(setq increment (1+ increment))
			    (command "_DELAY" 500)

			) ;repeat

        ;*********************
        ;RESTORING ZOOM LEVEL
        ;*********************
        (ODCL:closeProgress)
		(command "_ZOOM" "_C" (trans vc ve 1) vs)

		;save last printed date
		(markLastPrint)
        ;(print (setq rev (reverse cords)))


		    ; PREPARING THE SAME SELECTION SET FOR ANOTHER PRINT
		    (if (> (sslength ss) 5)
		    	(progn

            ;*********************************
            ;STORING COORDINATES INTO THE FILE
            ;*********************************
            ;(initget "Y" "N")
            (setq ans (getstring "\nWould you like to save these block references for later Printing? (Y/N) <N>: "))

	            (if (= (strcase ans nil) "Y")
	              (progn
	                ;IF THE PRINTING WAS FOR COLOR, COORDINATES IS ARRANGED FROM TOP TO BOTTOM
	                (if (/= plottype 'colour)
	                  (setq cords (reverse cords)))

	                (vlax-ldata-put $TAMIMI $CORDS cords)
	                (print)
	                (princ "\n*** COORDINATES HAVE BEEN SAVED ***")
	                (print))
	            )

	            ;request to print again in case of other variants
	    		(alert "Print a Copy of same project in other variants")
	    		(printSet)
		    ))


		    ;(setvar 'filedia 1)

		    ) ;progn
		    (progn
		    	(print (strcat "\nNothing has been printed to " zprinter))
		    	(princ)
		    )
	  	)
  	)



  (PRINC (strcat "\n[" zprinter "], [" zpaper "], [" zstyle "]"))
  ; shows an alert to user
  ;(a2 zprinter zpaper zstyle plottype)

  ;********************************************************************************
  ; if isHistory is requested the print will be sent directly to Prinset to choose
  ;********************************************************************************
    (if (= :vlax-true isHistory)
      (printSet)

      (progn
        (if (not ss)
          (progn
            (setq ss (ssget blocksToSelect))
          )
        )
        (doPrint)
      )
    )

  	(setvar 'cmdecho cm)
  (princ)
)


; *******************************************************************
; SORT-BLOCKS - blocks are sorted according to axis
;	top-left blocks are sorted first
; *******************************************************************

(defun sortBlocks(blocks / c1 c2 offset)
	(setq offset 10)
	(setq blocks
		;(apply 'ssadd
			(mapcar 'vlax-vla-object->ename
				(vl-sort
					(mapcar 'vlax-ename->vla-object blocks)

					'(lambda (ob1 ob2)

						(if (vlax-property-available-p ob1 'length) ;identifying polylines
							T
							(progn ; if not it's a block
								(if (vlax-property-available-p ob2 'length)
									nil
									(progn
										(setq c1 (vlax-get ob1 'insertionpoint)
											c2 (vlax-get ob2 'insertionpoint))

										(setq differ (- (getMax (getY c1) (getY c2))
													(getMin (getY c1) (getY c2))))
										(print (strcat "differ " (rtos differ)))

										(if (>= differ offset) ; y axis diff by offset
											(progn
												(> (getY c1) (getY c2)))

											(progn
												(print "same row")
												(print (strcat "c1:" (rtos (car c1)) " c2:" (rtos (car c2))))
												(and
													(< (getX c1) (getX c2))
													;(> (getY c1) (getY c2))
												)
											)
										)
									)
								)
							) ;block is found
						)
					)
				)
			)
		;)
	)
	;(setq blocks nil)
)

(defun getX(cord /)
	(car cord))

(defun getY(cord /)
	(cadr cord))

(defun getMin(n1 n2 /)
	(min (abs n1) (abs n2))
)

(defun getMax(n1 n2 /)
	(max (abs n1) (abs n2))
)

(defun getDefualtPDFLocation(dwgname / cur_loc)
	(setq cur_loc (getvar 'dwgprefix)
		folder (strcat cur_loc dwgname "_PDF"))

	(if (not (vl-file-directory-p folder)) ; no folder is found then, create one
		(progn
			(if (vl-mkdir folder)
				(setq loc folder)
				(setq loc "")
			)
		)
		;if exists
		(progn
			(setq loc folder)
		)
	)
)

; *******************************************************************
; Plotter : core function which helps to plot to printer
; *******************************************************************
; a1 -  printer
; a2 -  paper
; a3 -  style
; a4 -  bl coordinates
; a5 -  tr coordinates
; a6 - 	plot to file name
; a7 -  orientation

(defun plotter(a1 a2 a3 a4 a5 a6 /)
	; never change this function
	;(princ (strcat a6))
  (print (list a4 a5))
	(command "-plot" "y" "" a1 a2 "m" "l" "n" "w" a4 a5 "f" "c" "y" a3 "y" "" a6 "y" "y") ; command
)

(defun plotterj(a1 a2 a3 a4 a5 a6 /) ; jpeg plotter
	; never change this function
	;(princ (strcat a6))
	(command "-plot" "y" "" a1 a2 "l" "n" "w" a4 a5 "f" "c" "y" a3 "y" "" a6 "y" "y") ; command
)

; INCLUDED TO PRINT IN DIFFERENT ORIENTATION
									______ orientation
								   |
(defun plotter2(a1 a2 a3 a4 a5 a6 a7 /)
	; never change this function
	;(princ (strcat a6))
	(command "-plot" "y" "" a1 a2 "m" a7 "n" "w" a4 a5 "f" "c" "y" a3 "y" "" a6 "y" "y") ; command
)

;************************************
;NOTIFY USER ABOUT THE PRINT CAPABILITY
;****************************************
(setq temparrayref (vlax-ldata-get "TAMIMI" "COORDINATES"))
(if (/= temparrayref nil)
  (print (strcat "This file supports printing from history! of " (itoa (length temparrayref)) " blocks to print")))


;*****************************************
;PRINT USING COORDINATES SAVED IN THE FILE
;*****************************************
(defun c:ph(/ cords i j blocks temp echo missing t_missing)

	(setq cords (vlax-ldata-get "TAMIMI" "COORDINATES")
	  	i 0 j i missing 0)
	(util:activateCmdecho)

  	(if (/= cords nil)
	    (progn
	      	(foreach pair cords
	        	(progn
	          		(setq i (1+ i)) ;counter increment

					;array blocks is empty
		         	(if (= blocks nil)
			            (progn
			              	(setq blocks (ssget "_C" (trans (car pair) 0 1) (trans (last pair) 0 1) blocksToSelect))
			              	(if (= nil blocks)
			              		(progn
			              			(print "No blocks were found!")
			              			(1+ missing))
			              	)

			              	;(print (strcat "Selection: " (itoa i) " ---- SET " (itoa (sslength blocks)))))
			              	;(print)
			            )

			            ;array blocks is not empty
			            (progn
			              (setq temp (ssget "_C" (trans (car pair) 0 1) (trans (last pair) 0 1) blocksToSelect))
			              ;(setq blocks (ssget "_W" '(19452.9 -36086.5 -1.0e-008) '(19490.2 -36058.9 1.0e-008) (list (cons 0 "INSERT"))))

			              (if (or (= nil temp) (< (sslength temp) 0))
			              	(progn
			              		(setq missing (1+ missing))
			              		(print "Selection is empty")
			              	)
			              	(progn
								(print (strcat "TEMP COUNT: " (itoa (sslength temp))))
					            (print (strcat "Selection: " (itoa i) " ---- OBJECTS COUNT " (itoa (sslength temp))))

					            (repeat (setq n (sslength temp))
					            	(print (strcat "repeat # " (itoa n)))
					                (setq obj (ssname temp (setq n (1- n))))
					                (if (= nil obj)
					                	(print "block is missing")
					                	(setq blocks (ssadd obj blocks))
					                )
					            )
			              	)
			              )
			            )
	          )
	        )
	      )
	      ; check detected object whether selected correctly or not
	      ;(command "erase" blocks "")

	      (princ "\n*** Print Back Feature ***\n")

	      (command "_.SELECT" blocks)
	      ;(sssetfirst nil blocks)

	      (setq pre_selection_length (sslength blocks)) ; no of blocks have been selected BEFORE user selection

	      ;(princ "Select / remove blocks: ")
	      (if (> missing 0)
	      	(setq t_missing (strcat "[" (itoa missing) " block/s not found]"))
	      	(setq t_missing ""))

	 	  (princ (strcat "Select / remove blocks " t_missing ": "))

	      (while (> (getvar 'cmdactive) 0) (command pause)
	        (setq blocks (ssget "_P" blocksToSelect))
	      )

	      (setq post_selection_length (sslength blocks)) ; no of blocks have been selected AFTER user selection

	      (setq ans (getstring (strcat "\n*** Found " (itoa (sslength blocks)) " blocks, Continue (Y/N) <N>: ")))
	      (cond
	      	((= (strcase ans nil) "Y")
	        	(progn

	        ; 		(if (> post_selection_length pre_selection_length)
	        ; 			(progn
	        ; 				(setq cont (getstring "\n::: Sort-Blocks? (Yes/No) <N>: "))
		      	; 			(if (= (strcase cont nil) "Y")
		      	; 				(progn
									; (setq blocks (sortBlocks (vl-remove-if 'listp (mapcar 'cadr (ssnamex blocks))))) ; sort blocks
									; (print "Sorted"))
		      	; 			)
	        ; 			)
	        ; 		)

		            (setq
		            	f_cpi 'xerox 
		             	zstyle "for black.ctb"
		              	zprinter pXerox
		              	zpaper "A3 (297 x 420mm)")
		            (p2 zprinter zpaper zstyle 'black blocks :vlax-true (if (OR (> post_selection_length pre_selection_length) (< post_selection_length pre_selection_length) ) :vlax-true :vlax-false))
	          	)
	        )
	      );cond
	    )
	    (progn
	      (alert " * No saved coordinates have been found! *"))
  )
  ;(setvar 'cmdecho echo)
  (util:restoreCmdecho)
)


; *******************************************************************
; Rorate multiple objects aroud its base point
; *******************************************************************
(defun c:r2( / sel o e rot osm *error* error_bak ce)

	(setq osm (getvar 'osmode)
		i 0
		error_bak *error*
		ce (getvar 'cmdecho))

    (setvar 'osmode 0)
    (setvar 'cmdecho 0)

	(defun *error* (msg /)
			;ON ERROR
			(if (not (member msg '("Function cancelled")))
				(print msg)
				)
			(setvar 'osmode osm)
			(setvar 'cmdecho ce)
			(setq *error* error_bak)
		)

  ; backing up osmode varplpklpl1
	(if (setq sel (ssget '((0 . "INSERT"))))
    (progn
      (if (not (setq rot (getint "\nSpecify rotating angle <90>: ")))
          (setq rot 90))

      (while (< i (sslength sel))
          (setq o (ssname sel i)
            e (entget o)
            i (1+ i))

          (command "_rotate" o "" (cdr (assoc 10 e)) rot)
          ;(princ e)
      ) ;while
    ) ;progn
  )
	;restoring object snap mode value
    (setvar 'osmode osm)
	(princ)
)

; *******************************************************************
; Converts multiple LINES into POLYLINES
; JOINS converted polylines, inorder to make them compatible with multiline command
; *******************************************************************
(defun c:F4(/ sset ans)

  (setq ans "Y")
	(if (setq sset (ssget
    	(list (cons 0 "LINE"))))
		(command "_.PEDIT" "M" sset "" "Y" "J" "0.0" "")
	    ; (progn
	    ; 	(setq ans (strcase (getstring "\nJoin polylines (Yes/No) <Y>: ") nil))
	    ;   	(if (OR (eq ans "Y") (eq ans ""))
		   ;      (command "_.PEDIT" "M" sset "" "Y" "J" "0.0" "")
		   ;      (command "_.PEDIT" "M" sset "" "Y" "")
	    ;   	)
	    ; )
  	)
	(princ)
)




; *******************************************************************
; Prints X Y Coordinates to the lable
; COMMAND : P1
; *******************************************************************
(defun c:p1 (/  p x y ptcoord textloc)
	(while ;start while
		(setq p (getpoint "Pick Point: ")
			textloc (getpoint p "Pick Text Location"))

		(setq x (rtos (car p))
			y (rtos (cadr p)))

		(setq ptcoord (strcat "x=" x ", " "y=" y))
		(command "_leader" p textloc "" ptcoord "")
	(princ)
	) ;end while
)


; *******************************************************************
; Change MTEXT STEXT Text Continuously
; COMMAND : T4
; *******************************************************************
 (defun c:T4(/ usertext sel)
 	;(setq usertext (strcase (getstring "\nText to Replace: ") nil))
 	(princ  "*** Source Text ***")
 	(setq obj (vlax-ename->vla-object (ssname (ssget "_+.:E:S" '((0 . "*TEXT"))) 0)))
 		(setq usertext (vlax-get obj 'TextString))

 	(princ  "*** Select Text to Replace ***")
 	(while
 		(setq sel (ssget "_+.:E:S" '((0 . "*TEXT"))))
 		(progn
 			(if (> (sslength sel) 0)
 				(progn
 					(setq name (ssname sel 0))
	 				(setq p (vlax-ename->vla-object name))
	 				(vlax-put p 'TextString usertext)
 				)
 			)
 		)
 	)

 )


(defun c:pt(/ source_objects dest_objects)
	(print)
	(setq source_objects nil dest_objects nil limit_thrushold 2 lower_thrushold 0)

	(defun selectSourceObjects()
		(princ  "\r*** Select Texts ***")
		(setq source_objects (ssget '((0 . "*TEXT"))))

		(if (> (sslength source_objects) lower_thrushold)
			(progn
				(setq source_objects (sort source_objects)  ; sort top to bottom
						limit_thrushold (length source_objects))
				(selectDestinationObjects)
			)
			(selectSourceObjects))
	)

	(defun selectDestinationObjects()
		(princ  "\r*** Select Texts to replace ***")
		(setq dest_objects (ssget '((0 . "*TEXT"))))

		(if (= (sslength dest_objects) limit_thrushold)
			(progn
				(setq dest_objects (sort dest_objects)) ; sort top to bottom
				(pastTexts)
			)
			(selectDestinationObjects))
	)

	(defun sort(blocks /)
		(setq sorted_blocks
			(vl-sort
				(mapcar 'vlax-ename->vla-object
					(vl-remove-if 'listp (mapcar 'cadr (ssnamex blocks)))
				)
				(function (lambda (x y)
						; logic to sort
						(>
							(cadr (vlax-get x 'insertionpoint))
							(cadr (vlax-get y 'insertionpoint))
						)
					))
			)
		)
	)

	(defun deleteSource()
		(setq cont (getstring "\n::: Delete source texts ? (Yes/No) <Y>: "))
		    (if (not (= (strcase cont nil) "N"))
		    	(progn
		    		(foreach obj source_objects
		    		(vla-delete obj)))
		    )
	)

	(defun pastTexts(/ i)
		(setq i 0)
		(repeat (length source_objects)
			(progn
				(vlax-put (nth i dest_objects) 'TextString (vlax-get (nth i source_objects) 'TextString))
				(setq i (1+ i))
			)
		)
		(deleteSource)
		(closeRefEdit)
		(princ)
	)

	(defun closeRefEdit()
		(if (/= (getvar 'refeditname) "")
			(command "_.refclose" "_save"))
	)

	(selectSourceObjects)
)


; *******************************************************************
; Clear the saved data on vlax-ldata 
; COMMAND : CL - CROSS-FENCE
; *******************************************************************
(defun c:clr(/)

	(defun getArray()
		(princ "\nSearching for Records...")
		(vlax-ldata-get $TAMIMI $CORDS)
	)

	(if (getArray)
		(progn
			(princ "\nRecords found.")
			(setq cont (getstring "\n::: Delete coordinates from file ? (Yes/No) <Y>: "))
		    (if (not (= (strcase cont nil) "N"))
		    	(progn
		    		(vlax-ldata-delete $TAMIMI $CORDS)
					(princ "\nRecords has been cleared.")	
		    	)
		    )
		)
		(princ "\nNo Records.")
	)
	(princ)
)

; *******************************************************************
; Draw Cross Lines
; COMMAND : CL - CROSS-FENCE
; *******************************************************************


(defun c:CF( / ent p1 p2 ar)
	(if (setq p1 (getpoint "\nRectanle Start: "))
		(progn
			(setq p2 (getcorner p1 "\nRectangle End: "))
			(command "rectangle" p1 p2)
			;(setq cords (group2 (vlax-get ent 'coordinates)))
			(setq ent (vlax-ename->vla-object (entlast)))
			(setq ar (group2 (vlax-get ent 'coordinates)))
			(print ar)
			(vla-addline acSpace (vlax-3d-point (car ar)) (vlax-3d-point (caddr ar)))
			(vla-addline acSpace (vlax-3d-point (cadr ar)) (vlax-3d-point (cadddr ar)))

			(set 'an "")
			(if (= (setq an (strcase (getstring "\nDelete Rectangle [Y/N] <N>: ") nil)) "Y")
				(vla-delete ent))
		)
	)
	(princ)
)


(defun group2 ( l / r )
	;(print (itoa (length l)))
    (repeat (/ (length l) 2)
    	;(print l)
        (setq r (cons (trans (list (car l) (cadr l)) 0 1) r)
        	l (cddr l)
        )
        (print l)
    ) ;repeat
    (reverse r)
)



(defun setClipText(str / html result)
(if (= 'STR (type str))
  (progn
  (setq html   (vlax-create-object "htmlfile")
        result (vlax-invoke (vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData) 'setData "Text" str)
  )
  (vlax-release-object html)
   str
   )
 );end if
)

(defun c:cpp(/)
	 (setClipText (strcat (getvar "dwgprefix") (getvar "dwgname")))
)

;ENTGET FUNCTION GIVES ARRAY OF THE ENTITY PROPERTIES

(defun c:sn(/)
	(setvar 'cmdecho 1)

	(defun sortBlocks2(blocks / func getAxis p1 p2)
		;(and
			(setq p1 (getpoint "\nPick first point: "))
			(setq p2 (getpoint p1 "\nPick second point: "))

		;setting base axis
		(if (= (getX p1) (getX p2))
			(setq getAxis getY)
			(setq getAxis getX))



		(if (OR
				(AND (= getAxis getX) (< (getX p1) (getX p2)))
				(AND (= getAxis getY) (< (getY p1) (getY p2))))
			(setq func <)
			(setq func >)
		)

		(setq blocks
			;(apply 'ssadd
				;(mapcar 'vlax-vla-object->ename
					(vl-sort
						(mapcar 'vlax-ename->vla-object blocks)

						'(lambda (ob1 ob2)
							(setq c1 (vlax-get ob1 'insertionpoint)
								c2 (vlax-get ob2 'insertionpoint))

							(func (getAxis c1) (getAxis c2))
						)
					)
				;)
			;)
		)

	)

	(defun pasteMulti(/ ss sorted_blocks)
		(print "MultiMode")

		(setq ss (ssget '((0 . "INSERT") (2 . "BLOCK_STAIR_NO"))))
		(setq sorted_blocks (sortBlocks2 (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))))
		;sorted_blocks
		(print (itoa (length sorted_blocks)))
		(foreach obj sorted_blocks
			(progn
				(setNumber obj))
		)
		(princ)
		(exit)
	)

	(defun modeSingle(/ rtn)
		(while
			(princ)
			(initget "M N")
			(setq rtn (entsel "\nSelect Stair Attribute or [Multiple]:"))

			; if redirect to multi then it will be exited from function
			(cond
				((= rtn "M")
					(pasteMulti))

				((if
		 			(and
		 				(/= rtn nil)
		 				(setq ve (vlax-ename->vla-object (car rtn)))
		 				(= (vla-get-objectname ve) "AcDbBlockReference"))

		 			(progn
			 			(setNumber ve)
			 		)
			 		(modeSingle)
			 	))

			)
	 	)
 		(princ)
 	)

 	(defun setNumber(obj /)
 		(mapcar
 			'(lambda (at)
				(if (= (vla-get-tagstring at) "STAIR_NO")
					(progn
						(vla-put-TextString at (itoa *num*))
						(setq *num* (1+ *num*)))
				)
			)
			; applying fucntion to
			(vlax-invoke obj 'GetAttributes))
 	)

 	(defun getNumber(/)
 		; DEFAULT VALUE IF NOT SET
		(if (not *num*) (setq *num* 1))
 		(if (> (setq sn (getint (strcat "\nCounting From <" (itoa *num*) ">:"))) 0)
			(setq *num* sn)
			(princ (strcat " Invalid Number, Counting From " (itoa *num*)))
		)
		(modeSingle)
 		(princ)
 	)

 	(getNumber)
)

(defun c:vp()
	(if (not *act*)
		(progn (setq *act* 0)))

	(if (= *act* 1)
		(progn
			(ai_tiledvp 1 "_V")
			(setq *act* 0))
		(progn
			(ai_tiledvp 2 "_V")
			(setq *act* 1)
			(princ "*** VIEWPORT ACTIVE"))
	)
	(princ)
)

(defun c:xh(/)
	(command "_xline" "_H")
)

(defun c:xv(/)
	(command "_xline" "_V")
)

; DEBUG - IMPLEMENTING NEW MJJ
; WISH ME
(defun c:MK ( / cm e1 e2 i1 i2 il m1 m2 o1 o2 p1 ss ml_start ml_end ml_cir_int ml_cross_line ml_cord cords)
    (setq acApp (vlax-get-acad-object))
    (setq acDoc (vla-get-ActiveDocument acApp))
    (setq acSpace (vla-get-ModelSpace acDoc))
    ;(SETQ cords '())

    (defun trimThisTree(p1 o1 o2 /)
    	(setq m1 (group3 (vlax-get o1 'coordinates))
              m2 (group3 (vlax-get o2 'coordinates)))

                            (if (<  (min (distance p1 (car m1)) (distance p1 (last m1)))
                                    (min (distance p1 (car m2)) (distance p1 (last m2)))
                                )
                                (command "_.-mledit" "_OT" (list e1 (car il)) (list e2 (cadr il)) "")
                                (command "_.-mledit" "_OT" (list e2 (car il)) (list e1 (cadr il)) "")
                            )
    )


    (vla-StartUndoMark acDoc)

    (if (setq ss (ssget "_:L" '((0 . "MLINE"))))
        (progn
            (setq cm (getvar 'cmdecho))
            (setvar 'cmdecho 0)
            (repeat (setq i1 (sslength ss))
                (setq e1 (ssname ss (setq i1 (1- i1)))
                      o1 (vlax-ename->vla-object e1)
                      ; ml_start (car (group3 (vlax-get o1 'coordinates))) ; ml start coordinates
                      ; ml_end (last (group3 (vlax-get o1 'coordinates)))
                      ; ml_cord '()
                      ; ml_cord (cons ml_start ml_cord)
                      ; ml_cord (cons ml_end ml_cord)
                )
                (repeat (setq i2 i1)
                    (setq e2 (ssname ss (setq i2 (1- i2)))
                          o2 (vlax-ename->vla-object e2)
                          il (group3 (vlax-invoke o1 'intersectwith o2 acextendnone))
                    )

                    ;(princ il)
					(if (/= il nil)
						(progn
							(foreach point il
								(progn
									(if (not (member point cords))
									(setq cords (cons point cords)))
								)
							)
						)
                    )

                    ;(princ (strcat "\nINTERSECT: " )) (princ il)
                    ;(princ cords)
                    ; (foreach cor ml_cord
                    ;   (progn
                    ;     (if (AND (/= il nil) (not (member cor il)) (>= (distance cor (car il)) 0.15))
                    ;         (progn
                    ;           (setq tempcircle (vla-addcircle acSpace (vlax-3d-point (trans cor 0 1)) 0.1)
                    ;                 ml_cir_int (group3 (vlax-invoke o1 'intersectwith tempcircle acextendnone))
                    ;             )
                    ;          (if ml_cir_int
                    ;             (setq ml_cross_line (vla-addline acSpace (vlax-3d-point (car ml_cir_int)) (vlax-3d-point (last ml_cir_int))))
                    ;           )
                    ;           ;(princ (car ml_cir_int))
                    ;           ;(princ (last ml_cir_int))
                    ;           (vla-delete tempcircle)
                    ;         )
                    ;       )


                    ;     )
                    ; )

                    (cond
                        (   (= 1 (length il))
                            (command "_.-mledit" "_CJ" (list e1 (car il)) (list e2 (car il)) "")
                        )
                        (   (= 4 (length il))
                        	(print (distance (car il) (cadr il)))

                        	(if (> (distance (car il) (cadr il)) 0.3)
                        			(progn
										(trimThisTree (car il) o1 o2)
										;(trimThisTree (car il) o1 o2) ; trims first pair
                        			(princ (cadr il)))
                        			(command "_.-mledit" "_OC" (list e1 (car il)) (list e2 (cadr il)) "")
                        		)

                        )
                        (   (= 2 (length il))
                            (setq p1 (car il)
                                  m1 (group3 (vlax-get o1 'coordinates))
                                  m2 (group3 (vlax-get o2 'coordinates))
                            )
                            (if (<  (min (distance p1 (car m1)) (distance p1 (last m1)))
                                    (min (distance p1 (car m2)) (distance p1 (last m2)))
                                )
                                (command "_.-mledit" "_OT" (list e1 (car il)) (list e2 (cadr il)) "")
                                (command "_.-mledit" "_OT" (list e2 (car il)) (list e1 (cadr il)) "")
                            )
                        )
                    )
                )
            )
            (setvar 'cmdecho cm)

            ; FILLING LINES
            ; checks each ml coordinates to elimate the corners which intersects with other mls

            (repeat (setq i1 (sslength ss))
                (setq e1 (ssname ss (setq i1 (1- i1)))
                      o1 (vlax-ename->vla-object e1)
                      ml_start (car (group3 (vlax-get o1 'coordinates))) ; ml start coordinates
                      ml_end (last (group3 (vlax-get o1 'coordinates)))

                      ml_cord '()
                      ml_cord (cons ml_start ml_cord)
                      ml_cord (cons ml_end ml_cord)

                )

                      (foreach cor ml_cord ; MAX WOULD COME 2 POINTS
                      (progn
                        (if (AND (/= cor nil) (not (member cor cords)))
                            (progn
                              (setq tempcircle (vla-addcircle acSpace (vlax-3d-point (trans cor 0 1)) (/ (getvar 'CMLSCALE) 2))
                                    ml_cir_int (group3 (vlax-invoke o1 'intersectwith tempcircle acextendnone))
                                )
                             (if ml_cir_int
                                (setq ml_cross_line (vla-addline acSpace (vlax-3d-point (car ml_cir_int)) (vlax-3d-point (last ml_cir_int))))
                              )
                              ;(princ (car ml_cir_int))
                              ;(princ (last ml_cir_int))
                              (vla-delete tempcircle)
                            )
                          )


                        )
                    )
            )
        )
    )
    (vla-EndUndoMark acDoc)
    (princ)
)

; ************************************************************
;         HANDLES REPLACING AREA AND PERIMETER LENGTH
; ************************************************************

(defun c:apt(/ i sel)
  (util:activateCmdecho)

  (setq sel (ssget "_+.:E:S")) ; selecting single entity

  (if (= (sslength sel) 1)
    (progn
      (command "_.AREA" "_Object" (ssname sel 0) "")
      (princ (strcat "AREA: " (rtos (getvar 'area) 2 2) "\n")) ; 2 decimal 2 no of precision
      (princ (strcat "LENGTH: " (rtos (getvar 'perimeter) 2 2) "\n")) ; 2 decimal 2 no of precision

      (setq i 1)
      (while (setq lst (ssget "_+.:E:S" (list '(0 . "TEXT,MTEXT"))))
        (setq obj (ssname lst 0))
        (setq p (vlax-ename->vla-object obj))

        (cond
          ((= i 1)
              (vla-put-TextString p (rtos (getvar 'area) 2 2))
            )
          ((= i 2)
              (vla-put-TextString p (strcat (rtos (getvar 'perimeter) 2 0) ".00 M"))
            )
          (progn
            (exit))
        )
        (setq i (1+ i))
      )
    )
    (princ "No object is selected!")
  )
  (util:restoreCmdecho)
)

;erasing mlines
(defun c:rl(/ wlines)
	(setq wlines (ssget '(
		(-4 . "<AND")
			(0 . "MLINE") (8 . "WAL")
		(-4 . "AND>"))
	))
	(command "_.erase" wlines "")
	(princ)
)


; ************************************************************
;                         UTILITY ZONE
; ************************************************************

(defun util:activateCmdecho(/)
  (setq *echo* (getvar 'cmdecho))
  (setvar 'cmdecho 0)
)

(defun util:restoreCmdecho(/)
  (setvar 'cmdecho *echo*)
  ; (if (not *echo*)
  ;   (setvar 'cmdecho 1)
  ;   (setvar 'cmdecho *echo*))
  (princ)
)

(defun drawingName(/)
	(getvar 'dwgname))

(defun drawingPath(/)
	(getvar 'dwgprefix))

(defun get_file_size(/ rtn)
	(setq file (strcat (drawingPath) (drawingName)))
	(setq rtn (/ (vl-file-size file) 1024))
)

(defun markLastPrint(/)
	(vlax-ldata-put "TAMIMI" "date" (getDate))
	(princ (strcat "\nPrinted on " (getDate)))
)

(defun getDate(/ date)
	(setq date (menucmd "m=$(edtime,$(getvar, date), d\",\" DDDD HH:MM AM/PM)"))
)

(setq ind 0)
(defun progress_ind(/ chars)
	(setq chars '("\r-" "\r\\" "\r|" "\r\/"))

	;(repeat 40
		(princ (nth ind chars))
		(setq ind (rem (1+ ind) (length chars)))
	;)
	(princ)
)

; ************************************************************
;              FILE INFO : DESCRIBE THE FILE
; ************************************************************

(defun c:fi(/)
	(princ)

	(defun init(/)
		; get dialog definition and load dialog
		(setq dialog "fileinfo"
			 ddef (load_dialog "./dialog.dcl"))

		(if (> ddef 0) ; have dialogs
			(progn
				(if (not (new_dialog dialog ddef))
					(exit))

				(bind)
				(start_dialog)
				(unload_dialog ddef)
			)
		)
	)

	(defun bind(/)
		; buttons
		(action_tile "key_close" "(done_dialog)")
		(action_tile "key_audit" "(done_dialog)")

		(bindInfo)
		(princ "bind...")
	)

	(defun bindInfo(/)
		; set data to variables
		(set_tile "key_opened" (menucmd "m=$(edtime,$(getvar, date), d\",\" DDDD HH:MM AM/PM)"))
		(set_tile "key_drawing" (drawingName))
		(set_tile "key_location" (drawingPath))
		(princ)
	)

	(init)
)


; ************************************************************
;                         END OF FILE
; ************************************************************

(princ
    (strcat
        "\n\n**************************************************************************"
        "\n:: MyLsp.lsp | Version " MYVER " | \\U+00A9 Mohamedh Asrin " (menucmd "m=$(edtime,0,yyyy)")
        "                             Al-Tamimi Khafji ::"
        "\n            :: Provides a bunch of useful AUTOCAD functions ::"
        "\n************************************************************************** "

        ;"\n \\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1"

		"\n		  \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2557   \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2557  \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2557 \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2557 \\U+2588\\U+2588\\U+2588\\U+2557  \\U+2588\\U+2588\\U+2557 "
		"\n		 \\U+2588\\U+2588\\U+2554\\U+2550\\U+2550\\U+2588\\U+2588\\U+2557 \\U+2588\\U+2588\\U+2588\\U+2554\\U+2550\\U+2550\\U+2550\\U+255D  \\U+2588\\U+2588\\U+2554\\U+2550\\U+2550\\U+2588\\U+2588\\U+2551   \\U+2588\\U+2588\\U+2554\\U+2550\\U+255D \\U+2588\\U+2588\\U+2588\\U+2588\\U+2557 \\U+2588\\U+2588\\U+2551 "
		"\n		 \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2551  \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2557  \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2554\\U+2550\\U+255D   \\U+2588\\U+2588\\U+2551   \\U+2588\\U+2588\\U+2554\\U+2588\\U+2588\\U+2557\\U+2588\\U+2588\\U+2551 "
		"\n		 \\U+2588\\U+2588\\U+2554\\U+2550\\U+2550\\U+2588\\U+2588\\U+2551  \\U+255A\\U+2550\\U+2550\\U+2550\\U+2588\\U+2588\\U+2588\\U+2557 \\U+2588\\U+2588\\U+2554\\U+2550\\U+2588\\U+2588\\U+2557    \\U+2588\\U+2588\\U+2551   \\U+2588\\U+2588\\U+2551\\U+255A\\U+2588\\U+2588\\U+2588\\U+2588\\U+2551 "
		"\n		 \\U+2588\\U+2588\\U+2551  \\U+2588\\U+2588\\U+2551  \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2554\\U+255D \\U+2588\\U+2588\\U+2551  \\U+2588\\U+2588\\U+2557 \\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2588\\U+2557 \\U+2588\\U+2588\\U+2551 \\U+255A\\U+2588\\U+2588\\U+2588\\U+2551 "
		"\n		 \\U+255A\\U+2550\\U+255D  \\U+255A\\U+2550\\U+255D  \\U+255A\\U+2550\\U+2550\\U+2550\\U+2550\\U+2550\\U+255D  \\U+255A\\U+2550\\U+255D  \\U+255A\\U+2550\\U+255D \\U+255A\\U+2550\\U+2550\\U+2550\\U+2550\\U+2550\\U+255D \\U+255A\\U+2550\\U+255D  \\U+255A\\U+2550\\U+2550\\U+255D "
		;"\n \\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1\\U+25AB\\U+25A1"

    )
)
(princ)
(print)
(princ (strcat "FILE @ " (getDate)))
(PRINT)
