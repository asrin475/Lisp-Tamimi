;by Mohamedh Asrin 2018

(vl-load-com)

(defun c:yt()

	(defun replaceText(text / toFind toReplace vlText)
		(setq toFind "X =")

		(setq vlText (vlax-ename->vla-object text)
			vlTextString (vla-get-TextString vlText))

		(setq index (vl-string-search toFind vlTextString))
		;(print (itoa index))
		(if (/= nil index)
			(vla-put-TextString vlText (substr vlTextString (1+ index)))
			(vla-delete vlText))
		(princ)

	)

	(defun selectText(/ texts)
		(setq texts (ssget '((0 . "*TEXT"))))
		(if (OR (= nil texts) (< (sslength texts) 1))
			(progn
				(princ "Invalid Selection")
				(selectText)
			)
			(progn
				;text blocks
				(princ (itoa (sslength texts)))


				; (repeat (setq i (sslength texts))
				; 	(replaceText (ssname text (setq i (1- i)))))
				
				(setq i 0)
				(while (< i (sslength texts))
					(replaceText (ssname texts i))
					(setq i (1+ i))
				)
			)

		)
	)

	(selectText)

	(princ)
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