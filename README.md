# Lisp-Tamimi
*MyLisp.lsp is a collection of tools that eases working with autocad and it was created and maintained by Mohamedh Asrin*


## Prerequisites
1. AutoCAD 2008 and above
2. Windows 7 or later (tested only in Windows platforms)


## Things to know
``` $G_PRINTERS ``` : holds the printer names and their default paper sizes globally to access from anywhere

``` $G_STYLES ``` : list of styles ``` acad.ctb, forColor.ctb ... etc ```

``` $G_PRINTING_METHOD ``` : ouput variation of printers, whether the printed paper facing up or down


## Functions
1. ```P2C``` : Prints Autocad blocks to the specified printer
  ```lisp
  (defun c:p2C(/)
	;Current_printer_identifier
	(setq cpi 'ROBOT)

	;**************************
	;*******- FACE UP- ********
	;**************************

	;(princ "\n**************************** [ TOP -> BOTTOM ] ****************************")
	(p2 ($get_printer cpi) ($get_paper_for_printer cpi) ($get_printing_style 'COLOR) 'colour nil :vlax-false :vlax-true)
)```

2.```HPL```: Shows history of printings for a file 
```lisp
	**************************************************************
			   -= HISTORY : PRO 2228 =-
			 Today: 6, Wednesday 12:57 PM
			 Number of pages printed : 35
	**************************************************************
	1. COLOUR(22) - User   3-Feb-19 (Sun) 01:22 PM  [This week]
	2. BLACK(1) - User   3-Feb-19 (Sun) 12:15 PM  [This week]
	3. COLOUR(1) - User   29-Jan-19 (Tue) 01:27 PM  [This week]
	4. COLOUR(1) - User   28-Jan-19 (Mon) 01:44 PM  [This Month]
	5. COLOUR(1) - User   28-Jan-19 (Mon) 01:41 PM  [This Month]
```


  
