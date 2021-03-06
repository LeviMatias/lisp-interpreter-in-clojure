; TURTLE.LSP

(setq package (pkg "util")))

(declare)

;		      C O M M A N D S

(de :F ()
  (fd 50)
)))

(de :B ()
  (bk 50)
)))

;		      A L L


(de ALLFCN (tlist all-list)
  (if (null tlist)
      nil
      (let ( (turtle (first tlist)) )
	  (apply progn all-list) )
      (allfcn (rest tlist) all-list) )
)))

(df :ALL (all-list)
; evaluate the list one time for each existing turtle
; with that turtle as the current turtle
  (allfcn (tf) all-list)
)))


;		R E C O G N I Z E R


(de :TURTLEP (obj)
  (eq (type obj) 'turtle)
)))


;		E V A L U A T O R


(de EAPTURTLE (eap-tur l &AUX (turtle eap-tur) )
; what to do when evaluating a list whose car is a turtle
  (apply progn (rest l))
)))

(eap 'turtle eapturtle)


;		 S H A P E S


(dm :TUR (l &AUX (ll (rest l))
		  (len (length ll))
		  (new (newstr len (ascii 0)))
	     &SPECIAL len ) ; ** newstring
; create a shape string out of the list of numbers
  (if (neq (sub len 2) (mul (first ll)(second ll)))
      (error 'turtle-shape-inconsistent) )
  (for (i 1 len)
	(string-replace-char new i (car ll))
	(setq ll (cdr ll)) )
  `(quote ,new)
)))

;		T U R T L E   S H A P E S


(setq :TUR-UP (tur 4 16 0 252 0 0 0 184 0 0 0 252 0 0 0 252 0 0 15 255 192 0 
     253 117 252 0 253 117 252 0 15 255 192 0 13 117 192 0 13 117 192 0 255 
     255 252 0 253 117 252 0 13 117 192 0 3 255 0 0 0 48 0 0 0 48 0 0 )))

(setq :TUR-DOWN (tur 4 16 0 48 0 0 0 48 0 0 3 255 0 0 13 117 192 0 253 117 252 
     0 255 255 252 0 13 117 192 0 13 117 192 0 15 255 192 0 253 117 252 0 253 
     117 252 0 15 255 192 0 0 252 0 0 0 252 0 0 0 184 0 0 0 252 0 0 )))

(setq :TUR-BOX (tur 3 8 85 85 0 64 1 0 64 1 0 64 1 0 64 1 0 64 1 0 64 1 0 85 
     85 0 )))


(setq :TURTLE-SHAPES (vector tur-up tur-down tur-box))))












(setq S90 (tur 4 9 0 0 0 0 0 64 0 0 1 80 0 0 5 84 0 0 0 64 0 0 0 64 0 0 0 64 
     0 0 0 64 0 0 0 64 0 0 )))











(setq S270 (tur 4 9 0 64 0 0 0 64 0 0 0 64 0 0 0 64 0 0 0 64 0 0 5 84 0 0 1 
     80 0 0 0 64 0 0 0 0 0 0 )))











(setq S0 (tur 4 9 0 0 0 0 0 0 0 0 0 16 0 0 0 20 0 0 85 85 0 0 0 20 0 0 0 16 
     0 0 0 0 0 0 0 0 0 0 )))











(setq S180 (tur 4 9 0 0 0 0 0 0 0 0 1 0 0 0 5 0 0 0 21 85 64 0 5 0 0 0 1 0 
     0 0 0 0 0 0 0 0 0 0 )))











(setq S135 (tur 4 9 0 0 0 0 21 64 0 0 21 0 0 0 21 0 0 0 16 64 0 0 0 16 0 0 
     0 4 0 0 0 1 0 0 0 0 0 0 )))











(setq S315 (tur 4 9 0 0 0 0 16 0 0 0 4 0 0 0 1 0 0 0 0 65 0 0 0 21 0 0 0 21 
     0 0 0 85 0 0 0 0 0 0 )))











(setq S225 (tur 4 9 0 0 0 0 0 1 0 0 0 4 0 0 0 16 0 0 16 64 0 0 21 0 0 0 21 
     0 0 0 21 64 0 0 0 0 0 0 )))











(setq S45 (tur 4 9 0 0 0 0 0 85 0 0 0 21 0 0 0 21 0 0 0 65 0 0 1 0 0 0 4 0 
     0 0 16 0 0 0 0 0 0 0 )))

(setq TURTLE-DEFAULTS (vector s0 s45 s90 s135 s180 s225 s270 s315))

"TURTLE.LSP loaded"

)))
