;
;	    copyright (c) 1984, 1985, The LISP Company
;
; DEMO.LSP

(setq package (pkg "util"))))

(declare)
(declare (special-form :all))

;		U T I L I T I E S


(de :RINK (&AUX (new (rn 4)) )
; randomly set ink of current turtle, never set ink to paper color
  (if (eq new (paper))
      (rink)
      (ink new) )
)))

(de :NEWINK (&AUX (old (ink)) )
; set the ink to a different color
  (rink)
  (if (eq old (ink))
      (newink) )
)))

(de :RPOS (&AUX (v (vp)) )
; move turtle to random position
  (pen 0)
  (pos (add (first v) -160 (rn 320))
	(add (second v) -75 (rn 150)) )
  (pen 1)
)))


;	       D R A W I N G   R O U T I N E S


(de :CIR (&OPT (n 10))
  (rep 36 (fd n)(tl 10))
)))

(de :SQ (&OPT (n 50))
  (rep 4
	 (fd n)
	 (tl 90) )
)))

(de :TRI (&OPT (n 70))
  (rep 3
	 (fd n)
	 (tl 120) )
)))

;		D E M O S

(pl "Start demos by typing (SS) <enter> then one of the following:")

(pl "(DEMO1) random patterns")

(de :DEMO1 (&AUX (a (add 60 (rn 120))) )
  (tm 0)	; window mode
  (home)
  (:all (vis 0))
  (cs)
 ; (vis 1)
  (rink)
  (for (i 1 150)
	 (if (memq i '(50 100))
	     (newink) )
	 (fd i)
	 (tl a) )
  (if (tys)
      (demo-stop)
      (demo1) )
)))

(pl "(DEMO2) random patterns with multiple turtles")

(de :DEMO2 (&SPL i) ; ** all
  (demo-start)
  (if (lt (length (tf)) 2)
      (rep 3 (hatch 'andy)) )
  (cs)
  (rep 3
	 (:all (my (add 60 (rn 120))) 
	      (rpos)
	      (rink) )
	 (for  (i 1 35)
		(if (not (tys))
		    (:all (if (memq i '(13 25))
			     (newink) )
			 (fd i)
			 (tl (my)) ) ) )
	  )
  (if (tys)
      (demo-stop)
      (demo2) )
)))

(pl "(DEMO3) random patterns with multiple turtles without clearscreen")

(de :DEMO3 (&SPL i) ; ** all
  (demo-start)
  (if (lt (length (tf)) 2)
      (rep 3 (hatch 'andy )) )
   (rep 3
	 (:all (my (add 60 (rn 120))) (rpos) (rink))
	      (for (i 1 35)
	      (if (not (tys))
		  (:all (if (memq i '(13 25))
			   (newink) )
		       (fd i) (tl (my))) ) )
	  )
   (if (tys)
	(demo-stop)
	(demo3) )
)))

(pl "(DEMO4) randomly moving turtles that stop at fence")

(de :DEMO4 ()
 (vis 0)  (cs)
  (tm 1)		; fence 'em in
  (if (lt (length (tf)) 2)
      (rep 3 (hatch 'andy)) )
  (:all
    (rink)
;    (vis 1)
    (shape nil) )
  (forever
    (:all (fd (rn 10))
	  (tl (rn 45))
	  (tr (rn 45)) ))
)))

(pl "(DEMO5) randomly moving turtles that bounce off fence")

(de :DEMO5 ()
; like demo4 but with bouncing turtles
  (pl "Press any key to change ink colors")
 (vis 0)  (cs)
  (tm 2)		; fence error mode
  (if (lt (length (tf)) 2)
      (rep 3 (hatch 'andy)) )
  (:all 
    (rink) 
;    (vis 1) 
    (shape nil) )
  (forever
    (:all (:bounce))
    (iftrue (tys)
      (:all (newink))
      (console-in) ) ) 	; eat the char
)))

(de :BOUNCE ()
; move turtle at random, bounce if hit wall
  (tr (rn 45))
  (tl (rn 45))
  (if (catch error 
	 (let ((error throw-error))
	    (fd (rn 10))
	    nil ))
      (tr (add 100 (rn 160))) )
)))

(de DEMO-START ()
  (:all (vis 0))
  (tm 0)	; clip
)))

(de DEMO-STOP ()
;  (:all (vis 1))
)))

(pl "Stop demos by typing control-G")

"DEMO.LSP loaded"

)))
 
