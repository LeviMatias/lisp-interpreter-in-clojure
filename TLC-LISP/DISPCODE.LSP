;
;          copyright (c) 1984, 1985, The LISP Company
;
(setq package comp:)

(declare)

;		       D I S S A S S E M B L E R

(de :DIS (p)
  (if (pcodep p)
      (:dis-pcode p)
      (:dis-asm p) )
)))

(setq :d :dis)

(de :DIS-PCODE (p &AUX (pt (pointer p))
			(dis-seg (segment p))
			(dis-off (offset p))
			(end (add dis-off (length p)))
			(local-size 0)
			size code op prop 
		   &SPL pt dis-seg dis-off )
  (dis-addr)
  (rep 4
	(prin0 (examine pt))
	(setq local-size (add local-size (examine pt)))
	(bump-addr) )
  (terpri)
  (do ()
      ( ((ge dis-off end)) )
      (dis-addr)
      (setq code (examine pt)
	     op (vref ops (add1 code))
	     prop (getprop op 'lap)
	     args (opargs prop) )
      (prin2 op)
      (bump-addr)
      (dis-args args) )
)))

(de DIS-ARGS (l &AUX (a (first l)) count )
; uses PT
  (if (null l)
      (terpri)
      (selectq a
	 (byte
	   (prin0 (examine pt))
	   (bump-addr) )
	 (wlabel
	   (prin2 (hexword (add (examine-word pt) 2 dis-off)))
	   (bump-addr 2) )
	 (blabel
	   (prin2 (hexword (add (examine pt) 1 dis-off)))
	   (bump-addr) )
	 (obj-count
	   (setq count (prin0 (examine pt)))
	   (bump-addr)
	   (rep count
		(terpri)
		(dis-addr)
		(prin3 "obj ")
		(prin0 (examine-obj pt))
		(bump-addr 2) ) )
	 ((symbol obj)
	   (prin0 (examine-obj pt))
	   (bump-addr 2) )
	 (word
	   (prin2 (hexword (examine-word pt)))
	   (bump-addr 2) )
	 (ow
	   (error 'dis-args a) ) )
      (dis-args (rest l)) )
)))
      
(de DIS-ADDR ()
  (pl0 (hexword dis-seg) \: (hexword dis-off) "  ")
)))

(de BUMP-ADDR (&OPT (n 1) )
  (setq dis-off (add n dis-off)
	 pt (add n pt) )
)))

"DISPCODE.LSP loaded"

)))
