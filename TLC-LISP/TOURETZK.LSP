;
;          copyright (c) 1984, 1985, The LISP Company
;
; 
;

; This file includes those function that are needed to make
;  TLC-LISP86 compatible with most of the LISP described in:
;
;     LISP: A Gentle Introduction to Symbolic Computation
;		    by David Touretzky
;	     published by Harper&Row, 1984

; For a definition of "most of" see Part II of the TLC-LISP
; documentation.

; Define a new package to hold the code:
;
(pkg "dt"  sys:)

; Now enter that package:
; 
(setq package dt:)

; Build local copies of first and rest to protect the system's versions:
;
(insert "first")   (insert "rest")

; Now define a collection of Touretzky's primitives either by
; renaming TLC's functions or by adding new functions.
;
(de oddp (x) (eq (rem x 2) 1)))

(setq greaterp gt)

(setq lessp lt)

(setq plus add)

(setq times mul)

(setq quotient div)

(setq remainder rem)

(de cadddr (x) (car (cdddr x)))

(setq defun de)

(setq makunbound unbind)

; The treatment of the predicate LISTP differs. Under duress, TLC 
; is compatible with Zeta-LISP -- (LISTP NIL) is NIL even though
; NIL is a list. 
;
(setq consp :listp)

(defun dt:listp (x)(or (:listp x) (null x)))

; Now we  define an n-ary APPEND function to replace our APPEND+
; We use a macro expander lurking in the UTIL: package.
;
(dm dt:APPEND (l)
  (if (le (length l) 2)
      (error 'append)
      (util:common+ ':append (rest l)) )
)))

; Now some more simple primitives, please:

(de ncons (x) (cons x ())))

(de last (x) 
   (if (null x) nil
	(do ((x x (cdr x)))
	    (((null (cdr x)) x)))))


; NTHCDR -- A variation of our extended REST function.
; Notice that we use :rest to get to the sys: package.
;
(de nthcdr (n x) (:rest x n))))

; NTH -- Another variation of a TLC function. Here, the order of 
;	   arguments is different and we are 1-based while Touretzky
;	   is 0-based.
;
(de dt:nth (n x) (:nth x (add1 n))))


; Next, a collection of more meaty functions: set operations.
; See Touretzky's Appendix C, section 1.3.
;
; We wrote our definitions to highlight the similarities between
; the operations.
;
(de intersection (sx sy)
   (set-op and sx sy)))
     
(de setdifference (sx sy)
   (set-op or sx sy)))))

(de set-op (fn s1 s2 &aux (result nil))
    (do ((s1 s1 (cdr s1)))
	 (((null s1) (reverse result)))
	 (fn (:member (car s1) s2)
	     (setq result (cons (car s1) result)))))

(de union (x y)(:append x (setdifference y x))))

; MSG -- a printer hack. Notice that we use a mapping function
;	   to control the printer. Notice particularly that we
;	   write (LAMBDA ...) rather than '(LAMBDA ...)  -- a 
;	   major philosophical point.
;
(df msg (%%l)
  (mapn (lambda (%x)
	    (if (eq %x t) (terpri)
		(prin3 (eval %x))))
	 %%l)
   t)))))

; ASSOC and ASSQ -- two association list functions. Here we 
;		      again use functional arguments, and again
;		      write EQ and EQUAL, not 'EQ or 'EQUAL.
;
(de dt:assoc (i %l) (ass equal i %l)))

(de dt:assq (i %l) (ass eq i %l))))

(de dt:ass (bin-op i l)
   (cond ((null l) nil)
	  ((bin-op (caar l) i) (car l))
	  ((ass bin-op i (cdr l))))))))


; MEMBER and MEMQ -- versions of TLC functions, and variations
;		       of ASSOC and ASSQ.
;
(de dt:member (i %l) (mem equal i %l))))
  
(de dt:memq (i %l) (mem eq i %l)))))

(de dt:mem (bin-op i l)
   (cond ((null l) nil)
	  ((bin-op (car l) i) l)
	  ((mem bin-op i (cdr l)))))))

; SUBLIS -- amusing only for its use of an internal LET.
;
(de sublis (tbl l)
    (cond ((atom l)
	     (let ((item (:assoc l tbl)))
		  (if item (cdar item) l)))
	   ((let ((left (sublis tbl (car l)))
		  (right (sublis tbl (cdr l))))
		 (if (and (eq (car l) left)
			  (eq (cdr l) right))
		     l
		     (cons left right))))))))

; DISPLACE -- a renaming of RPLACB (a rosie by any other name ...)
;
(setq displace rplacb)

; FIND-IF -- Page 169 of Touretzky, except we don't need (or want)
;		the ragument quoted, and we don't need the explicit
;		call on APPLY. See how much nicer it is?
;
(de find-if (fn l)
   (do ((l l (cdr l)))
	(((null l) nil)
	 ((fn (car l)) (car l))))))

; SUBSET -- Page 172 of Touretzky. See the comment on FIND-IF.
;
(de subset (fn l)
   (do ((l l (cdr l)) (result nil))
	(((null l) (reverse result)))
	(if (fn (car l)) (setq result (cons (car l) 
					    result)))))

; APPLY-TO-ALL -- a generalized version of MAPCAR. Here APPLY
;		    is useful, but please, functional arguments
;		    not names of functions.
;
(de dt:apply-to-all (fn &rest l)
   (if (or (null l) (dt:null-in l)) nil
	(cons (apply fn (dt:firsts l))
	      (apply apply-to-all (cons fn (dt:rests l))))))

(de dt:null-in (l)
  (cond ((null l) nil)
	 ((null (car l)) t)
	 (t (null-in (cdr l))))))
	  
(de dt:firsts (l)
   (if (null l) nil
	(cons (caar l)(firsts (cdr l)))))
	
(de dt:rests (l)
   (if (null l) nil
	(cons (cdar l) (rests (cdr l))))))
	
; EVERY -- what you should expect by now.
;
(de every (fn l)
   (do ((l l (cdr l)))
	(((null l) t)
	 ((not (fn (car l))) nil))))

; REDUCE -- See the comments in Part II of the documentation.
;
(de reduce (fn l id) (left-reduce fn l id))

(de left-reduce (fn l id)
   (if (null l) id
	(left-reduce fn (cdr l) (fn id (car l)))))

(de right-reduce (fn l id)
   (if (null l) id
	(fn (car l) (right-reduce fn (cdr l) id)))))


; PRSYM -- A less obtrusive symbol printer. The current TLC-LISP
;	     printer supplies gratitious package prefixes. Tacky.
;
(de prsym (x &aux (pkg (symbol-pkg x)) (package package))
  (do ((c-pkg package (super-pkg c-pkg)))
      (((null c-pkg) (prold x))
	((eq c-pkg pkg) (setq package c-pkg) (prold x))) 
	))))

; ENTER-DT  and LEAVE-DT -- Functions to enter and leave the DT-package.
;
(de :enter-dt (&aux (prv (pr 'symbol))) 
 (setq package dt:)
 (if (eq (type prv) 'subr) (setq prold prv))
 (pr 'symbol prsym)
 "entering touretzky mode"))))))

(de leave-dt ()
 (pr 'symbol prold)
 (setq package sys:)
 "leaving touretzky mode"))))))))


; That's it. If we've forgotten pieces, please let us know.
