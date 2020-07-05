;
;          copyright (c) 1984, 1985, The LISP Company
;
; COMP2.LSP -- lisp compiler

(setq package (pkg "comp"))

(include "compdecl")

;		T O P L E V E L   S T U F F


(dt EVAL-WHEN (l &AUX (when (second l))
		       (forms (rest l 3)) )
  (if (memq 'compile when)
      (apply progn forms)
)))

(dt DE (l)

)))

(dt PROGN (l)

)))

;	 C O D E   G E N E R A T I N G	 F U N C T I O N S


; LOCAL-LIST FORMAT: ( (name reqd opt type) ...)


(de INFO-ARITY (l)
  (if l
      (add (second l) (third l))
      nil )
)))

(de MK-PCALL (arity name 
  &AUX (info (first (assoc name local-fcn-list)))
	(unb-count (sub (info-arity info) arity)) )
  (if (null unb-count)
      (error "Forward reference" name) )
  (if (minusp unb-count)
      (error "Wrong arg count" name) )
  (list ':PC unb-count name)
)))

(de IS-CONSTANT (sym)
  (memq sym constant-list)
)))

;(de IS-CONSTANT (sym) &AUX (s (pname sym)) )
;  (and (not (memq sym special-list))
;	 (eq (nth s 1) \*)
;	 (eq (nth s (length s)) \*) )
;)))

(de MK-GETVAL (var &AUX (loc (cdar (assoc var local-list))) ) 
  (cond
    ( (null var)
      (mk-nil) )
    ( loc
      (mk-fget loc) )
    ( (cdar (assoc var '( (edit:x :x)
			   (edit:y :y)
			   (edit:ed-buffer :edbuf)
			   (edit:xoffset xo)
			   (edit:yoffset yo) ))) )
    ( (is-constant var)
      (mk-const (eval var)) )
    ( t
      `(PUSHVAL ,var) ) )
)))

(de COMP-PUTVAL (var &AUX (loc (cdar (assoc var local-list))) ) 
  (if loc
      (list (mk-dupl)
	     (mk-fset loc) )
      (list (mk-dupl) 
	     (mk-popval var) ) ))
)))

(de MK-POPVAL (var &AUX (loc (cdar (assoc var local-list))) ) 
  (if loc
      (mk-fset loc)
      `(POPVAL ,var) )
)))

(de MK-COMMENT (l)
  `(COMMENT ,l) 
))))

(de MK-CONST (const) 
  (cond
    ( (null const)
      (mk-nil) )
    ( (and (fixp const)
	    (ge const 0)
	    (le const 7) )
      (mk-fix const) )
    ( t
     `(CONST ,const) ) )
)))

(de MK-JTYPE (typ lab &AUX (num (if (fixp typ)
				     typ
				     (typenum typ) )) )
  `(jtype ,num ,lab)
)))

(de COMP-OBJS (l)
  (if (null l)
      nil
      (concat (mk-obj (first l))
	       (comp-objs (rest l)) ))
)))

(de ALL-CONST (l)
  (cond
    ( (null l) t )
    ( (memvec (type (first l)) [symbol char fix])
      (all-const (rest l)) )
    ( t nil ) )
)))

(de COMP-MEM (l)
; generate (MEM) opcode if possible else named call to MEMQ
  (if (and (listp l)
	    (all-const l) )
      (concat (mk-mem (length l))
	       (comp-objs l) )
      (concat (mk-const l)
	       (list (mk-ncall 2 'memq)) ))
)))

(de ALL-CONST-VEC (v &AUX (len (length v)) )
  (do ( (i 1 (add1 i)) )
      ( ((gt i len) t)
	 ((not (memvec (type (vref v i)) [fix symbol char])) nil) )
      )
)))

(de COMP-MEM-VEC (vec
  &OPT (len (length vec))
	(i 1) )
  (if (gt i len)
      nil
      (concat (mk-obj (vref vec i))
	       (comp-mem-vec vec len (add1 i)) ))
)))

(dc MEMVEC (exp &AUX (pat (second exp))
		      (vec (third exp)) )
; generate (MEM) opcode if possible else named call to MEMVEC
  (append (comp-exp pat)
	   (if (and (vectorp vec)
		    (all-const-vec vec) )
	       (concat (mk-mem (length vec))
		       (comp-mem-vec vec) )
	       (append (comp-exp vec)
		       (list (mk-ncall 2 'memvec)) ) ))
)))

(de MK-CMARK (name label)
  `(CATCH ,name ,label) 
)))

(de MK-RET ()
; invoked by optimizer, do not make mk-ret a macro
  `(RET)
)))

(de MK-DUPL ()
; ditto
   `(DUPL) 
)))


;	 C O M P I L E	 F U N C T I O N S   O N   T H E   P L I S T


(dc ESCAPE (l)
  l)))))

(dc SETQ (exp)
  ; remove SETQ
  (comp-assgn2 (rest exp))
)))

(de COMP-ASSGN2 (exp)
  ; the entire setq expr must leave exactly one value on the stack
  ; so popval all but the last one
  (if (eq (length exp) 2)
      (append (comp-exp (second exp)) 
	       (comp-putval (first exp)) )
      (append+ (comp-exp (second exp)) 
		(list (mk-popval (first exp)))
		(comp-assgn2 (rest (rest exp))) )))
)))

(dc LET (exp &AUX (putvals-label (gensym "var"))
			(init (do-init (second exp) putvals-label)) )
  (concat+ (mk-mark)
	    (append+ (first init)
		     (comp-exp-flush (rest (rest exp)))
		     (list (mk-unbind) )))
)))

(dc PROG1 (exp &AUX (n (length (rest exp))) )
  (append (comp-exps (rest exp))
	   (selectq n
	     (0 (list (mk-const nil)))
	     (1 nil)
	     (2 (list (mk-disc)))
	     (ow (list (mk-flush (- n 2))
		       (mk-disc) ) ) ))
)))

(dc PROGN (exp)
  (comp-exp-flush (rest exp))
)))

(de COMP-EXP-FLUSH (exps &AUX (exp (first exps)) )
; compile a list of forms and flush all but the last value
; return nil if exps is nil
  (cond
    ( (null exps)
      (list (mk-const nil)) )
    ( (null (rest exps))	; last or only exp
      (comp-exp exp) )
    ( (memvec target-type [code asm])
      (append+ (comp-exp exp)
		(list (mk-disc))
		(comp-exp-flush (rest exps)) ) )
    ( t
      (append (comp-exps exps)
	       (list (mk-flush (sub1 (length exps))) )) ) ) 
)))

(dc AND (exp &AUX (forms (rest exp))
			(endlab (gensym "exit"))
			(loselab (gensym "lose")) )
  (if (null forms)
      (list (mk-const t))
      (append (comp-and1 forms endlab loselab)
	       (list loselab
		     (mk-const nil)
		     endlab) ) )
)))

(de COMP-AND1 (form endlab loselab &AUX (exp (first form)) )
  (if (rest form)
      (append (comp-exp exp)
	       (concat (mk-iff loselab)
		       (comp-and1 (rest form) endlab loselab) ))
      ; else last one
      (append (comp-exp exp)
	       (list (mk-iffn loselab)
		     (mk-jmp endlab) ) ))
)))

(dc OR (exp &AUX (forms (rest exp))
		       (endlab (gensym "exit"))
		       (loselab (gensym "lose")) )
  (if (null forms)
      (list (mk-const nil))
      (append (comp-or1 forms endlab loselab)
	       (list loselab
		     (mk-const nil)
		     endlab) ) )
)))

(de COMP-OR1 (form endlab loselab &AUX (exp (first form))
					(nextlab (gensym "next")) )
  (if (rest form)
      (append (comp-exp exp)
	       (concat+ (mk-iffn nextlab)
			(mk-jmp endlab)
			nextlab
			(comp-or1 (rest form) endlab loselab) ))
      ; else last one
      (append (comp-exp exp)
	       (list (mk-jmp endlab) )))
)))

(dc IF (exp &AUX (exitlab (gensym "endif"))
		       (otherlab (gensym "iffalse"))
		       (val2 (rest (rest (rest exp)))) )
  (append (comp-clause (list (second exp) (third exp))
			exitlab
			otherlab )
	   (concat otherlab
		   (append (comp-exp-flush val2)
			   (list exitlab) )))
)))

(dc SELECTQ (exp &AUX (match (second exp))
		       (choices (rest (rest exp))) )
  (append (if (and match (listp match))
	       (comp-exp match)
	       (list (if (symbolp match)
			 (mk-getval match)
			 (mk-const match) )))
	   (comp-choices choices (gensym "exit")) )
)))

(de COMP-CHOICES (l exit-lab &AUX (skip-lab (gensym "next"))
				   (matcher (first (first l)))
				   (choice (rest (first l))) )
; l is ( (matcher exp exp ...) ...)
; matcher is atom or (atom atom ...)
; choices is (exp exp ...)
; value of match is on stack
  (cond
    ( (null l) 				; no otherwise clause
      (concat+ (mk-const nil)			; value of selectq
		exit-lab			; value then match on stack
		(list (mk-flush 1)) ) ) 	; match
    ( (memvec matcher [t ow otherwise])
      (append (comp-exp-flush choice)
	       (concat exit-lab
		       (list (mk-flush 1)) )) )
    ( t
      (concat (mk-dupl)		  ; match
	       (append+ (if (and matcher (listp matcher)) 
			    (comp-mem matcher)
			    (list (mk-const matcher) (mk-ieq)) )
			(list (mk-iff skip-lab))
			(comp-exp-flush choice)
			(concat+ (mk-jump exit-lab)
				 skip-lab
				 (comp-choices (rest l) exit-lab) ))) ) )
)))

'(dc CATCH (exp &AUX (endlabel (gensym "end"))
			  (atm (second exp)) )
 (concat+ (mk-cmark atm endlabel)
	   (append (comp-exp-flush (rest (rest exp)))
		   (list (mk-pcmark)
			 endlabel )))
)))

'(dc THROW (exp)
  (append (comp-exp-flush (rest (rest exp)))
	   (list (mk-fcmark (second exp))) )
))))

(dc COND (exp &AUX (exitlab (gensym "exit"))
			 (nextlab (gensym "next")) )
; exp is (COND (pred conseq ...) ...)
  (append (comp-cond-clause (second exp) exitlab nextlab)
	   (comp-cond2 (rest (rest exp)) 
		       nextlab
		       exitlab ) )
)))

(de COMP-COND2 (alts currentplab exitlab &AUX (nextlab (gensym "next")) )
  (cond 
    ( (null alts) 
      (list currentplab (mk-const nil) exitlab) )
    ( t 
      (concat currentplab
	       (append (comp-cond-clause (first alts) exitlab nextlab)
		       (comp-cond2 (rest alts)
				   nextlab
				   exitlab ))) ) )
)))

(de COMP-COND-CLAUSE (exp exitlab nextplab &AUX (pred (first exp))
						 (conseq (rest exp)) )
; exp is (pred conseq1 conseq2...)
; eval pred, jump to nextlab if false else eval conseq and jump to exitlab
; (pred) without conseq must return value of pred
  (append+ (comp-exp pred)
	    (if conseq
	       (concat+ (mk-iff nextplab)
			(comp-exp-flush conseq) )
	       ; else no conseq
	       (list (mk-iffn nextplab)) )
	    (list (mk-jump exitlab)) ))
)))

(dc QUOTE (exp)
  (list (mk-const (second exp)))
)))

(dc LIST (exp &AUX (args (rest exp))
			 (len (length args)) )
  (append (comp-exps args)
	   (list (mk-list len)) )
)))

(dc STRING (exp &AUX (args (rest exp))
			   (len (length args)) )
  (append (comp-exps args)
	   (list (mk-string len) )
)))

(dc REP (exp 
  &AUX (count (second exp))
	(body (rest exp 2))
	(rep-loop (gensym "rep-loop-"))
	(rep-end (gensym "rep-end-")) )
  (append+ (comp-exp count)
	    (list (mk-repinit rep-end)
		  rep-loop )
	    (append (comp-exp-flush body)
		    (list (mk-repjmp rep-loop)
			  rep-end )))
)))

(dc BOUNDP (exp &AUX (arg (second exp))
			   (bplab (gensym "ub"))
			   (bpendlab (gensym "ubend")) )
  (cond
    ( (symbolp arg)
      (comp-expr exp) )
    ( (and (listp arg) 	; (boundp 'local)
	    (eq (first arg) 'quote)
	    (setq loc (cdar (assoc (second arg) local-list))) )
      (concat+ (mk-fget loc)
		(mk-jtype 'unbound bplab)
		(mk-disc)
		(mk-const t)
		(mk-jmp bpendlab)
		bplab
		(mk-disc)
		(mk-nil)
		(list bpendlab) ) )
    ( t
      (comp-expr exp) ) )
)))

(de QUOTED-EXP (exp)
  (and (listp exp)
	(eq (car exp) 'quote) )
)))

(dc EQ (exp &AUX (arg1 (second exp))
		       (arg2 (third exp))
		       (eq-list '(fix char)) )
  (append (comp-exps (rest exp))
	   (if (or (memq (type arg1) eq-list)
		   (memq (type arg2) eq-list)
		   (quoted-exp arg1)
		   (quoted-exp arg2) )
	       (list (mk-ieq))
	       (list (mk-eq)) ))
)))

(dc (PRINT PRIN0 PRIN1 PRIN2 PRIN3) (expr)
; special case printers with sink present
  (if (neq (length expr) 3)
      (comp-expr expr)
      (append+ (list (mk-mark))
		(comp-expr expr)
		(list (mk-unbind)) ))
)))


(dc (READ TERPRI) (expr)
; special case terpri and read with source present
  (if (neq (length expr) 2)
      (comp-expr expr)
      (append+ (list (mk-mark))
		(comp-expr expr)
		(list (mk-unbind)) ))
)))

(dc REST (exp)
  (append (comp-exp (second exp))
	   (if (eq (length exp) 2)
	       `((cdr))
	       (append (comp-exp (third exp))
		       (list (mk-ncall 2 'rest)) )))
)))

(dc FUNCALL (exp)
  (append (comp-exps (rest exp))
	   `( (funcall ,(sub1 (length exp))) ) )
)))

;		 I N I T I A L I Z A T I O N


(de ALIAS (name alias)
  (putprop name 'compile-expression (getprop alias 'compile-expression))
)))

(alias '= 'eq)

(de CSS (atm)
; define an opcode that is the same as a subr
  (putprop atm 'compile-expression (list atm)) )))

(de CSSS (atm alias)
  (putprop atm 'compile-expression (list alias)) )))

(css 'car)
(css 'cdr)
(css 'caar)
(css 'cadr)
(css 'caddr)
(css 'assoc)
(css 'nconc)
(css 'vref)
(css 'vset)
(css 'cons)
(css 'gt)
(css 'le)
(css 'ge)
(css 'lt)

(csss 'concat 'cons)
(csss '< 'lt)
(csss '> 'gt)
(csss '<= 'le)
(csss '>= 'ge)

(css 'add1)
(css 'sub1)
(css 'zerop)
(css 'minusp)
(css 'rplaca)
(css 'rplacd)
(css 'append)
(css 'nconc)
(css 'type)
(css 'nth)
(css 'not)
(css 'length)

(csss 'null 'not)
(csss 'first 'car)

(remove-macros)

"COMP.LSP loaded"

)))
