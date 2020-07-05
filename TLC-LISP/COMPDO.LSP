;
;          copyright (c) 1984, 1985, The LISP Company
;
; COMPDO.LSP -- lisp compiler for DO

(setq package (pkg "comp"))

(include "compdecl")


(dc :DO (exp 
  &AUX (vars (second exp))
	(exit (third exp))
	(body (rest exp 3))
	(special-flag nil)
	(putval-label (gensym "var"))
	(unbindlabel (gensym "exit"))
	(itercode-label (gensym "iter"))
	(orig-local-list local-list)
	(local-list (do-local-list vars))
	(init (do-init vars putval-label orig-local-list))
	(init-code (first init))
	(iter-code (second init))
  &SPL special-flag )
;
  (append+ (if special-flag
		(list (mk-mark))
		nil )
	    init-code
	    (exitcode exit unbindlabel)
	    (comp-exp-flush body)
	    (concat+ (mk-disc)
		     (append+ iter-code
			      (list (mk-jump putval-label)
				    unbindlabel )
			      (if special-flag
				  (list (mk-unbind))
				  nil ))))
)))

(de DO-INIT (vartriples put-label 
  &OPT (orig-local-list local-list)
  &AUX (initcode nil) 
	(namelist nil)
	(itercode nil) )
; Returns a list of TWO elements: code to initialize do vars and
; code for iteration values
  (do ( (trips (reverse vartriples) (rest trips)) )
      ( ((null trips) (list (append+ (bindvars namelist)
				      initcode
				      (concat put-label (genputvals namelist)) )
			     itercode )) )
      (let* ( (currenttriple (first trips))
	       (var (first currenttriple))
	       (init (second currenttriple))
	       (iter (third currenttriple)) )
	 (setq namelist (concat var namelist))
	 (selectq (length currenttriple)
	   (1			; no init or iter
	     (setq initcode (concat (mk-unbound) initcode))
	     (setq itercode (concat (mk-getval var) itercode)) )

	   (2			; no iter
	     (setq initcode (let ( (local-list orig-local-list) )
				 (append (comp-exp init) initcode) ))
	     (setq itercode (concat (mk-getval var) itercode)) )

	   (3			; all three
	     (setq initcode (let ( (local-list orig-local-list) )
				 (append (comp-exp init) initcode) ))
	     (setq itercode 
		   (append (comp-exp iter) itercode )) )
	   (ow
	     (error "Bad syntax in DO"	currenttriple) ) ) ) )
)))

(de DO-LOCAL-LIST (l &AUX (ll (do-locals1 l)) )
; extract variables and check for duplicates
  (cond
    ( local-flag
      (setq local-size (add local-size (length ll)))
      (do-locals2 ll)
      (append (gen-locals ll (length local-list))
	       local-list ) )
    ( t
      (setq special-flag t)
      nil ) )
)))

(de DO-LOCALS1 (l &AUX (var (first (first l))) )
; extract variables from do variable list
  (if (null l)
      nil
      (concat var (do-locals1 (rest l))) )	 
)))

(de DO-LOCALS2 (l &AUX (var (first l)) )
; set SPECIAL-FLAG if any variables are special
  (cond
    ( (null l) )
    ( (memq var (rest l))
      (error ':duplicate-do-symbol var) )
;    ( (assoc var local-list)
;	(error ':do-symbol-same-as-argument-sumbol var) )
    ( (memq var special-list)
      (setq special-flag t)
      (do-locals2 (rest l)) )
    ( t
      (do-locals2 (rest l)) ) )
)))

(de GENPUTVALS (varlist)
  (do ( (vars varlist (rest vars))
	 (code nil (concat (mk-popval (first vars)) code)) )
      ( ((null vars) code) ) )
)))

(de EXITCODE (exitpairs unbindlab &AUX (nextlab (gensym "next")) )
  (append (comp-clause (first exitpairs)
			unbindlab
			nextlab )
	   (fakecond (rest exitpairs)
		      unbindlab
		      nextlab ) )
)))

(de FAKECOND (alts exitlab currentplab &AUX (nextplab (gensym "next")) )
  (cond
    ( (null alts) 
      (list currentplab) )
    ( t 
      (concat currentplab
	       (append (comp-clause (first alts)
				    exitlab
				    nextplab )
		       (fakecond (rest alts)
				 exitlab
				 nextplab )))))
)))

(de BINDVARS (varlist &AUX (sym (first varlist))
			    (loc (cdar (assoc sym local-list))) )
  (cond
    ( (null varlist)
      nil )
    ( loc
      (bindvars (rest varlist)) )
    ( t
      (concat (mk-bind (first varlist))
	       (bindvars (rest varlist)) ) ) )
)))

"COMPDO.LSP loaded"

)))
