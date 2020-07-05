;
;          copyright (c) 1984, 1985, The LISP Company
;
; COMPARGS.LSP -- formal parameter compiler

(setq package (pkg "comp"))

(include "compdecl")


;	 A R G	 L I S T   E X T R A C T O R S


(de GET-ARGS (arglist)
; return the list of required args
  (if (or (null arglist)
	   (memq (first arglist) '(&rest &opt &optional &aux &special &spl)) )
      nil
      (concat (first arglist)
	       (get-args (rest arglist)) ))
)))

(de GET-AUXES (arglist)
  (get-middle arglist '&AUX)
)))
  
(de GET-REST (arglist)
  (get-middle arglist '&rest)
)))
  
(de GET-OPTS (arglist &AUX (try (get-middle arglist '&opt)) )
  (if try
      try
      (get-middle arglist '&optional) )
)))

(de GET-SPECS (arglist &AUX (try (get-middle arglist '&spl)) )
  (if try
      try
      (get-middle arglist '&special) )
)))
  
(de GET-MIDDLE (lst key)
; Return a list of all elements in lst between key and the next indicator
  (cond
    ( (null lst) nil)
    ( (eq (first lst) key)
      (get-middle1 (rest lst)) )
    ( t
      (get-middle (rest lst) key) ) )
)))

(de GET-MIDDLE1 (lst)
  (if (or (null lst)
	   (memq (first lst) '(&rest &aux &special &spl)) )
      nil
      (concat (first lst)
	       (get-middle1 (rest lst)) ))
)))
    
(de GET-LOCALS (arglist &AUX (locs (mapcar get-symbol 
					    (append+ arg-list 
						     opt-list
						     aux-list )))
			      (l (if local-flag
				     (gen-locals locs 0)
				     nil )) )
  (check-locals l)
  l
)))

(de GET-LOCAL-SIZE ()
  (add (length arg-list)
	(length opt-list)
	(length aux-list) )
)))

(de CHECK-LOCALS (l &AUX (var (cdar l)) )
; check for duplicate symbols
  (cond
    ( (null l) )
    ( (assoc var (rest l))
      (error ':duplicate-local-symbol-name var) )
    ( t
      (check-locals (rest l)) ) )
)))

(de GET-SYMBOL (a)
; return the arglist symbol without default expressions
  (if (symbolp a)
      a
      (first a) )
)))

(de GEN-LOCALS (l n &AUX (a (first l)) )
; generate the local reference alist
  (cond
    ( (null l) nil)
    ( (memq a special-list)
      (concat (cons n n)
	       (gen-locals (rest l) (add1 n)) ) )
    ( t
      (concat (cons a n)
	       (gen-locals (rest l) (add1 n)) ) ) )
)))
  

;	 A R G	 L I S T   C O M P I L E R S

    
(de COMP-ARGLIST (args &AUX (first-opt (length arg-list))
			     (first-rest (add first-opt (length opt-list)))
			     (first-aux (add first-rest (length rest-list))) )
  (append+ 
	    (comp-args arg-list 0)
	    (comp-opts opt-list first-opt)
	    (comp-rest rest-list first-rest)
	    (comp-auxes aux-list first-aux) )
)))

(de COMP-ARITY (typ)
  (list (selectq typ
	   (fexpr 255)
	   (macro 254)
	   (ow (length arg-list)) )
	 (length opt-list)
	 (sub local-size (length opt-list) (length arg-list))
	 0 )
)))
 
(de COMP-ARGS (args arg-number)
  (if (null args)
      nil
      (append (comp-arg (first args) arg-number)
	       (comp-args (rest args) (add1 arg-number)) ))))

(de COMP-ARG (arg num &AUX (loc (assoc arg local-list)) )
  (cond
    ( loc	; local symbol, no code generated
      nil )
    ( t
      (concat+ (mk-fget num)
		(comp-arg-bind arg) ) ) )  
)))

(de COMP-AUXES (args num)
  (if (null args)
      nil
      (append (comp-aux (first args) num)
	       (comp-auxes (rest args) (add1 num)) ) )
)))

; the arg can be an symbol or (symbol init-expr)
(de COMP-AUX (arg num &AUX (loc (cdar (assoc (if (symbolp arg) arg (first arg))
					local-list ))) )
  (cond
    ( (and loc (listp arg))	; local aux with default
      (append (comp-exp (second arg))
	       (list (mk-fset loc)) ) )
    ( loc			; local aux without default, generates no code
      nil )
    ( (listp arg)		; named aux with default
      (append (comp-exp (second arg))
	       (comp-arg-bind (first arg)) ))
    ( t			; named aux without default
      (concat (mk-unbound)
	       (comp-arg-bind arg) ) ) )
)))
      
(de COMP-OPTS (args num )
  (if (null args)
      nil
      (append (comp-opt (first args) num)
	       (comp-opts (rest args) (add1 num))))))

(de COMP-OPT (arg num &AUX (done-lab (gensym "optok"))
			    (default-lab (gensym "noopt"))
			    (loc (cdar (assoc (if (symbolp arg)
						  arg
						  (first arg))
					      local-list ))) )
  (cond
    ( (and loc (listp arg))	; local with default
      (concat+ (mk-fget num)
		(mk-jbound done-lab)
		(append (comp-exp (second arg))
			(concat+ done-lab
				 (list (mk-fset loc)) ))) )
    ( loc			; local without default
      nil )	  
    ( (symbolp arg)		; special without default expression
      (concat+ (mk-fget num)
		(comp-arg-bind arg) ) )
    ( t			; special with default expression
      (concat+ (mk-fget num)
		(mk-jbound done-lab)
		(append (comp-exp (second arg))
			(concat+ done-lab
				 (comp-arg-bind (first arg)) ))) ) )
)))

; *** DOESN'T WORK
(de COMP-REST (rest-list num)
  (if (null rest-list)
      nil
      (throw hack 'comp-rest)
      (pl "COMP-REST " rest-list \  num)
      (let ( (lab (gensym "rest"))
	      (arg (first rest-list))  )
	 (concat+ (mk-@list (sub (length arg-list)))
		  (comp-arg-bind arg) )))
)))

(de COMP-ARG-BIND (arg)
  (list (mk-arg arg)))))

"COMPARGS.LSP loaded"

)))
