;
;          copyright (c) 1984, 1985, The LISP Company
;
; COMP.LSP -- lisp compiler

(setq package (pkg "comp"))

; Notes
;
; We use the convention that MK-xxx return single opcode lists
; and COMP-xxx return lists of at least one opcode list
;

(eval-when (eval compile)
  (load "compmacs")
)))

(include "compdecl")

(eval-when (eval pload)
  (setq default-special '(current-source current-sink current-err 
			  smash package error read-table toplev file-list 
			  error toplev gcbeep )
	default-inline	'(car cdr symbolp listp)
	default-asm	'()
	default-code	'()
	default-local	'()
	default-constant '()
	default-special-form '(:edit :compile)
	assume-special	t	; no locals
)))

;		P L I S T   U T I L I T I E S


(de DEF-EXPRESSION (name)
; how to compile an expression inside a function definition
  (putprop name ':compile-expression temp)
))

(de DEF-TOPLEVEL (name)
; how to evaluate an expression at toplevel in a file being compiled
  (putprop name ':compile-toplevel temp)
))

(de DEF-FILE (name)
  (putprop name ':compile-file temp)
))

(de DEF-FILE-TO-FILE (name)
  (putprop name ':compile-file-to-file temp)
))

(de DEF-OPT (name)
; how to optimize a p-machine opcode
  (putprop name ':opt temp)
))

(de DEF-PREOPT (name)
; how to pre-optimize a p-machine opcode
  (putprop name ':pre-opt temp)
))

(de DEF-LAP (name)
; how to assemble a p-machine opcode to pcode
  (putprop name ':lap temp)
))

(de DD (name form fcn)
  `(progn
     'compile
     ,(append '(de TEMP) form)
     ,(if (listp name)
	   `(mapn ,fcn (quote ,name))
	   `(funcall ,fcn (quote ,name)) )
     (unbind 'temp)
     (quote ,name) )
)))

(dm DT (l &AUX (name (second l))
		(form (rest l 2)) )
; toplevel
  (dd name form 'def-toplevel)
)))

(dm DC (l &AUX (name (second l))
		(form (rest l 2)) )
  (dd name form 'def-expression)
)))

(dm DLAP (l &AUX (name (second l))
		  (form (rest l 2)) )
  (dd name form 'def-lap)
)))

(dm DOPT (l &AUX (name (second l))
		  (form (rest l 2)) )
  (dd name form 'def-opt)
)))

(dm DPRE (l &AUX (name (second l))
		  (form (rest l 2)) )
  (dd name form 'def-preopt)
)))

(dm DCF (l &AUX (name (second l))
		 (form (rest l 2)) )
  (dd name form 'def-file)
)))

(dm DCFF (l &AUX (name (second l))
		  (form (rest l 2)) )
  (dd name form 'def-file-to-file)
)))

(df :COMPILE (l
  &AUX (obj1 (eval (first l)))
	(l (rest l))
	(a (first l))
	(l1 (if (atom a) (eval a) nil))
	(obj2 (if (stringp l1)
		  (progn (setq l (rest l))
			 l1 )
		  nil ))
	(list-sink nil)
  &SPL obj1 obj2 l ) ; unwind-protect
  (unwind-protect
    (setq l (icompile obj1 obj2 l))
    (if (and list-sink (filep (stream-fcn list-sink)))
	 (close list-sink) ) )
  l			; return the result from icompile
)))

(de ICOMPILE (obj1 obj2 flags
  &AUX (smash t)
	(target-type 'pcode)
	(local-flag nil)
	(package package)
	(macro-list nil)
	(special-list default-special)
	(local-list default-local)
	(special-form-list default-special-form)
	(local-fcn-list nil)
	(constant-list nil)
	(asm-list default-asm)
	(code-list default-code)
	(pcode-list nil)
	(inline-list default-inline) )
  (if flags
      (comp-declare (concat ':declare flags)) )
  (cond
     ( (null obj1)
	"Compiler Version 3.1 6-18-85" )
     ( (symbolp obj1)
	(compile-symbol obj1 t) )
     ( (and (stringp obj1) (null obj2))
	(compile-file obj1) )
     ( (and (stringp obj1) 
	     (stringp obj2) )
	(compile-file-to-file obj1 obj2) )
     ( t
	(el "Cannot compile " (type obj1)) ) )
)))

(de :DECOMPILE (name &AUX (prop (getprop name 'source)) )
  (if (null prop)
      (el name " not compiled")
      ; else
      (set name prop)
      (remprop name 'source)
      name )
)))

(de :CLOAD (s &AUX (s2 (string s ".p")) &SPL s s2 ) ; ** fexpr
  (compile s s2)
  (pload s2)
)))

(de :COMPILE-CLASS (c &AUX (v (c ':msg-method))
			    (len (length v)) )
  (for (i 2 len 2)
	(setq temp (vref v i))
	(compile 'temp)
	(store v i temp) )
)))

(de :ED-COMPILE (atm 
  &AUX (macro-list nil)
	(package package)
	(target-type 'pcode)
	(special-list default-special)
	(local-list default-local)
	(special-form-list default-special-form)
	(constant-list nil)
	(local-fcn-list nil)
	(asm-list default-asm)
	(pcode-list nil)
	(code-list default-code)
	(inline-list default-inline)
	(list-flag nil)
	(local-flag nil)
  &SPL atm ) ; ** catch
; for editor compile command
  (if (catch hack (compile-symbol atm) nil)
      (error ':cannot-compile '&REST) )
)))

(de :ED-COMP-ASM (atm 
  &AUX (macro-list nil)
	(package package)
	(target-type 'code)
	(special-list default-special)
	(local-list default-local)
	(special-form-list default-special-form)
	(constant-list nil)
	(local-fcn-list nil)
	(asm-list default-asm)
	(pcode-list nil)
	(code-list default-code)
	(inline-list default-inline)
	(list-flag nil)
	(local-flag t) ; default is not special
  &SPL atm ) ; ** catch
; for editor comp-asm command
  (if (catch hack (compile-symbol atm) nil)
      (error ':cannot-compile '&REST) )
)))

(de COMPILE-SYMBOL (self-name 
  &OPT (undo-flag nil)
  &AUX l )
  (if undo-flag
      (putprop self-name 'source (eval self-name))
      (remprop self-name 'source) )
  (setq l (comp self-name))
  (set self-name (apply (eval (first l)) (rest l)))
  self-name
)))

(de ASSEMBLE (l)
  (setq :comp-code-list (concat (car l) (cdr l)) :comp-code-type target-type)
  (if list-sink
      (let ( (current-sink list-sink) )
	 (pl "(" (selectq target-type 
		   (code "defcode")
		   (pcode "defpcode")
		   (asm "defasm") )
	     " " (upper (pname self-name)) " (" ")" ) ))
  (selectq target-type
    ( :asm
      (concat 'asm (:comp-asm l)) )
    ( :code
      (concat 'code (:comp-code l)) )
    ( :pcode
      (concat 'pcode (:lap l)) )
    ( t
      (error "Unknown target-type " target-type) ) )
)))

(de COMP (self-name &OPT (undo-flag nil) )
  (icomp self-name undo-flag (getfn (eval self-name)) (type (eval self-name)))
)))

(de COMP-LAMBDA (form &OPT (undo-flag nil) )
; form is (lambda (...) ...)
  (icomp ':&self undo-flag (rest form) 'expr)
)))

(de COMP-DEF (exp 
  &AUX (name (second exp)) )
; exp is (de NAME (...) ...)
  (icomp name nil (rest exp 2) (def-type exp))
)))

(de DEF-TYPE (exp)
  (selectq (first exp)
    (de 'expr)
    (df 'fexpr)
    (dm 'macro)
    (ow (error "def-type" (first exp))) )
)))

(de ICOMP (self-name undo-flag def self-type
  &AUX (target-type (cond ( (memq self-name asm-list) 'asm)
			   ( (memq self-name code-list) 'code)
			   ( (memq self-name pcode-list) 'pcode)
			   ( t target-type) ))
	(args (first def))
	(arg-list (get-args args))	; required args
	(aux-list (get-auxes args))
	(opt-list (get-opts args))
	(special-list (append (get-specs args) special-list))
	(local-list (append (get-locals args) local-list))
	(local-size (get-local-size))
	(rest-list (get-rest args))
	(loc (do-local-fcn self-name self-type)) ; dummy
	(body (rest def))
	(obj-list nil)
	(const-list nil)
	(args-code (comp-arglist args))
	(body-code (comp-exp-flush body))
	(arity-code (comp-arity self-type)) )
; compile the symbol whose value is an expr, fexpr or macro
; return the list (pcode (n...) (refs...)) or (code (n...)...)
  (assemble (opt (append+ arity-code
			   args-code
			   body-code
			   (list (mk-ret)) )))
)))

;		L O C A L   F C N S


(de SET-REQD (l n)
  (rplaca (rest l) n)
)))

(de SET-OPT (l n)
  (rplaca (rest l 2) n)
)))

(de SET-TYPE (l n)
  (rplaca (rest l 3) n)
)))

(de DO-LOCAL-FCN (name typ &AUX (loc (first (assoc name local-fcn-list))) )
  (if (null loc)
      nil
      (set-reqd loc (length arg-list))
      (set-opt loc (add (length aux-list) (length opt-list)))
      (set-type loc typ) )
)))

;			D E C L A R E

(de COMP-DECLARE (l)
; support the old way (declare &special a b c...)
; and the new way (declare (special a b c...) (asm ...))
  (el)
  (cond
    ( (null (rest l))	; old style (declare)
      (setq local-flag t) )
    ( (atom (second l))	 ; also old way
      (setq local-flag t)
      (setq special-list (append (rest l) special-list)) )
    ( t 
      (mapn comp-declare1 (rest l)) ) )
  (el)
  (el "Symbols default to " (if local-flag "LOCAL" "SPECIAL"))
  (el "Target type defaults to " (upper (pname target-type)))
  (el)
)))

(dt DECLARE (l)
  (comp-declare l)
)))

(de DO-DCL (names msg lst)
  (cond
    ( (null names)
      nil )
    ( t
      (el0 msg " -- ")
      (mapc eprin0 names)
      (terpri current-err)
      (set lst (append names (eval lst)))
      t ) )
)))

(de DO-LOCAL-DCL (name)
  (setq local-fcn-list (concat (list name 0 0 nil) local-fcn-list))
  (eprin0 name)
)))

(de COMP-DECLARE1 (l &AUX (names (rest l)) name )
; L is (special foo bar) or (local) or something
  (selectq (first l)
    (:special
      (setq local-flag (not (do-dcl names "SPECIAL" 'special-list))) )
;    (:local
;	(if names
;	    (el "LOCAL -- ") )
;	(mapn do-local-dcl names)
;	(setq local-flag (null names)) )
    (:asm
      (if (not (do-dcl names "ASM" 'asm-list))
	   (setq target-type 'asm) ) )
    (:code
      (if (not (do-dcl names "CODE" 'code-list))
	   (setq target-type 'code) ) )
    (:pcode
      (if (not (do-dcl names "PCODE" 'pcode-list))
	   (setq target-type 'pcode) ) )
;    (:inline
;	(do-dcl names "INLINE" 'inline-list) )
    (:list
      (setq name (first names)
	     list-sink (cond ( (stringp name)
			       (el "LIST " (file-name name))
			       (open name 'write) )
			     ( t
			       current-err ))) )
    (:constant
      (do-dcl names "CONSTANT" 'constant-list) )
    (:special-form
      (do-dcl names "SPECIAL-FORM" 'special-form-list) )
    (ow 
      (error "Unknown declaration" l) ) )
)))

;		 C O M P I L E	 F U N C T I O N S


(de COMP-EXP (exp &AUX comp fcn typ)
; compile an expression
  (cond
    ( (symbolp exp)  
      (list (mk-getval exp)) )

    ( (atom exp)
      (list (mk-const exp)) )

    ( (setq comp (getprop (setq fcn (first exp)) 'compile-expression)) 
      ; comp may either be a fcn or the opcode to use, i.e. '(ADD)
      (if (listp comp)
	   (append+ ; (list (mk-exp))
		    (comp-exps (rest exp)) ; args
		   (list comp) )
	   ;(concat (mk-exp); (funcall comp exp)) ;); )

    ( (or (listp fcn) ; fcn position is an expression
	   (assoc fcn local-list) ) ; fcn name is an (fget n)
      (comp-exp (concat 'funcall exp)) )

    ( (assoc fcn local-fcn-list)
      (comp-local exp) )
      
    ( (memq fcn special-form-list)
      (comp-special-form exp) )
      
    ( (not (boundp fcn))
      (comp-expr exp) ) 

    ( (memq (setq typ (type (eval fcn))) '(macro mpcode))
      (comp-exp (:expand-macro exp)) )

    ( (memq typ '(fexpr fsubr fpcode fcode))
      (comp-special-form exp) )

    ( t	; subr, lsubr, expr -- use named call
      (comp-expr exp) ) )  
)))

(de COMP-SPECIAL-FORM (exp)
  (concat+ ; (mk-exp)
	    (mk-const (rest exp))
	    (list (mk-ncall 1 (first exp))) )
)))

(de COMP-EXPR (exp &AUX (fcn (first exp))
			 (numargs (length (rest exp))) )
; symbol in fcn position, use named call
  (append+ ; (list (mk-exp))
	    (comp-exps (rest exp))	   ; args
	    (list (mk-ncall numargs fcn)) )
)))

(de COMP-LOCAL (exp &AUX (fcn (first exp))
			  (numargs (length (rest exp))) )
; local symbol in fcn position, use pcall
  (append+ ; (list (mk-exp))
	    (comp-exps (rest exp))	   ; args
	    (list (mk-pcall numargs fcn)) )
)))

(de COMP-EXPS (explist)
; compile a list of forms i.e. the body of a progn
  (cond 
    ( (null explist) nil )
    ( t
      (append (comp-exp (first explist))
	       (comp-exps (rest explist)) ) ) )
)))

(de COMP-CLAUSE (exp exitlab nextplab &AUX (pred (first exp))
					    (conseq (rest exp)) )
; exp is (pred conseq1 conseq2...)
; eval pred, jump to nextlab if false else eval conseq and jump to exitlab
  (append (comp-exp pred)
	   (concat+ (mk-iff nextplab)
		    (append (comp-exp-flush conseq)
			    (list (mk-jump exitlab)) ))
)))

"COMP.LSP loaded"

)))
