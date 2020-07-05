;
;          copyright (c) 1984, 1985, The LISP Company
;
; COMPFILE.LSP -- compiled code save to file
;
; The default for an expression in a source file is to compile it to the dest file
; If not compilable then evaluate it and copy it to the dest file 
;
; Stuff inside (eval-when (compile) ...) gets evaluated (not compiled)
; but NOT copied unless other eval-when indicators are present
;
; Stuff inside (eval-when (pload) ...) gets copied to the dest file
; but NOT evaluated unless other eval-when indicators are present
;
; Each form inside (progn 'compile ...) gets processed as if seen by
; the compiler at toplevel, thus (de...) gets compiled to the dest file
; just like (de...) at toplevel
; whereas (progn (de...)) gets copied to the dest file uncompiled
;

(setq package (pkg "comp"))

(include "compdecl")

(setq	comp-version	#0150
	*buf-size*	1000
	psave-buffer	(newstr *buf-size* \ )
)))

; PLOAD type bytes

(setq	*pl-end*	0
	*pl-pcode*	1
	*pl-code*	2
	*pl-asm*	3
	*pl-objref*	4
	*pl-endref*	5
	*pl-expr*	6
	*pl-multi-pcref* 7
	*pl-subrref*	8
	*pl-intref*	9
	*pl-set*	10
	*pl-setone*	11
	*pl-atomref*	12
	*pl-pkg*	13
	*pl-sysatom*	14
	*pl-multi-atom* 15
	*pl-multi-sysatom* 16
	*pl-multi-subrref* 17
)))

(dm PBYTE (l &AUX (byte (second l)) )
  `(writefile (ascii ,byte) current-sink)
)))

(dm PWORD (l &AUX (word (second l)) )
  `(let ( (w ,word) )
     (pbyte (logand w #FF))
     (pbyte (div (logand w #FF00) 256)) )
)))

(de COMPILE-FILE-TO-FILE (srcname destname
  &AUX (lap-echo nil)
	(old-sink current-sink)
	(current-source (open srcname 'read))
	(current-sink (open destname 'write 'random)) ; msdos hack
  &SPL old-sink destname lap-echo ) ; ** unwind-protect
  (el)
  (el "Source file " (upper (file-name srcname)))
  (el "Target file " (upper (file-name destname)))
  (unwind-protect
    (compile-save old-sink destname)
    (close current-source)
    (close current-sink) )
)))

(de COMPILE-SAVE (old-sink destname
  &AUX name
	(last-pkg t) ; not nil, gensyms have nil package
  &SPL last-pkg)
; compile the file current-source to the file current-sink
  (prin3 "AAB")       ; identifies ploadable file
  (pword comp-version)
  (do ( (exp (read) (read)) )
      ( ((eq exp 'end-of-file) (compile-save-end destname)) )
      (cff-exp exp t) )
)))

(de COMPILE-SAVE-END (destname)
  (expr-save (string (upper destname) " loaded"))
  (pbyte *pl-end*)
  (pword 0)
)))

(de CFF-EXP (exp toplevel &AUX fcn)
; exp read from source file
; do what needs to be done, dest file is current-sink
; if TOPLEVEL and not compilable then eval and copy to dest file
; if not TOPLEVEL i.e. (eval-when (compile)...) then eval but do not compile
  (cond 
    ( (atom exp) nil )
    ( (setq fcn (getprop (first exp) ':compile-file-to-file))
      (funcall fcn exp) )
    ( (and (boundp (setq fcn (first exp)))
	    (memq (type (eval fcn)) '(macro mpcode)) )
      (cff-exp (expand-macro exp) toplevel) )
    ( toplevel
      (cff-default exp) )
    ( t
      (eval exp) ) )
)))

(de CFF-DEFAULT (exp &SPL exp) ; ** catch
; no function on plist, eval and copy to dest
  (let ( (current-sink old-sink)
	  (error print-throw-error) )
	(iftrue (catch error (eval exp) nil)
		(el ^G "Expression was: " exp)
		(terpri current-err) ) )
  (expr-save exp)
)))

(dcff :INCLUDE (exp &AUX (name (second exp))
			 (current-source (open name 'read)) )
  (unwind-protect
    (do ( (exp (read) (read)) )
	 ( ((eq exp 'end-of-file) nil) )
	 (cff-exp exp t) ) ; as if in this file
    (close current-source) 
)))

(dcff (DE DF) (exp &AUX (name (second exp)) &SPL exp ) ; catch
  (eprin0 name)
  (eval exp) ; *****
  (iftrue (catch hack (psave exp) nil)
	   (prin2 "&REST" current-err)
	   (expr-save exp) )
)))

(dcff DM (exp &AUX (name (second exp)) &SPL exp )
  (eprin0 name)
  (setq macro-list (concat name macro-list))
  (if (catch hack (eval exp) nil)
      (error "macro with &REST" exp) )
  (expr-save exp)
)))

(dcff EVAL-WHEN (l
  &AUX (when (second l))
	(forms (rest l 2)) )
  (if (memq 'compile when)
      (apply progn forms) )
;	(:mapntwo cff-exp forms nil) ) ;; not copied if not compilable
  (if (memq 'pload when)
      (mapn expr-save forms) )
)))

(dcff DECLARE (l)
  (comp-declare l)
)))

(dcff REMOVE-MACROS (exp)
; generate expression to remove macros 
  (expr-save `(mapn unbind (quote ,macro-list)))
)))

(dcff QUOTE (exp)
  nil
)))

(dcff PROGN (exp)
  (if (equal (second exp) '(quote compile))
      (:mapntwo cff-exp (rest exp 2) t) ; each form treated as if at toplevel
      (cff-default exp) ) ; evaled and copied
)))

;		C O M P I L E	F I L E


(de COMPILE-FILE (name &AUX (lap-echo nil)
			     (src (open name 'read))
			&SPL src lap-echo exp ) ; ** catch, unwind-protect
  (unwind-protect
    (do ( (exp (read src) (read src)) )
	 ( ((eq exp 'end-of-file) nil) )
	 (cond
	   ( (is-def exp)
	     (prin0 (second exp))
	     (if (catch hack (compile-symbol (eval exp)) nil)
		 (prin2 "&REST") )
	     (?comp-macro exp) )
	   ( t
	     (eval exp)
	     nil ) ) )
    (close src) )
)))

(de COMPILE-FILE (name 
  &AUX (lap-echo nil)
	(src (open name 'read))
	fcn
  &SPL src lap-echo ) ; ** unwind-protect
  (unwind-protect
    (do ( (exp (read src) (read src)) )
	 ( ((eq exp 'end-of-file) nil) )
	 (cf-exp exp) )
    (close src) )
)))

(de CF-EXP (exp)
; eval if not compilable
  (cond
    ( (atom exp)
      (eval exp) )
    ( (setq fcn (getprop (first exp) ':compile-file))
      (funcall fcn exp) )
    ( t
      (eval exp) ) )
)))

(dcf :INCLUDE (exp &AUX (name (second exp))
			 (current-source (open name 'read)) )
  (unwind-protect
    (do ( (exp (read) (read)) )
	 ( ((eq exp 'end-of-file) nil) )
	 (cf-exp exp) )
    (close current-source) 
)))

(dcf (DE DF) (exp &AUX l &SPL exp l) ; catch
  (prin0 (second exp))
  (if (catch hack (setq l (comp-def exp)) nil)
      (prin2 "&REST") 
      (set (second exp) (apply (eval (first l)) (rest l))) )
)))

(dcf DM (exp &AUX (name (second exp)) )
  (prin0 name)
  (setq macro-list (concat name macro-list))
  (eval exp)
)))

(dcf DECLARE (l)
  (comp-declare l)
)))

(dcf EVAL-WHEN (l
  &AUX (when (second l))
	(forms (rest l 2)) )
;  (if (or (memq 'compile when)
;	    (memq 'pload when) )
   (if (memq 'compile when)
	(apply progn forms) )
;	(mapc cf-exp forms) )
)))

(dcf REMOVE-MACROS (exp)
  (mapn unbind macro-list)
)))

(dcf PROGN (exp)
  (if (equal (second exp) '(quote compile))
      (mapn cf-exp (rest exp 2)) ; evaled if not compilable
      (cff-default exp) )
)))

;		U T I L I T I E S


(de PSAVE (exp &AUX (ll (comp-def exp))
		     (typ (first ll))
		     (name (second exp)) )
; create PLOADable format in current-sink, arg is list returned by assemble
  (selectq typ
    (code (code-save name (rest ll)))
    (asm (asm-save name (rest ll)))
    (pcode (pcode-save name (rest ll)))
    (ow (error 'psave)) )
)))

(de PCODE-SAVE (name l
  &AUX (p (first l))
	(obj-ref-list (second l))
	(plen (length p)) )
; l is ((numbers...) (obj index...))
  (pbyte *pl-pcode*)
  (pword plen)
  (for (i 1 plen)
	(pbyte (nth p i)) )
  (mapn2 obj-ref-save obj-ref-list)
  (pbyte *pl-endref*)
  (pword 0)
  (set-one-save name)
)))

(de CODE-SAVE (name l
  &AUX (c (first l))
	(len (length c))
	(obj-ref-list (second l))
	(int-ref-list (third l))
	(code-ref-list (fourth l))
	(subr-ref-list (fifth l)) )
; l is (code-list obj-ref-list int-ref-list code-ref-list subr-ref-list)
  (pbyte *pl-code*)
  (pword len)
  (for (i 1 len)
	(pbyte (nth c i)) )
  (mapn2 obj-ref-save obj-ref-list)
  (pbyte *pl-endref*)
  (pword 0)
  (mapn2 int-ref-save int-ref-list)
  (code-ref-list-save code-ref-list)
  (mapn2 subr-ref-save subr-ref-list)
  (set-one-save name)
)))

(de ASM-SAVE (name l
  &AUX (c (first l))
	(len (length c))
	(obj-ref-list (second l))
	(int-ref-list (third l))
	(code-ref-list (fourth l))
	(subr-ref-list (fifth l)) )
; l is (code-list obj-ref-list int-ref-list code-ref-list subr-ref-list)
  (pbyte *pl-asm*)
  (pword len)
  (for (i 1 len)
	(pbyte (nth c i)) )
  (mapn2 obj-ref-save obj-ref-list)
  (pbyte *pl-endref*)
  (pword 0)
  (mapn2 int-ref-save int-ref-list)
  (code-ref-list-save code-ref-list)
  (mapn2 subr-ref-save subr-ref-list)
  (set-one-save name)
)))

(de CODE-REF-LIST-SAVE (l)
  (if (null l)
      nil
      (pbyte *pl-multi-pcref*)
      (pword (length l))
      (mapn pword l) )
)))

(de SUBR-REF-SAVE (name loc)
  (pbyte *pl-subrref*)
  (obj-save name t)	; sys: package assumed
  (pword loc)
)))

(de INT-REF-SAVE (int loc)
;  (el "INT-REF " int \ (hexword loc))
  (pbyte *pl-intref*)
  (pword loc)
  (pbyte int)
)))

(de EXPR-SAVE (exp)
  (pbyte *pl-expr*)
  (obj-save exp)
)))

(de OBJ-REF-SAVE (obj loc &AUX p)
  (cond
    ( (symbolp obj)
      (if (or (null (setq p (symbol-pkg obj)))
	       (eq p sys:) )
	   (pbyte *pl-sysatom*)
	   (?pkg-def p)
	   (pbyte *pl-atomref*) )
      (obj-save obj t)
      (pword loc) )
    ( t
      (pbyte *pl-objref*)
      (obj-save obj)
      (pword loc) ) )
)))

(de ?PKG-DEF (p)
; define a new package if necessary
  (if (eq p last-pkg)
      nil
      (setq last-pkg p)
      (pbyte *pl-pkg*)
      (obj-save p) )
)))

(de OBJ-SAVE (obj 
  &OPT (atom-flag nil)
  &AUX (package nil) ; force pkg prefix on all atoms
	(index 0)
  &SPL index ) ; obj-save-stream
; save length then printed chars of object
  (if atom-flag
      (prin3 obj obj-save-stream)
      (prin0 obj obj-save-stream) )
;  (el "OBJ-SAVE " (substr psave-buffer 1 index) " at " (seek current-sink))
  (pword index)
;  (write-block psave-buffer current-sink index)
  (write-block (substr psave-buffer 1 index) current-sink)
)))

(de OBJ-SAVE-PUTC (c)
  (if (eq index *buf-size*)
      (error "psave buffer too small, decrease size of expression" psave-buffer) )
  (string-replace-char psave-buffer (setq index (add1 index)) c)
)))

(setq obj-save-stream (stream obj-save-putc))

(de SET-ONE-SAVE (name)
  (?pkg-def (symbol-pkg name))
  (pbyte *pl-setone*)
  (obj-save name t)
)))

"COMPFILE.LSP loaded"

)))
