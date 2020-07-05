;
;          copyright (c) 1984, 1985, The LISP Company
;
; OPT.LSP -- lap code optimizer

(setq package (pkg "comp")))

(declare)

(de :E1 (l &AUX (current-sink current-err) )
  (p1 l)
)))

(de OPT (ll &SPL target-list lab-list optdone )
; optimize until no more changes
  (setq :opt-source ll)
  (do ( (target-list nil nil)
	 (lab-list nil nil)
	 (optdone nil) )
      ( (optdone ll) )
      (do-opt ll) )
)))

(de DO-OPT (ll)
  (setq optdone t)	; may be reset by an optimizing routine
  (opt-analyze ll)
  ;(el "AFTER ANALYZE " ll)
  (opt-alter ll)
  ;(el "AFTER OPT " ll)
  ll 
)))

(de OPT-ANALYZE (l &OPT (prev nil)
		    &AUX fcn
			 (item (first l)) )
; L is a lap code list
; generates LAB-LIST, an assoc list of ((label (prev-code-list)) ...)
; and TARGET-LIST, a list of "reachable" labels
  ;(el0 "OA " item \ )
  (cond
    ( (null l) 
      ;(el "End")
      nil )
    ( (labelp item)
      ;(el "label")
      (setq lab-list (concat (list item prev) lab-list))
      ; don't change prev until opcode encountered ****** WHY???
      (opt-analyze (rest l) prev) )
    ( (numberp item)	; arity
      ;(el "number")
      (opt-analyze (rest l) l ;prev;) )
    ( (setq fcn (getprop (first item) ':pre-opt))
      ;(el "fcn " (who fcn))
      (funcall fcn item l prev)
      ; do l again if item was removed, else recurse on (rest l)
      (if (eq item (first l))
	   (opt-analyze (rest l) l)
	   (opt-analyze l prev) ) )
    ( t
      ;(el "punt")
      (opt-analyze (rest l) l) ) )
)))

(de OPT-ALTER (l &OPT (prev nil)
		  &AUX fcn
		       (item (first l)) )
  ;(el0 " ALTER " item)
  (cond
    ( (null l) nil )
    ( (labelp item)
      (opt-label item l prev) )
    ( (numberp item) ); arity
    ( (setq fcn (getprop (first item) ':opt))
      ;(el (who fcn))
      (funcall fcn item l prev) ) )
  (cond      ; recursive part, seperate for tail recursion
    ( (null l) nil )
    ( (eq item (first l))
      (opt-alter (rest l) l) )
    ( t
      (opt-alter l prev) ) )
)))

; PREDICATES

(dm LABELP (l &AUX (a (second l)) )
  `(symbolp ,a)
)))

(dm OPCODEP (l &AUX (a (second l)) )
  `(listp ,a)
)))

; LAP CODE SELECTORS

(de SKIP-OPCODES (l &AUX (item (first l)))
  (cond
    ( (null l)
      nil )
    ( (labelp item)
      l )
    ( t
      (skip-opcodes (rest l)) )
  )
))))

(de SKIP-LABELS (l &AUX (item (first l)))
  (cond
    ( (null l)
      (error 'trailing-labels) )
    ( (opcodep item)
      l )
    ( t
      (skip-labels (rest l)) )
  )
))))

; OPCODE RECOGNIZERS

(dm IS-JUMP (l &AUX (item (second l)) )
  `(and (opcodep ,item)
	 (eq (first ,item) 'jump) )
)))

(dm IS-RETURN (l &AUX (item (second l)) )
  `(and (opcodep ,item)
	 (eq (first ,item) 'ret) )
)))

(dm IS-FGET (l &AUX (item (second l)) )
  `(and (opcodep ,item)
	 (eq (first ,item) 'fget) )
)))

(dm IS-FSET (l &AUX (item (second l)) )
  `(and (opcodep ,item)
	 (eq (first ,item) 'fset) )
)))

(dm IS-FLUSH (l &AUX (item (second l)) )
  `(and (opcodep ,item)
	 (eq (first ,item) 'flush) )
)))

(de IS-IF (item)
  (and (opcodep item)
	(memvec (first item) [ift iff iffn :jbound]) )
)))

(de IS-REP (item)
  (and (opcodep item)
	(memvec (first item) [repinit repjmp]) )
)))

(dm IS-CATCH (l &AUX (item (second l)) )
  `(and (opcodep ,item)
	 (eq (first ,item) 'catch) )
)))

(dm IS-JTYPE (l &AUX (item (second l)) )
  `(and (opcodep ,item)
	 (eq (first ,item) 'jtype) )
)))

; OPCODE SELECTORS
;
; (NC n name)
; (RET)
; (IFT label)
; (CATCH symbol label)

(dm CALL-COUNT (l &AUX (item (second l)) )
  `(second ,item)
)))

(dm CALL-NAME (l &AUX (item (second l)) )
  `(third ,item)
)))
	 
(dm CATCH-LABEL (l &AUX (item (second l)) )
  `(third ,item) ))))

(dm JUMP-LABEL (l &AUX (item (second l)) )
  `(second ,item) )))

(dm REP-LABEL (l &AUX (item (second l)) )
  `(second ,item) )))

(dm JTYPE-LABEL (l &AUX (item (second l)) )
  `(third ,item) )))

(dm IF-LABEL (l &AUX (item (second l)) )
  `(second ,item) )))

(de OPCODE-LABEL (item)
  (cond
    ( (is-jump item)
      (jump-label item) )
    ( (is-if item)
      (if-label item) )
    ( (is-catch item)
      (catch-label item) )
    ( (is-jtype item)
      (jtype-label item) )
    ( (is-rep item)
      (rep-label item) )
    ( t
      (error 'opcode-label) ) )
)))

(dm FLUSH-COUNT (l &AUX (item (second l)) )
  `(second ,item)
)))

(dm FGET-COUNT (l &AUX (item (second l)) )
  `(second ,item)
)))

(dm FSET-COUNT (l &AUX (item (second l)) )
  `(second ,item)
)))

; LAB-LIST selectors and related, arg L is ( (label . prev-code-list) ...)

(de OPCODE-DEST (item)
  (dest-code (assoc (opcode-label item) lab-list))
)))

(de DEST-CODE (l)
  (first (dest-code-list l))
)))

(de PREV-CODE (l)
  (first (prev-code-list l))
)))

(de DEST-CODE-LIST (l)
  ;(el "DCL Skip " (car l))
  (skip-labels (rest (prev-code-list l))) 
)))

(de PREV-CODE-LIST (l)
  ;(el "PCL Skip " (car l))
  (skip-labels (second (first l))) 
)))

; OD -- reset optdone flag, an optimization was performed
(dm OD (l)
  '(setq optdone nil) )))

; SPECIFIC OPTIMIZERS FOR EACH OPCODE
;
; ITEM  is the opcode i.e. (jump label)
; L is the code-list starting at item i.e. ((jump label) (foo) label (bar) ...)
; PREV is the code-list before item i.e. ((baz) (jump label) (foo) label (bar) ...)

(de OPT-LABEL (label l prev)
  (cond
    ( (not (memq label target-list))
      ;(el "Label " label " unreachable")
      (?rplacb l (rest l))
      (od) ) )
))))

(dpre FLUSH (item l prev 
  &AUX (count (flush-count item)) )
  (cond
    ( (zerop count)
      ;(el "Zero flush count")
      (?rplacb l (rest l))
      (od) )
    ( (eq count -1)
      (rplaca l (mk-dupl))
      (od) ) )
)))

(dopt (IFT IFF IFFN) (item l prev 
  &AUX (dest (opcode-dest item)) )
;
  (cond
     ( (is-jump dest)	; replace label in if
	;(el "Unnecessary if branch to jump at " (opcode-label item))
	(replacd item (list (jump-label dest))) ; item = (IFxxx label)
	(od) ) )
)))

(dopt NC (item l prev
  &AUX (next-list (skip-labels (rest l)))
	(next-item (first next-list)) )
; wins on (jump l) ... (nc y) l (ret)
; by generating (jump l) ... ... (ntail y) l (ret)
  ;(el "OPT-CALL " item " NEXT " next-item)
  (cond
     ( (and (is-return next-item)
	     (eq self-name (call-name item)) )
	;(el "Tail recursion at " item)
	(rplaca l (mk-ntail (call-count item)))
	(od) ) )
))))

(dopt UNBIND (item l prev
  &AUX (next-list (skip-labels (rest l)))
	(next-item (first next-list)) )
; ... (unbind) (ret) -> ... (ret)
  (cond
     ( (is-return next-item)
	;(el "Unbind/ret eliminated")
	(rplaca next-list (mk-ret))
	(rplacb l (rest l))
	(od) ) )
)))

(dopt FSET (item l prev
  &AUX (next-list (rest l))
	(next-item (first next-list)) )
  (cond
     ( (and (is-fget next-item)
	     (eq (fset-count item) (fget-count next-item)) )
	; ... (fset n)(fget n) -> (dupl)(fset n)
	;(el "(fset)(fget) -> (dupl)(fset)")
	(rplaca l (mk-dupl))
	;(el "L now " l)
	(rplaca next-list (mk-fset (fset-count item)))
	;(el "Next now " next-list)
	(od) ) )
)))

(dopt FGET (item l prev
  &AUX (next-list (rest l))
	(next-item (first next-list)) )
  (cond
     ( (and (is-fget next-item)
	     (eq (fget-count item) (fget-count next-item)) )
	; ... (fget n)(fget n) -> (fget n)(dupl)
	;(el "Double fget eliminated")
	(rplaca next-list (mk-dupl))
	(od) ) )
)))

(dopt FLUSH (item l prev
  &AUX (next-list (rest l)) ; skip-labels
	(next-item (first next-list)) )
  (cond
     ( (is-return next-item)
	; ... (flush n) (ret) -> ... (ret)
	;(el "Flush/ret eliminated")
	(rplaca next-list 
		(mk-ret) )
	(rplacb l (rest l))
	(od) )
     ( (is-flush next-item)
	; ...(flush m)(flush n) -> (flush n+m)
	;(el "Flush/flush eliminated")
	(rplaca next-list 
		(mk-flush (add (flush-count item) (flush-count next-item))) )
	(rplacb l (rest l))
	(od) ) )
)))
  
(dopt JUMP (item l prev
  &AUX (dest (opcode-dest item)) )
;
  (cond
     ( (or (is-return dest)
	    (is-jump dest) )
	; replace unnecessary jump with opcode at jump destination
	;(el "Unnecessary jump to " (opcode-label item) " code " dest)
	(rplaca l dest)
	;(el "RESULT " l)
	;(el "SOURCE " :opt-source)
	(od) ) )
)))

(de PRE-OPT-TARGET (item l prev)
; record the fact that the target label is reachable
  (setq target-list (concat (opcode-label item) target-list))
)))

(de PRE-OPT-UNREACH (item l prev
  &AUX (next-item (first (rest l))) )
; code after this item cannot be executed so remove until a label is reached
  (cond
    ( (opcodep next-item)
      ;(el "Unreachable code beginning with " next-item)
      (rplacd l (skip-opcodes (rest l)))
      ;(el "RESULT " l)
      ;(el "SOURCE " :opt-source)
      (od) ) ) 
)))

(dpre (CATCH JTYPE REPINIT REPJMP IFT IFF IFFN JBOUND) (item l prev)
  (pre-opt-target item l prev)
)))

(dpre (RET THROW) (item l prev)
  (pre-opt-unreach item l prev)
)))

(dpre JUMP (item l prev)
  (pre-opt-target item l prev)
  (pre-opt-unreach item l prev) 
)))

(remove-macros)

"OPT.LSP loaded"

)))
