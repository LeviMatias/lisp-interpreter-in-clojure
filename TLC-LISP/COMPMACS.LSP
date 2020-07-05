;
;          copyright (c) 1984, 1985, The LISP Company
;

(setq package comp:)

(include "compdecl")


;	 C O D E   G E N E R A T I N G	 M A C R O S


;(dm MK-EXP (l)
;  ''(exp)
;)))

(dm MK-MEM (l &AUX (count (second l)) )
  `(list 'mem ,count)
)))

(dm MK-OBJ (l &AUX (obj (second l)) )
  `(list ':obj ,obj)
)))

(dm MK-REPINIT (l &AUX (lab (second l)) )
  `(list 'repinit ,lab)
)))

(dm MK-REPJMP (l &AUX (lab (second l)) )
  `(list 'repjmp ,lab)
)))

(dm MK-SUB1 (l)
  ''(sub1)
)))

(dm MK-ADD1 (l)
  ''(add1)
)))

(dm MK-DEC (l)
  ''(dec)
)))

(dm MK-NIL (l)
  ''(NIL)
)))

(dm MK-EQ (l)
  ''(EQ)
)))

(dm MK-IEQ (l)
  ''(IEQ)
)))

(dm MK-UNBOUND (l)
  ''(UNBOUND)
)))

(dm MK-ARG (l &AUX (name (second l)) )
  `(list 'ARG ,name)
)))

(dm MK-JUMPNIL (l &AUX (label (second l)) )
  `(list 'IFF ,label)
)))

(dm MK-IFF (l &AUX (label (second l)) )
  `(list 'IFF ,label)
)))

(dm MK-IFT (l &AUX (label (second l)) )
  `(list 'IFT ,label)
)))

(dm MK-JUMP (l &AUX (label (second l)) )
  `(list 'JUMP ,label)
)))

(setq mk-jmp mk-jump)

(dm MK-IFFN (l &AUX (label (second l)) )
  `(list 'IFFN ,label)
)))

(dm MK-JBOUND (l &AUX (label (second l)) )
  `(list ':JBOUND ,label)
)))

(dm MK-MARK (l)
  ''(MARK) 
)))

(dm MK-TOS (l &AUX (n (second l)) )
  `(list 'TOS ,n) 
)))

(dm MK-SOT (l &AUX (n (second l)) )
  `(list 'SOT ,n) 
)))

(dm MK-PCMARK (l) 
  ''(UNCATCH) 
)))

(dm MK-FCMARK (l &AUX (name (second l)) ) 
  `(list 'THROW ,name) 
)))

(dm MK-DISC (l)
  ''(DISC) 
)))

(dm MK-UNBIND (l)
  ''(UNBIND) 
)))

(dm MK-BIND (l &AUX (var (second l)) ) 
  `(list 'BIND ,var)
)))

(dm MK-NCALL (l)
  `(list 'NC ,@(rest l))
)))

(dm MK-NTAIL (l)
  `(list 'NTAIL ,@(rest l))
)))

(dm MK-FLUSH (l)
  `(list 'FLUSH ,@(rest l))
)))

(dm MK-FGET (l)
  `(list 'FGET ,@(rest l))
)))

(dm MK-FSET (l)
  `(list 'FSET ,@(rest l))
)))

(dm MK-LIST (l)
  `(list 'LIST ,@(rest l))
)))

(dm MK-FIX (l)
  `(list 'FIX ,@(rest l))
)))

(dm MK-STRING (l)
  `(list 'STRING ,@(rest l))
)))

"COMPMACS.LSP loaded"

)))
