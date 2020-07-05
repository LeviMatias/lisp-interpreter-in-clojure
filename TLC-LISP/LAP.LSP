;
;          copyright (c) 1984, 1985, The LISP Company
;
; LAP.LSP -- two pass lisp pseudo-code assembler

(setq package (pkg "comp"))

(declare)

(setq *sym-size* 16 list-sink nil)

(de :LAP (source 
  &AUX (pc 0)
	(symtab (newvec *sym-size* nil))
	(object nil)
	(obj-ptr nil)
	(ref-list nil)
  &SPL pc symtab object ref-list obj-ptr )
; input list: ( number... (opcode args...) ... label ...)
; output list: ( (number number ...) (index-number object...))
  (if list-sink
    (let ( (package sys:) (current-sink list-sink) ) ; kill package prefix on opcodes
      (prin3 "        ")
      (mapn lap-print source)
      (terpri)
      (print3 ")))")
      (terpri) ))
  (fixup source (get-args-size source))
  (mapn lap1 source)
  (setq pc 0 )
  (mapn lap2 source)
  (list object ref-list)
)))

(de LAP-PRINT (op)
  (cond
    ( (numberp op)
      (prin0 op) )
    ( (listp op)
      (terpri)
      (prin3 "        ")
      (prin0 op) )
    ( t
      (terpri)
      (prin0 op) ) )
)))

(de GET-ARGS-SIZE (l &AUX (a (first l)) )
  (add (if (memq a '(255 254)) 1 a)
	(second l)
	(third l) )
)))

(de FIXUP (l size &AUX (op (first l)) n opcode )
; convert FGET et al to reverse reference
; i.e fget0 with 3 args becomes fget2
; also convert long form (fget 0) to short form (fget0)
  (cond
    ( (null l) 
      nil )
    ( (atom op)
      (fixup (rest l) size) )
    ( (memvec (setq opcode (first op)) [:fget :fset :pc :fix :list :nc :tos :sor :pret :ptail :string ])
      (do-fixup l op opcode size)
      (fixup (rest l) size) )
    ( t
      (fixup (rest l) size) ) )
)))

(de DO-FIXUP (l op opcode size)
  (selectq opcode
    (:fget
      (setq n (sub size (second op) 1))
      (if (lt n 8)
	   (rplaca l (list (nth [:fget0 :fget1 :fget2 :fget3 :fget4 :fget5 :fget6 :fget7] 
			  (add1 n) )))
	   (rplaca l `(:fget ,n)) ) )
    (:fset
      (setq n (sub size (second op) 1))
      (if (lt n 8)
	   (rplaca l (list (nth [:fset0 :fset1 :fset2 :fset3 :fset4 :fset5 :fset6 :fset7] 
			  (add1 n) )))
	   (rplaca l `(:fset ,n)) ) )
    (:pc
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:pc0 :pc1 :pc2 :pc3 :pc4 :pc5 :pc6 :pc7] 
				(add1 n) )
			   (third op) )) ) )
    (:fix
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:fix0 :fix1 :fix2 :fix3 :fix4 :fix5 :fix6 :fix7] 
				(add1 n) ) ))
	   (rplaca l (mk-const n)) ) )
    (:list
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:list0 :list1 :list2 :list3 :list4 :list5 :list6 :list7] 
				(add1 n) ) ))
	   (rplaca l `(nc ,n list)) ) )
    (:string
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:string0 :string1 :string2 :string3 :string4 :string5 :string6 :string7] 
				(add1 n) ) ))
	   (rplaca l `(nc ,n string)) ) )
    (:nc
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:nc0 :nc1 :nc2 :nc3 :nc4 :nc5 :nc6 :nc7] 
				(add1 n) )
			   (third op) )) ) )
    (:tos
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:tos0 :tos1 :tos2 :tos3 :tos4 :tos5 :tos6 :tos7] 
			  (add1 n) ))) ) )
    (:sot
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:sot0 :sot1 :sot2 :sot3 :sot4 :sot5 :sot6 :sot7] 
			  (add1 n) ))) ) )
    (:pret
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:pret0 :pret1 :pret2 :pret3 :pret4 :pret5 :pret6 :pret7] 
				(add1 n) )
			   (third op) )) ) )
    (:ptail
      (if (lt (setq n (second op)) 8)
	   (rplaca l (list (nth [:ptail0 :ptail1 :ptail2 :ptail3 :ptail4 :ptail5 :ptail6 :ptail7] 
				(add1 n) )
			   (third op) (fourth op) (fifth op))) ) )
    ( ow
      nil ) )
)))

(de LAP1 (expr &AUX (info (getinfo expr)) )
; pass1, create symbol table
; expr is list (opcode arg(s)...) or label atom
  (selectq (optype info)
    (label
      (new-symbol expr pc) )
    (opcode
      (setq pc (add pc (oplength info))) )
    (number
      (setq pc (add1 pc)) )
    (pseudo nil)
    (ow
      (error 'unknown-opcode-type expr (first info)) ) )
)))

(de LAP2 (expr &AUX (info (getinfo expr)) opinfo)
; pass 2, generate code
  (selectq (optype info)
    (obj
      (emit-obj (second expr)) )
    (opcode
      (if (setq opinfo (opcode info))
	   (emit-byte (opcode info)) )
      (emit-args (rest expr) (opargs info)) )
    (number
      (emit-byte expr) )
    (label nil)
    (pseudo nil)
    (ow (error info)) )
)))

(de EMIT-ARGS (args types)
  (cond
    ( (and (null args) (null types)) )
    ( (neq (length args) (length types))
      (error 'opcode-arg-count expr) ) ; ** s.b. special symbol
    ( t
      (emit-arg (first args) (first types))
      (emit-args (rest args) (rest types)) ) )
)))

(de EMIT-ARG (:arg typ)
  (selectq typ
    ((byte obj-count) (emit-byte arg))
    (word (emit-word arg))
    (obj (emit-obj arg))
    (symbol (assert arg 'symbol) (emit-obj arg))
    (blabel (emit-byte (sub (sym-lookup arg) pc 1)))
    (wlabel (emit-word (sub (sym-lookup arg) pc 2)))
    (ow (error 'unknown-arg-type typ)) )
)))

(de EMIT-BYTE (byte)
 (if (or (gt byte 255)
	  (lt byte -127) )
     (error 'emit-byte 'overflow byte) )
 (if obj-ptr
     (nconc obj-ptr (setq obj-ptr (list byte)))
     (setq object (list byte)
	    obj-ptr object ))
 (setq pc (add1 pc))
)))

(de EMIT-WORD (word)
  (emit-byte (logand word #FF))
  (emit-byte (div (logand word #FF00) 256))
)))

(de EMIT-OBJ (obj)
  (selectq (type obj)
    ( (char fix)
      (emit-word (where obj)) )
    ( ow
      (setq ref-list (concat+ obj (add1 pc) ref-list))
      (emit-word 0) ) )
)))
  

;			S Y M B O L   T A B L E


(de NEW-SYMBOL (sym val 
  &AUX (index (add1 (rem (hash (pname sym)) *sym-size*)))
	(l (vref symtab index)) )
  (if (memq sym l)
      (error 'duplicate-symbol sym) )
  (store symtab index (append (list sym val) l)
)))

(de SYM-LOOKUP (sym 
  &AUX (index (add1 (rem (hash (pname sym)) *sym-size*)))
	(l (vref symtab index))
	(loc (memq sym l)) )
  (if (not loc)
      (error 'undefined-symbol sym) )
  (second loc)
)))

(de PRINT-SYMBOLS ()
  (mapvec p2 symtab)
)))

;	  P R O P E R T Y   L I S T   F U N C T I O N S


; PROPERTY LIST FORMAT:
; (type opcode length arg(s))
; type can be macro or opcode
;
; args can be any of: (byte word symbol wlabel blabel obj obj-count) or nil
;

(de GETINFO (expr &AUX info)
  (cond
    ( (listp expr)
      (setq info (getprop (first expr) 'lap))
      (if (null info)
	   (error 'unknown-opcode expr) )
      info )
    ( (numberp expr)
      '(number) )
    ( (symbolp expr)
      '(label) )
    ( t
      (error 'unknown-expression-type expr) ) )
)))

(de OPTYPE (proplist)
  (first proplist)
)))

(de OPCODE (proplist)
  (second proplist)
)))

(de OPLENGTH (proplist)
  (third proplist)
)))

(de OPARGS (proplist)
  (rest proplist 3)
)))

;	  I N I T I A L I Z A T I O N


(setq OPS (newvec 256 ':?wop?))

(df BLOCK (l &AUX (name (first l))
		   (code (second l))
		   op )
  (for (i 0 7)
	(setq op (insert (string name i) sys:))
	(putprop op 'lap (concat+ 'opcode (add code i) (rest l 2)))
	(store ops (add 1 i code) op) )
  name
)))

(df OP (l &AUX (op (first l))
		(code (second l)) )
  (putprop op 'lap (concat 'opcode (rest l)))
  (if code
    (store ops (add1 code) op) )
  op
)))

(df PSEUDO (l &AUX (op (first l)) )
  (putprop op 'lap '(pseudo))
  op
)))

; name opcode length &OPT arg-type(s)

(block "fget" 128 1)
(block "fset" 136 1)
(block "flush" 144 1)
(block "fix" 152 1)
(block "tos" 160 1)
(block "sot" 168 1)
(block "pret" 176 2 byte)
(block "nc" 184 3 symbol)
(block "string" 208 1)
(block "list" 216 1)
(block "ntail" 224 1)
(block "pc" 232 3 wlabel)
(block "ptail" 240 5 byte byte wlabel)

(op :nil 0 1)
(op :pushval 1 3 symbol)
(op :popval 2 3 symbol)
(op :exch 3 1)
(op :dupl 4 1)
(op :const 5 3 obj)
(op :flush 6 2 byte)
(op :disc 7 1)
(op :fget 8 2 byte)
(op :fset 9 2 byte)
(op :tos 10 2 byte)
(op :sot 11 2 byte)
(op :bind 12 3 symbol)
(op :rfield 13 2 byte)
(op :wfield 14 2 byte)

(op :ret 15 1)
(op :ifts 16 2 blabel)
(op :iffs 17 2 blabel)

(op :ift 21 3 wlabel)	    ; aliases
(op :iff 22 3 wlabel)
(op :iffn 20 3 wlabel)
;(op :iftn ...
(op :jump 23 3 wlabel)

(op :jmps 18 2 blabel)
(op :ifns 19 2 blabel)
(op :ifnl 20 3 wlabel)

(op :iftl 21 3 wlabel)
(op :iffl 22 3 wlabel)
(op :jmpl 23 3 wlabel)

(op :esc 24 1)
(op :asrt 25 2 byte)
(op :jtype 26 4 byte wlabel)
(op :catch 27 5 symbol wlabel)
(op :throw 28 3 symbol)
(op :uncatch 29 1)

'(op :add 30 1)
'(op :sub 31 1)
'(op :mul 32 1)
'(op :div 33 1)
(op :add1 34 1)
(op :sub1 35 1)
;(op :neg 36 1)
(op :not 37 1)

(op :gt 38 1)
(op :ge 39 1)
(op :lt 40 1)
(op :le 41 1)
(op :eq 42 1)

(op :nth 43 1)
(op :vref 44 1)
(op :vset 45 1)
(op :carcdr 46 1)
(op :car 47 1)
(op :cdr 48 1)
(op :cons 49 1)
(op :rplaca 50 1)
(op :rplacd 51 1)
(op :append 52 1)
(op :assoc 53 1)
(op :set 54 1)
(op :nconc 55 1)
(op :zerop 56 1)
(op :minusp 57 1)
(op :type 58 1)
(op :length 59 1)

(op :pushw 60 3 word)
(op :popw 61 3 word)

(op :mark 62 1)
(op :unmark 63 1)
(op :unbind 63 1)

(op :arg 64 3 symbol)

(op :nc 65 4 byte symbol)
;(op :pc 66 4 byte symbol)

(op :ntail 67 2 byte)
;(op :ptail 68 2 byte)

(op :int 69 1)
(op :unbound 70 1)
(op :dec 71 1)

(op :x 72 1)	       ; editor support
(op :y 73 1)
(op :edbuf 74 1)
(op :xo 75 1)
(op :yo 76 1)

(op :jbound 77 2 blabel)

(op :ieq 78 1)

(op :mem 79 2 obj-count)
(op :repinit 80 3 wlabel)
(op :repjmp 81 3 wlabel)

(op :xget 82 2 byte)
(op :xset 83 2 byte)
(op :funcall 84 2 byte)

(op :pret 85 3 byte byte)
(op :pc 86 4 byte wlabel)
(op :pcall 87 3 wlabel)
(op :ptail 88 6 byte byte byte wlabel)

(op :?wop? 127 1)

; PSEUDO OPS

(op :obj nil 2 obj)

(pseudo :exp)

(eval-when (load)
  (mapn unbind '(op block pseudo))
)))

"LAP.LSP loaded"

)))
