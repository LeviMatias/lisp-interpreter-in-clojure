;
;          copyright (c) 1984, 1985, The LISP Company
;
;
;          copyright (c) 1984, 1985, The LISP Company
;
; ALOAD.LSP  -- Autoloading support

(df AUTO (l &AUX (name (first l))
		 (flag (second l))
		 (file (open name 'write))
		 (dir (open (mk-aload-dir name) 'write)))
  (unwind-protect (auto1 (rest l 2))
		   (close file) (close dir)))

(de AUTO1 (l &AUX (atm (first l))
		  (index (seek file)))
  (if (null l)
      nil
      (print (list 'setq atm (list 'aload name index)) dir)
      (iftrue (eq flag 'smash)
	      (prin3 "(setq " file)
	      (prin0 atm file)
	      (putprop atm 'aload (list name index)))
      (if (memq (type (eval atm)) '(list atom)) 
	  (prin3  \' file))
      (print (eval atm) file)
      (prin3 "))) ; " file)   ;comment 
      (print atm file)
      (set atm (aload name index))	
      (auto1 (rest l))))


(de MK-ALOAD-DIR (name &AUX (dir (string-search name \.)))
   (if dir
      (string (substring name 1 dir) "dir")
      (string name ".dir")))			

(de UNSMASH (atm &AUX (prop (getprop atm 'aload)))
   (if prop (set atm (apply aload prop))
	     (error "Cannot unsmash" atm)))
