
(progn

(dm EOF-STRING (l &AUX (s (rest l)) )
  (list 'quote 
	 (apply string (append s (list *eof*))) )
   )


(vector
(edit:eof-string "To move the cursor:")
(edit:eof-string "  ^X down, ^S left, ^D right, ^E up, ^R previous page, ^C next page")
(edit:eof-string "  ^A word left, ^F word right")
*eof*
(edit:eof-string "To enter characters, simply type them in")
*eof*
(edit:eof-string "To delete characters:")
(edit:eof-string "  ^G delete cursor char, ^Y delete cursor line")
(edit:eof-string "  ^T delete word to right, rub or ^H delete char to left")
*eof*
(edit:eof-string "To scroll:")
(edit:eof-string "  ^W scroll down, ^Z scroll up")
*eof*
(edit:eof-string "Multiple character commands:")
(edit:eof-string "  ^K file commands, ^Q screen commands, ^J misc commands")
*eof* 
(edit:eof-string "Misc commands:")
(edit:eof-string "  esc to escape to LISP, another esc returns to editing")
(edit:eof-string "  ^L repeat last find/replace")
(edit:eof-string "  ^U abort current command")
*eof* 
(edit:eof-string "Press K, Q or J for help on these commands, any other key to resume editing")
(newvector (sub edit:*ymax* 20) *eof*)
))) )))

		  
