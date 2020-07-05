(vector
*eof*
(edit:eof-string " ^JH help")
(edit:eof-string " ^JJ evaluate expression from cursor location")
(edit:eof-string " ^JC evaluate then compile definition at cursor location")
(edit:eof-string " ^JP parse from cursor, finds right paren when cursor on left paren")
*eof*
(edit:eof-string "Press K, Q or J for help on these commands, any other key to resume editing")
(newvector edit:*ymax* *eof*)
)))
