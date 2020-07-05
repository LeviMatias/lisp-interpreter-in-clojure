(vector
*eof*
(edit:eof-string " ^QY delete rest of line")
*eof*
(edit:eof-string " ^QR move to beginning of buffer")
(edit:eof-string " ^QC move to end of buffer")
(edit:eof-string " ^QS move to left margin")
(edit:eof-string " ^QD move to right margin")
(edit:eof-string " ^QE move to top of screen")
(edit:eof-string " ^QX move to bottom of screen")
(edit:eof-string " ^QB move to beginning of block")
(edit:eof-string " ^QK move to end of block")
*eof*
(edit:eof-string " ^QF find a string")
(edit:eof-string " ^QA replace one string for another")
*eof*
(edit:eof-string "Press K, Q or J for help on these commands, any other key to resume editing")
(newvector (sub edit:*ymax* 10) *eof*))
)))
