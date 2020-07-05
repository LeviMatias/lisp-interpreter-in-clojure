(vector
(edit:eof-string "Input/output commands:")
(edit:eof-string " ^KA read an atom, ^KR read a file")
(edit:eof-string " ^KF change current file")
(edit:eof-string " ^KS save current file on disk then continue to edit same file")
*eof*
(edit:eof-string "Block commands:")
(edit:eof-string " ^KB mark block beginning")
(edit:eof-string " ^KC copy block to current cursor location")
(edit:eof-string " ^KH remove block markers")
(edit:eof-string " ^KK mark block end")
(edit:eof-string " ^KV move block to cursor location")
(edit:eof-string " ^KW write the block to a file on disk, prompts for file name")
(edit:eof-string " ^KY delete block")
*eof*
(edit:eof-string "Exit commands:")
(edit:eof-string " ^KD done editing, update file on disk then prompt for new command")
(edit:eof-string " ^KQ quit editing, prompts for update file if changes occured")
(edit:eof-string " ^KX update file and exit from editor")
*eof*
(edit:eof-string "Misc commands:")
(edit:eof-string " ^KJ evaluate entire edit buffer (as if loaded from a file)")
*eof*
(edit:eof-string "Press K, Q or J for help on these commands, any other key to resume editing")
(newvector (sub edit:*ymax* 20) *eof*)
)))
		  


