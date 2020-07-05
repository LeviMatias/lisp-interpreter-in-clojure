; IBM.LSP -- support for IBM PC

(if (not (boundp ':graphics))	; may have been set if LISP @NOGR was used
    (setq :graphics t) )))

(setq :color-card (not (eq (logand (equipment) #30) #30))))

; equipment is the IBM ROM interrupt 11H routine
; 30H is the monochrome card, 10H and 20H are the color card

(ed-fast t)		 ; speed is of the essence

(de :IBM-CURSOR ()
; change to full box cursor 
  (int #10 :ah 1 :cl 15 :ch 0)
)))

(de :SCREEN-FCN ()
; used by editor when finished to restore user screen
; may be redefined by WIN.LSP below
  (console-out (string *esc* "[2J")	; clearscreen
)))

(de :SCREEN-MODE ()
  ':ts
)))

(pkg "win" sys:)

(cond
  ( (and :color-card :graphics)
    (pl "IBM Color/Graphics Monitor Adapter present -- loading turtle graphics code")
    (print (pload "win.p"))
    (print (pload "turtle.p"))
    (:tinit)
    (print (pload "demo.p"))
    (:paper 0)
    (:vp 160 90)
    (:home)
    (:pen 1)
    (:ink 1)
    (:vis 1)
    (attribute #1F) )	; for editor

  ; color card but no turtle graphics
  ( :color-card
    (attribute #1F) )	; editor

  ; black and white adapter card
  ( t
    (attribute #70) ) ) ; editor
)))	

"IBM.LSP loaded"

)))
