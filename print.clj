
(load-file "utility.clj")
(load-file "control.clj")

(defn prnt [e](
    pr (convert-nils e)
))


(defn imprimir ([v](
    do
    (cond (revisar-f v)
        (do 
        (map prnt v)
        (newline)
        (flush)
        )
    (not= v '\space) 
        (do 
        (prnt v)
        (newline)
        (flush)
        )
    ) 
    (convert-nils v)

))([lis orig](
    cond (or (nil? lis) (nil? orig))(do
        (newline)
        (flush)
        (convert-nils orig)
    ) true (do
        (prnt (first lis))
        (print \space)
        (imprimir (rest lis) orig)
        (convert-nils orig)
    )
))
)
