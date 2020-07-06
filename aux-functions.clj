
;;;;;;;;utility
(defn coll-and-error?[l]
    (and (coll? l) (= (first l) '*error*))
)

(defn in-pairs[l f](
  map f (partition 2 l)
))

(defn amb-contains? [amb elem](
   reduce (fn [a b](or a b)) (in-pairs amb #(= elem (first %)))
))

;;;;;;;;;;;;;;

(defn igual? [a, b])(

)

(defn revisar-f[lis](
    cond (coll-and-error?)
        lis
    true
        

))

(defn buscar [elem amb](
    reduce (
        fn[a b](
            cond (not(nil? a))
                a 
            (not(nil? b))
                b 
            true
                nil
        )
    )
    (in-pairs amb #(
        if (= elem (first %))
            (second %)
            nil
    ))
)
)

(defn imprimir ([v](
    do
    (cond (coll-and-error? v)
        (do 
        (apply pr v)
        (newline)
        (flush)
        )
    (not= v '\space) 
        (do 
        (pr v)
        (newline)
        (flush)
        )
    )
    
    v
))([lis orig](
    cond (nil? lis)(do
        (newline)
        (flush)
    orig
    ) true (do
        (pr (first lis))
        (print \space)
        (imprimir (rest lis) orig)
    )
))
)

(defn actualizar-amb [amb-global clave valor]
    (cond (coll-and-error? valor)
        amb-global
    (amb-contains? amb-global clave)
        (apply concat (
          in-pairs amb-global #(
            if (= clave (first %)) 
              (list (first %) valor) 
              %
            ) 
          )
        )
    true
        (concat amb-global (list clave valor))
    )
)