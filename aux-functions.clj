
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

(defn is-nil? [a](
    or (nil? a) (= a 'NIL) (= a "NIL") (and (coll? a) (= (count a) '0))
))

;;;;;;;;;;;;;;

(defn igual? [a, b](
    cond (= a b)
        true
    (and (is-nil? a) (is-nil? b) )
        true
    (= (clojure.string/lower-case (str a)) (clojure.string/lower-case (str b)) )
        true
    true
        false
)
)

(defn controlar-aridad [lis val-esperado](
    cond (> (count lis) val-esperado)
        ((list '*error* 'too-many-args)
    (< (count lis) val-esperado)
        ((list '*error* 'too-few-args))
    true
        val-esperado
))

(defn revisar-f[lis](
    cond (coll-and-error? lis)
        lis
    true
        nil
))

(defn revisar-lae[lis](
    (reduce (fn [a b] (
        cond (coll-and-error? a)
            a
        (coll-and-error? b)
            b
        true
            nil
    )) lis)
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