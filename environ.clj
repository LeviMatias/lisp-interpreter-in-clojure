(load-file "utility.clj")
(load-file "control.clj")

(defn get-result-buscar [pair, elem](
    cond (nil? pair) (list '*error* 'unbound-symbol elem)
        true (second pair) 
    )
)

(defn search-pair [elem, amb](
    reduce ( fn [a b](
            cond (not (nil? a))
                a 
            (not (nil? b))
                b 
            true
                nil
        )
    )
    (in-pairs amb #(
        if (igual? elem (first %)) (list 'match (second %)) nil
        )
    )
)
)

(defn buscar [elem amb](
    if (= 0 (count amb)) (list '*error* 'unbound-symbol elem)
    (get-result-buscar (search-pair elem amb) elem)
))



(defn actualizar-amb [amb-global clave valor]
    (cond (revisar-f valor)
        amb-global
    (amb-contains? amb-global clave)
        (apply concat (
          in-pairs amb-global #(
            if (igual? clave (first %)) (list (first %) valor)  % 
            ) 
          )
        )
    true
        (concat amb-global (list clave valor))
    )
)