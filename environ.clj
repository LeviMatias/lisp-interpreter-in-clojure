(load-file "utility.clj")
(load-file "control.clj")

(defn search-pair [elem, amb-pairs](
    cond (is-nil? amb-pairs) (list '*error* 'unbound-symbol elem)
    (igual? (ffirst amb-pairs) elem) (second (first amb-pairs))
    true (search-pair elem (next amb-pairs))
)
)

(defn buscar [elem amb](
    if (= 0 (count amb)) (list '*error* 'unbound-symbol elem)
    (search-pair elem (partition 2 amb))
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