(load-file "utility.clj")
(load-file "control.clj")

(defn search-pair [elem, amb-pairs](
    cond (is-nil? amb-pairs) (list '*error* 'unbound-symbol elem)
    (igual? (ffirst amb-pairs) elem) (second (first amb-pairs))
    true (search-pair elem (next amb-pairs))
)
)

(defn buscar [elem amb]( 
    search-pair elem (partition 2 amb) 
))

(defn amb-contains? [elem amb](
    not (seq? (buscar elem amb))
))

(defn actualizar-amb [amb-global clave valor]
    (cond (revisar-f valor)
        amb-global
    (amb-contains? clave amb-global)
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

