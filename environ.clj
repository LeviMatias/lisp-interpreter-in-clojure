(load-file "utility.clj")
(load-file "control.clj")

(defn buscar [elem amb](
    reduce ( fn[a b](
            cond (not (nil? a))
                a 
            (not (nil? b))
                b 
            true
                nil
        )
    )
    (in-pairs amb #(
        if (= elem (first %)) (second %) nil
        )
    )
)
)



(defn actualizar-amb [amb-global clave valor]
    (cond (revisar-f valor)
        amb-global
    (amb-contains? amb-global clave)
        (apply concat (
          in-pairs amb-global #(
            if (= clave (first %)) (list (first %) valor)  % 
            ) 
          )
        )
    true
        (concat amb-global (list clave valor))
    )
)