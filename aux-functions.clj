(load-file "utility.clj")
(load-file "control.clj")

(defn aplicar-fl-lae [f lae](
    let [ari (controlar-aridad lae 1)]
    (cond (seq? ari) ari
        (igual? (first lae) nil) nil
        (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
        true (f (first lae))
    )
))

