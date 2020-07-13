
(declare evaluar)

(defn evaluar-secuencia-en-cond [lis amb-global amb-local](
    (last (map (fn [elem](evaluar elem  amb-global amb-local) ) lis))
))

(defn evaluar-cond [lis amb-global amb-local](
    cond (is-nil? lis) (list nil amb-global)
    (evaluar (ffirst lis) amb-global amb-local) (evaluar-secuencia-en-cond (next (ffirst lis)) amb-global amb-local)
    true (evaluar-cond (next lis) amb-global amb-local)
))
