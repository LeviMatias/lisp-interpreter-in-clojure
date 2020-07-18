(load-file "control.clj")
(declare evaluar)

(defn evaluar-secuencia-en-cond [lis amb-global amb-local](
   cond (<= (count lis) 1) (evaluar (first lis) amb-global amb-local)
   true (let [res (evaluar (first lis) amb-global amb-local)](
       if (revisar-f (first res)) res
       (evaluar-secuencia-en-cond (rest lis) (second res) amb-local) 
   ))
))

(defn evaluar-cond [lis amb-global amb-local](
    cond (is-nil? lis) (list nil amb-global)
    (first (evaluar (ffirst lis) amb-global amb-local)) (evaluar-secuencia-en-cond (nfirst lis) amb-global amb-local)
    true (evaluar-cond (rest lis) amb-global amb-local)
))
