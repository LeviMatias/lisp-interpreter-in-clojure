
;;;;;;;;utility
(defn in-pairs[l f](
  map f (partition 2 l)
))

(defn amb-contains? [amb elem](
   reduce (fn [a b](or a b)) (in-pairs amb #(= elem (first %)))
))

(defn is-nil? [a](
    or (nil? a) (= a 'NIL) (= a "NIL") (= a '())
))

(defn parse-nil [e](
    if (is-nil? e) nil e
))

(defn convert-nils [l](
  cond (coll? l)
    (map (fn [elem](parse-nil elem)) l)
    true
    (parse-nil l)
))

(defn prnt [e](
    pr (convert-nils e)
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
        (list '*error* 'too-many-args)
    (< (count lis) val-esperado)
        (list '*error* 'too-few-args)
    true
        val-esperado
))

(defn aplicar-fl-lae [f lae](
    let [ari (controlar-aridad lae 1)]
    (cond (seq? ari) ari
        (igual? (first lae) nil) nil
        (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
        true (f (first lae))
    )
))

(defn revisar-f[l](
    cond (and (coll? l) (= (first l) '*error*))
        l
    true
        nil
))

(defn revisar-lae [lis](
    reduce (fn [a b] (
        cond (and (revisar-f a))
            a
        (revisar-f b)
            b
        true
            nil
    )) lis
))

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