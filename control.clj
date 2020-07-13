(load-file "utility.clj")

(defn controlar-aridad [lis val-esperado](
    cond (> (count lis) val-esperado)
        (list '*error* 'too-many-args)
    (< (count lis) val-esperado)
        (list '*error* 'too-few-args)
    true
        val-esperado
))

(defn revisar-f[l](
    cond (and (coll? l) (= (first l) '*error*))
        l
    true
        nil
))

(defn revisar-lae [lis](
    cond (= (count lis) 1) (revisar-f (first lis))
    (= (count lis) 0) 'nil
    true
    (reduce (fn [a b] (
        cond (and (revisar-f a))
            a
        (revisar-f b)
            b
        true
            nil
    )) lis )
))
