(defn in-pairs[l f](
  map f (partition 2 l)
))

(defn amb-contains? [amb elem](
    cond (= 0 (count amb)) nil
    (= 2 (count amb)) (if (= elem (first amb)) (second amb) nil)
    true (reduce (fn [a b](or a b)) (in-pairs amb #(= elem (first %))))
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