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

(defn run-comp[f lae](
    if (f (parse-nil (first lae)) (parse-nil (second lae)) )
    't
    'nil
)) 

(defn try-number-comp [f lae](
    let [ari (controlar-aridad lae 2)]
    (if (seq? ari) ari
    (try (run-comp f lae) 
            (catch Exception e (list '*error* 'number-expected))))
))

;(defn try-number-comp [f lae](
;    if (<= (count lae) 1) (list '*error* 'too-few-args)
;    (try (f (first lae) (second lae)) 
;            (catch Exception e (list '*error* 'number-expected)))
;))
