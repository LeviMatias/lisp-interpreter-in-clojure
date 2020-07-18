(require '[clojure.test :refer [is deftest run-tests]])

(load-file "utility.clj")
(load-file "print.clj")
(load-file "control.clj")
(load-file "environ.clj")
(load-file "eval.clj")
(load-file "aux-functions.clj")
(load-file "tlc-lisp.clj")

(deftest test-controlar-aridad
    (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
    (is (= 4 (controlar-aridad '(a b c d) 4)))
    (is (= '(*error* too-many-args) (controlar-aridad '(a b c d e) 4))) 
)

(deftest test-igual?
    (is (igual? nil 'NIL))
    (is (igual? nil "NIL"))
    (is (igual? nil ()))
    (is (igual? () 'NIL))
)

(deftest test-actualizar-amb
    (is (= '(+ add - sub x 1)  (actualizar-amb '(+ add - sub) 'x 1)))
    (is (= '(+ add - sub x 3 y 2) (actualizar-amb '(+ add - sub x 1 y 2) 'x 3)))
)

(deftest test-revisar-f
    (is (nil? (revisar-f 'doble)))
    (is (= '(*error* too-few-args) (revisar-f '(*error* too-few-args))))
)

(deftest test-revisar-lae
    (is (nil? (revisar-lae '(1 add first))))
    (is (= '(*error* too-many-args) (revisar-lae '(1 add (*error* too-many-args) first))))
)

(deftest test-buscar
    (is (= 'sub (buscar '- '(+ add - sub)) ))
    (is (= '(*error* unbound-symbol doble) (buscar 'doble '(+ add - sub)) ))
    (is (nil? (buscar 'valor '(valor nil))))
    (is (nil? (buscar 'nil '(nil nil))))
)

(deftest test-evaluar-secuencia-en-cond 
    (is (= '(2 (setq setq y 2)) (evaluar-secuencia-en-cond '((setq y 2)) '(setq setq) nil) ))
    (is (= '(3 (setq setq y 2 z 3)) (evaluar-secuencia-en-cond '((setq y 2) (setq z 3)) '(setq setq) nil) ))
)

(deftest test-evaluar-cond 
    (is (= '(nil (equal equal setq setq)) (evaluar-cond nil '(equal equal setq setq) nil) ))
    (is (= '(nil (equal equal first first)) (evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) ))
    (is (= '(2 (equal equal setq setq y 2)) (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil )))
    (is (= '(3 (equal equal setq setq y 2 z 3)) (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil) ))
)

(deftest test-imprimir
    (is (= '"hola" (imprimir "hola") ))
    (is (= '5 (imprimir 5)))
    (is (= 'a (imprimir 'a)))
    (is (= '\space (imprimir \space) ))
    (is (= '(hola "mundo") (imprimir '(hola "mundo")) ))
    (is (= '(*error* hola "mundo") (imprimir '(*error* hola "mundo")) ))
)

(run-tests)



