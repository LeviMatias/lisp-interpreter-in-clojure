(declare evaluar)
(declare aplicar)
(declare controlar-aridad)
(declare igual?)
(declare cargar-arch)
(declare imprimir)
(declare actualizar-amb)
(declare revisar-f)
(declare revisar-lae)
(declare buscar)
(declare evaluar-cond)
(declare evaluar-secuencia-en-cond)

(declare aplicar-f-lae)

(load-file "utility.clj")
(load-file "print.clj")
(load-file "control.clj")
(load-file "environ.clj")
(load-file "aux-functions.clj")

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua.
; Si la 2da. posicion del resultado es nil, retorna true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado. 
(defn repl
   ([]
      (println "Interprete de TLC-LISP en Clojure")
	  (println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2020")
	  (println "Inspirado en:")
      (println "TLC-LISP Version 1.51 for the IBM Personal Computer")
      (println "Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
      (repl '(add add append append cond cond cons cons de de env env equal equal eval eval exit exit
 			  first first ge ge gt gt if if lambda lambda length length list list load load lt lt nil nil not not
 			  null null or or prin3 prin3 quote quote read read rest rest reverse reverse setq setq sub sub
 			  t t terpri terpri + add - sub)))
   ([amb]  
      (print ">>> ") (flush)
      (try (let [res (evaluar (read) amb nil)]
	            (if (nil? (fnext res))
				    true
					(do (imprimir (first res)) (repl (fnext res)))))
           (catch Exception e (println) (print "*error* ") (println (get (Throwable->map e) :cause)) (repl amb))))
)

; Carga el contenido de un archivo.
; Aridad 3: Recibe los ambientes global y local y el nombre de un archivo
; (literal como string o atomo, con o sin extension .lsp, o el simbolo ligado al nombre de un archivo en el ambiente), abre el archivo 
; y lee un elemento de la entrada (si falla, imprime nil), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y un arg. mas: el resultado de la evaluacion.
; Aridad 4: lee un elem. del archivo (si falla, imprime el ultimo resultado), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y el resultado de la eval.
(defn cargar-arch
  ([amb-global amb-local arch]
    (let [nomb (first (evaluar arch amb-global amb-local))]
      (if (and (seq? nomb) (igual? (first nomb) '*error*))
	    (do (imprimir nomb) amb-global) 
        (let [nm (clojure.string/lower-case (str nomb)),
              nom (if (and (> (count nm) 4)(clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
              ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                             (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
							                                        (cargar-arch (fnext res) nil in res))
	                                                           (catch Exception e (imprimir nil) amb-global))))
			  	       (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
  		     ret))))
  ([amb-global amb-local in res]
    (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (fnext res) nil in res))
         (catch Exception e (imprimir (first res)) amb-global)))
)

; Evalua una expresion usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la evaluacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'too-many-args) y el ambiente es el ambiente global.
; Si la expresion es un escalar numero o cadena, retorna la expresion y el ambiente global.
; Si la expresion es otro tipo de escalar, la busca (en los ambientes local y global) y retorna el valor y el ambiente global.
; Si la expresion es una secuencia nula, retorna nil y el ambiente global.
; Si el primer elemento de la expresion es '*error*, retorna la expresion y el ambiente global.
; Si el primer elemento de la expresion es una forma especial o una macro, valida los demas elementos y retorna el resultado y el (nuevo?) ambiente.
; Si no lo es, se trata de una funcion en posicion de operador (es una aplicacion de calculo lambda), por lo que se llama a la funcion aplicar,
; pasandole 4 argumentos: la evaluacion del primer elemento, una lista con las evaluaciones de los demas, el ambiente global y el ambiente local. 
(defn evaluar [expre amb-global amb-local]
	(if (not (seq? expre))
		(if (or (number? expre) (string? expre)) (list expre amb-global) (list (buscar expre (concat amb-local amb-global)) amb-global))
		(cond (igual? expre nil) (list nil amb-global)
			  (igual? (first expre) 't) (list true amb-global)
		      (igual? (first expre) '*error*) (list expre amb-global)
		      (igual? (first expre) 'exit) (if (< (count (next expre)) 1) (list nil nil) (list (list '*error* 'too-many-args) amb-global))
	          (igual? (first expre) 'setq) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
			                                     (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
			                                     (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
												 (= (count (next expre)) 2) (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
				                                                                 (list (first res) (actualizar-amb amb-global (fnext expre) (first res))))
												 true (let [res (evaluar (first (nnext expre)) amb-global amb-local)]
                                                           (evaluar (cons 'setq (next (nnext expre))) (actualizar-amb amb-global (fnext expre) (first res)) amb-local)))
			  (igual? (first expre) 'de) (cond (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) amb-global)
											   (and (not (igual? (first (nnext expre)) nil)) (not (seq? (first (nnext expre))))) (list (list '*error* 'list 'expected (first (nnext expre))) amb-global)
			                                   (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) amb-global)
			                                   (not (symbol? (fnext expre))) (list (list '*error* 'symbol 'expected (fnext expre)) amb-global)
											   true (list (fnext expre) (actualizar-amb amb-global (fnext expre) (cons 'lambda (nnext expre)))))
			  (igual? (first expre) 'quote) (list (if (igual? (fnext expre) nil) nil (fnext expre)) amb-global)
			  (igual? (first expre) 'lambda) (cond (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) amb-global)
											       (and (not (igual? (fnext expre) nil)) (not (seq? (fnext expre)))) (list (list '*error* 'list 'expected (fnext expre)) amb-global)
											       true (list expre amb-global))
			  (igual? (first expre) 'load)()
   			  (igual? (first expre) 'cond) (evaluar-cond (next expre) amb-global amb-local)
			  true (aplicar (first (evaluar (first expre) amb-global amb-local)) (map (fn [x] (first (evaluar x amb-global amb-local))) (next expre)) amb-global amb-local)))
)

; Aplica una funcion a una lista de argumentos evaluados, usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la aplicacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'arg-wrong-type) y el ambiente es el ambiente global.
; Aridad 4: Recibe la func., la lista de args. evaluados y los ambs. global y local. Se llama recursivamente agregando 2 args.: la func. revisada y la lista de args. revisada.
; Aridad 6: Si la funcion revisada no es nil, se la retorna con el amb. global.
; Si la lista de args. evaluados revisada no es nil, se la retorna con el amb. global.
; Si no, en caso de que la func. sea escalar (predefinida o definida por el usuario), se devuelven el resultado de su aplicacion (controlando la aridad) y el ambiente global.
; Si la func. no es escalar, se valida que la cantidad de parametros y argumentos coincidan, y:
; en caso de que se trate de una func. lambda con un solo cuerpo, se la evalua usando el amb. global intacto y el local actualizado con los params. ligados a los args.,  
; en caso de haber multiples cuerpos, se llama a aplicar recursivamente, pasando la funcion lambda sin el primer cuerpo, la lista de argumentos evaluados,
; el amb. global actualizado con la eval. del 1er. cuerpo (usando el amb. global intacto y el local actualizado con los params. ligados a los args.) y el amb. local intacto. 
(defn aplicar
   ([f lae amb-global amb-local]
      (aplicar (revisar-f f) (revisar-lae lae) f lae amb-global amb-local))
   ([resu1 resu2 f lae amb-global amb-local]
      (cond resu1 (list resu1 amb-global)
		    resu2 (list resu2 amb-global)
		    true  (if (not (seq? f))
		              (list (cond
					  		(igual? f 'terpri)(do (newline) nil)
							(igual? f 'prin3)(let [ari (controlar-aridad lae 1)]
   												(if (seq? ari) ari
												(do (prnt (first lae)) (first lae))))
  			                (igual? f 'env) (if (> (count lae) 0)
							                    (list '*error* 'too-many-args)
												(concat amb-global amb-local))
							(igual? f 'eval)(let [ari (controlar-aridad lae 1)]
   												(if (seq? ari) ari
												(try (eval (first lae)) 
												     (catch Exception e (list '*error* 'eval-failed-cant-resolve)))))

							;;logic & math
							(igual? f 'not)	(let [ari (controlar-aridad lae 2)]
   												(if (seq? ari) ari
												(not (first lae))))
							(igual? f 'add) (if (< (count lae) 2)
							                    (list '*error* 'too-few-args)
							                    (try (reduce + lae) 
												     (catch Exception e (list '*error* 'number-expected))))
							(igual? f 'sub) (if (< (count lae) 2)
							                    (list '*error* 'too-few-args)
							                    (try (reduce - lae) 
												     (catch Exception e (list '*error* 'number-expected))))
							(igual? f 'gt) (try-number-comp > lae)
							(igual? f 'ge) (try-number-comp >= lae)
							(igual? f 'lt) (try-number-comp < lae)
							(igual? f 'le) (try-number-comp <= lae)
							(igual? f 'equal) (try-number-comp igual? lae); works even if they are not numbers (special case)
							(igual? f 'null) (if (< (count lae) 1)
							                    (list '*error* 'too-few-args)
												(reduce (fn [a b](or a b)) (map is-nil? lae)))
													 
							;;list
							(igual? f 'first) (aplicar-fl-lae ffirst lae)
							(igual? f 'reverse) (aplicar-fl-lae reverse lae)
							(igual? f 'length) (aplicar-fl-lae count lae )
							(igual? f 'rest) (aplicar-fl-lae rest lae)
							(igual? f 'list) (apply list lae)
							(igual? f 'cons)(let [ari (controlar-aridad lae 2)]
   												(if (seq? ari) ari)
												(try (cons (first lae) (second lae)) 
												     (catch Exception e (list '*error* 'list-expected-2nd-arg))))
							(igual? f 'append)(let [ari (controlar-aridad lae 2)]
   												(if (seq? ari) ari)
												(try (concat (first lae) (second lae)) 
												     (catch Exception e (list '*error* 'list-expected))))

							true (let [lamb (buscar f (concat amb-local amb-global))]
								    (cond (or (number? lamb) (igual? lamb 't) (igual? lamb nil)) (list '*error* 'non-applicable-type lamb)
		                                  (or (number? f) (igual? f 't) (igual? f nil)) (list '*error* 'non-applicable-type f)
		                                  (igual? (first lamb) '*error*) lamb
	 	                                  true (aplicar lamb lae amb-global amb-local)))) amb-global)
					(cond (< (count lae) (count (fnext f))) (list (list '*error* 'too-few-args) amb-global)
					      (> (count lae) (count (fnext f))) (list (list '*error* 'too-many-args) amb-global)
					      true (if (nil? (next (nnext f)))
					               (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))
						           (aplicar (cons 'lambda (cons (fnext f) (next (nnext f)))) lae (fnext (evaluar (first (nnext f)) amb-global (concat (reduce concat (map list (fnext f) lae)) amb-local))) amb-local))))))
)

; Falta terminar de implementar las 2 funciones anteriores (aplicar y evaluar)

; Falta implementar las 9 funciones auxiliares (actualizar-amb, controlar-aridad, imprimir, buscar, etc.)

((fn [] true))