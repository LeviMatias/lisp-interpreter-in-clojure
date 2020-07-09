;;not needed

(defmacro de
"defines a function" 
[fname args & body]
  `(defn ~fname ~args ~@body)
)

(defmacro lambda
"defines an anonymous function"
[args & body]
    `(fn ~args ~@body)
)

(defmacro load
"loads a file"
[filepath]
    `(load-file ~filepath)
)

(defmacro or
"works as a logical or evaluating multiple conditions until true is reached or no more conditions"
[& eval]
    `(or ~eval)
)
