# ==================
# Struct definitions
# ==================

(defn NumC [n]
  (struct :type 'NumC
          :value n))

(defn StrC [s]
  (struct :type 'StrC
          :value s))

(defn BoolC [b]
  (struct :type 'BoolC
          :value b))

(defn IdC [i]
  (struct :type 'IdC
          :value i))

(defn IfC [test then else]
  (struct :type 'IfC
          :test test
          :then then
          :else else))

(defn ProcC [params body]
  (struct :type 'ProcC
          :params params
          :body body))

(defn ClosV [params body env]
  (struct :type 'ClosV
          :params params
          :body body
          :env env))

(defn AppC [fun args]
  (struct :type 'AppC
          :fun fun
          :args args))

(defn Binding [name value]
  (struct :type 'Binding
          :name name
          :value value))

(defn CloV [params body env]
  (struct :type 'CloV
          :params params
          :body body
          :env env))

(defn PrimV [arity proc]
  (struct :type 'PrimV
          :arity arity
          :proc proc))

# ==================
# Global Environment
# ==================

(defn assert-num [x]
  (match x
    {:type 'NumC :value n} n
    (error "Expected number")))

(def global-env [(Binding (IdC '+) (PrimV 2 (fn (x) (NumC (+ (assert-num (get x 0)) (assert-num (get x 1)))))))])

# ==================
# Parser
# ==================

# Check if a symbol is a reserved word
(defn reserved-word? [sym]
  (case sym
    'if true
    'proc true
    'declare true
    'in true
    false))

# Check if a list contains only unique elements
(defn distinct? [lst]
  (= (length lst) (length (distinct lst))))

(defn assert-id [id]
  (if (not (= (reserved-word? id))) (error "arg not a symbol"))
  (cond
    (= (symbol? id) true) (IdC id)
    (error "arg not a symbol")))

# Parses an S-Expression into an AST
(defn parse [sexp]
  (case (type sexp)
    :string (StrC sexp)
    :boolean (BoolC sexp)
    :symbol (assert-id sexp)
    :number (NumC sexp)
    :tuple (match sexp
             ['if c t e] (IfC (parse c) (parse t) (parse e))
             ['proc args body] (ProcC
                                 (tuple/join (map assert-id args))

                                 (parse body))
             ['declare binds body] (AppC (ProcC (tuple/join (map (fn (x) (assert-id (get x 0))) binds))
                                                (parse body))
                                         (tuple/join (map (fn (x) (parse (get x 1))) binds)))
             [f & a] (AppC (parse f) (tuple/join (map parse a)))
             (error "parsing error"))))


# ==================
# Serialization
# ==================

# Converts a runtime value to its str representation
(defn serialize [x]
  (match x
    {:type 'NumC :value v} (string v)
    {:type 'BoolC :value v} (if v "true" "false")
    {:type 'StrC :value v} (string "\"" v "\"")
    {:type 'CloV} "#<procedure>"
    {:type 'PrimV} "#<primop>"
    (error (string/format "Unknown value type: %p" x))))

# ==================
# Interpreter
# ==================

(defn lookup [x env]
  (defn lookup-r [x binds]
    (match binds
      [{:type _ :name {:value n} :value v} & rest] (cond
                                                     (= n x) v
                                                     (lookup-r x rest))
      (error "Unbound identifier")))
  (lookup-r x env))

(defn extend-env [params args env]
  (if (not (= (length params) (length args))) (error "Wrong number of arguments in function call"))
  (defn extend-env-r [params-rec args-rec]
    (case (length params-rec)
      0 env
      (let [params-f (get params-rec 0)
            params-r (tuple/slice params-rec 1)
            args-f (get args-rec 0)
            args-r (tuple/slice args-rec 1)] (tuple/join [(Binding params-f args-f)] (extend-env-r params-r args-r)))))
  (extend-env-r params args))

# Interprets an AST
(defn interp [exp env]
  #   (print (string/format "=======\nExp: %p\nEnv: %p\n=======" exp env))
  (case (get exp :type)
    'NumC exp
    'BoolC exp
    'StrC exp
    'IdC (lookup (get exp :value) env)
    'IfC (match (interp (get exp :test) env)
           {:type 'BoolC :value 'true} (interp (get exp :then) env)
           {:type 'BoolC :value 'false} (interp (get exp :else) env)
           (error "Expected boolean for condition"))
    'ProcC (ClosV (get exp :params) (get exp :body) env)
    'AppC (let [funval (interp (get exp :fun) env)]
            (case (get funval :type)
              'ClosV (interp (get funval :body) (extend-env (get funval :params) (tuple/join (map (fn (x) (interp x env)) (get exp :args))) (get funval :env)))
              'PrimV (if (= (length (get exp :args)) (get funval :arity)) ((get funval :proc) (tuple/join (map (fn (x) (interp x env)) (get exp :args)))) (error "Invalid arity"))
              (error (string/format "Invalid AST: %p" exp))))
    (error (string/format "Invalid AST: %p" exp))))

# Runs a QWJZ5 program
(defn top-interp [sexp]
  (serialize (interp (parse sexp) global-env)))

# ==================
# Primitive Operations
# ==================

# Creates a primitive binary numeric operations
# (defn make-num-binop [op f error-msg]
#   (PrimV op
#          (fn [args]
#            (if (= (length args) 2)
#              (let [v1 (get args 0)
#                    v2 (get args 1)]
#                (if (and (= (get v1 :type) 'NumV)
#                         (= (get v2 :type) 'NumV))
#                  (let [n1 (get v1 :value)
#                        n2 (get v2 :value)]
#                    (if (and (= op '/) (= n2 0))
#                      (error "Division by zero")
#                      (NumV (f n1 n2))))
#                  (error error-msg)))
#              (error (string op " expects 2 arguments, got " (length args)))))))

# Creates a primitive comparison operation (like <=, >=)
# (defn make-comparison [f error-msg]
#   (PrimV
#     (fn [args]
#       (if (= (length args) 2)
#         (let [v1 (get args 0)
#               v2 (get args 1)]
#           (if (and (= (get v1 :type) 'NumV)
#                    (= (get v2 :type) 'NumV))
#             (BoolV (f (get v1 :value) (get v2 :value)))
#             (error error-msg)))
#         (error (string "expects 2 arguments, got " (length args)))))))

# Primop to print a value to the console
# (defn println-op [args]
#   (if (= (length args) 1)
#     (do
#       (print (serialize (get args 0)))
#       (print "\n")
#       (BoolV true))
#     (error "println expects 1 argument")))


# # Primop to read a number from the console
# (defn read-num-op [args]
#   (do
#     (print "> \n")
#     (let [input (string/trim (file/read stdin :line))]
#       (if-let [num (scan-number input)]
#         (NumV num)
#         (error "Not a valid real number")))))

# # Primop to read a string from the console
# (defn read-str-op [args]
#   (do
#     (print "> \n")
#     (let [input (string/trim (file/read stdin :line))]
#       (StrV input))))

# # Primop to execute a seq of expressions and return the last value
# (defn seq-op [args]
#   (if (> (length args) 0)
#     (get args (- (length args) 1))
#     (error "need at least one expression")))


# # Primop to concatenate values into a single string
# (defn concat-op [args]
#   (var result "")
#   (each arg args
#     (set result (string result (serialize arg))))
#   (StrV result))


# ==================
# Test cases
# ==================

# Test proc
(assert (= (top-interp '((proc (x y) (+ x y)) 3 5)) "8"))
(assert (= (top-interp '(((proc (x) (proc (y) (+ x y))) 6) 3)) "9"))

# Test declare
(assert (= (top-interp '(declare ((x 10) (y 7)) (+ x y))) "17"))

# Test numeric literals
(assert (= (top-interp 0) "0"))
(assert (= (top-interp 12) "12"))
(assert (= (top-interp -80) "-80"))
(assert (= (top-interp 3.14) "3.14"))

# Test string literals
(assert (= (top-interp "") "\"\""))
(assert (= (top-interp "csc") "\"csc\""))

# Test if
(assert (= (top-interp '(if true "good" "bad")) "\"good\""))
(assert (= (top-interp '(if false "good" "bad")) "\"bad\""))

# Test AST node strut
(def num-ast (parse 123))
(assert (= (get num-ast :type) 'NumC))
(assert (= (get num-ast :value) 123))

(def str-ast (parse "hello"))
(assert (= (get str-ast :type) 'StrC))
(assert (= (get str-ast :value) "hello"))

(def id-ast (parse 'x))
(assert (= (get id-ast :type) 'IdC))
(assert (= (get id-ast :value) 'x))

# Test reserved words parsing
(def if-ast (parse 'if))
(assert (= (get if-ast :type) 'IdC))
(assert (= (get if-ast :value) 'if))

# Test runtime value structures
(def num-val (NumC 12))
(assert (= (get num-val :type) 'NumC))
(assert (= (get num-val :value) 12))

(def bool-val (BoolC true))
(assert (= (get bool-val :type) 'BoolC))
(assert (= (get bool-val :value) true))

(def str-val (StrC "testing"))
(assert (= (get str-val :type) 'StrC))
(assert (= (get str-val :value) "testing"))

# Test the reserved-word? function
(assert (reserved-word? 'if))
(assert (reserved-word? 'proc))
(assert (reserved-word? 'declare))
(assert (reserved-word? 'in))
(assert (not (reserved-word? 'x)))
(assert (not (reserved-word? 'hello)))

# Test the distinct? function
(assert (distinct? [1 2 3 4]))
(assert (not (distinct? [1 2 3 1])))
(assert (distinct? ['a 'b 'c]))
(assert (not (distinct? ['a 'b 'a])))

# Test the serialize function
(assert (= (serialize (NumC 12)) "12"))
(assert (= (serialize (BoolC true)) "true"))
(assert (= (serialize (BoolC false)) "false"))
(assert (= (serialize (StrC "hello")) "\"hello\""))

# Test the lookup function
(let [env (tuple (Binding (IdC 'x) 1) (Binding (IdC 'y) 2) (Binding (IdC 'z) 3))]
  (assert (= (lookup 'y env) 2))
  (assert (= (lookup 'x env) 1))
  (assert (= (lookup 'z env) 3)))
