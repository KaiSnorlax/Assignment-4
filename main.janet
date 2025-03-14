# ==================
# Struct definitions
# ==================

(defn NumC [n]
  (struct :type 'NumC
          :value n))

(defn StrC [s]
  (struct :type 'StrC
          :value s))

(defn IdC [i]
  (struct :type 'IdC
          :value i))

(defn IfC [test then else]
  (struct :type 'IfC
          :test test
          :then then
          :else else))

(defn DeclareC [bindings body]
  (struct :type 'DeclareC
          :bindings bindings
          :body body))

(defn lamC [params body]
  (struct :type 'lamC
          :params params
          :body body))

(defn AppC [fun args]
  (struct :type 'AppC
          :fun fun
          :args args))

(defn Binding [name value]
  (struct :type 'Binding
          :name name
          :value value))

(defn Env [bindings]
  (struct :type 'Env
          :bindings bindings))

# Value representations
(defn NumV [n]
  (struct :type 'NumV
          :value n))

(defn BoolV [b]
  (struct :type 'BoolV
          :value b))

(defn StrV [s]
  (struct :type 'StrV
          :value s))

(defn CloV [params body env]
  (struct :type 'CloV
          :params params
          :body body
          :env env))

(defn PrimV [op proc]
  (struct :type 'PrimV
          :op op
          :proc proc))

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

(defn parse-args [args]
  (def ids (tuple)))

# Parses an S-Expression into an AST
(defn parse [sexp]
  (case (type sexp)
    :string (StrC sexp)
    :symbol (IdC sexp)
    :number (NumC sexp)
    :tuple (match sexp
             ['if c t e] (pp (IfC (parse c) (parse t) (parse e)))
             ['proc [tuple? args] body] (lamC
                                          (tuple
                                            (each arg args
                                              (case (type arg)
                                                :symbol (IdC arg)
                                                (error "arg not a symbol"))))
                                          (parse body)))))

(defn lookup [x env]
  (def bindings (get env :bindings))

  (defn lookup-r [x binds]
    (match binds
      {:type _ :name n :value v} (cond
                                   (= n x) (pp x)
                                   (error "Unbound identifier"))
      [{:type _ :name n :value v} r] (cond
                                       (= n x) (pp x)
                                       (lookup-r x r))))
  (lookup-r x bindings))


(def binds (tuple (Binding 'x 5) (Binding 'y 5)))
(def env-val (Env binds))
(assert (= (get env-val :type) 'Env))
(assert (= (get env-val :bindings) '({:name x :type Binding :value 5} {:name y :type Binding :value 5})))
(lookup 'y env-val)
(lookup 'x env-val)
(lookup 'z env-val)
# (def found (lookup 'x env-val))


# ==================
# Interpreter
# ==================

# Interprets an AST
(defn interp [exp]
  (case (get exp :type)
    'NumC (get exp :value)
    'StrC (get exp :value)
    'IdC (error "TODO")))

# Runs a QWJZ5 program
(defn top-interp [sexp]
  (interp (parse sexp)))


# ==================
# Serialization
# ==================

# Converts a runtime value to its str representation
(defn serialize [v]
  (case (get v :type)
    'NumV (string (get v :value))
    'BoolV (if (get v :value) "true" "false")
    'StrV (string "\"" (get v :value) "\"")
    'CloV "#<procedure>"
    'PrimV "#<primop>"
    (:error (string "Unknown value type: " (get v :type)))))


# ==================
# Primitive Operations
# ==================

# Creates a primitive binary numeric operations
(defn make-num-binop [op f error-msg]
  (PrimV op
         (fn [args]
           (if (= (length args) 2)
             (let [v1 (get args 0)
                   v2 (get args 1)]
               (if (and (= (get v1 :type) 'NumV)
                        (= (get v2 :type) 'NumV))
                 (let [n1 (get v1 :value)
                       n2 (get v2 :value)]
                   (if (and (= op '/) (= n2 0))
                     (error "Division by zero")
                     (NumV (f n1 n2))))
                 (error error-msg)))
             (error (string op " expects 2 arguments, got " (length args)))))))

# Creates a primitive comparison operation (like <=, >=)
(defn make-comparison [op f error-msg]
  (PrimV op
         (fn [args]
           (if (= (length args) 2)
             (let [v1 (get args 0)
                   v2 (get args 1)]
               (if (and (= (get v1 :type) 'NumV)
                        (= (get v2 :type) 'NumV))
                 (BoolV (f (get v1 :value) (get v2 :value)))
                 (error error-msg)))
             (error (string op " expects 2 arguments, got " (length args)))))))

# Primop to print a value to the console
(defn println-op [args]
  (if (= (length args) 1)
    (do
      (print (serialize (get args 0)))
      (print "\n")
      (BoolV true))
    (error "println expects 1 argument")))


# Primop to read a number from the console
(defn read-num-op [args]
  (do
    (print "> \n")
    (let [input (string/trim (file/read stdin :line))]
      (if-let [num (scan-number input)]
        (NumV num)
        (error "Not a valid real number")))))

# Primop to read a string from the console
(defn read-str-op [args]
  (do
    (print "> \n")
    (let [input (string/trim (file/read stdin :line))]
      (StrV input))))

# Primop to execute a seq of expressions and return the last value
(defn seq-op [args]
  (if (> (length args) 0)
    (get args (- (length args) 1))
    (error "need at least one expression")))


# Primop to concatenate values into a single string
(defn concat-op [args]
  (var result "")
  (each arg args
    (set result (string result (serialize arg))))
  (StrV result))


# ==================
# Test cases
# ==================

# top-interp
(assert (= (top-interp 5) 5))
(assert (= (top-interp "test") "test"))

# Test numeric literals
(assert (= (top-interp 0) 0))
(assert (= (top-interp 12) 12))
(assert (= (top-interp -80) -80))
(assert (= (top-interp 3.14) 3.14))

# Test string literals
(assert (= (top-interp "") ""))
(assert (= (top-interp "csc") "csc"))

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
(def num-val (NumV 12))
(assert (= (get num-val :type) 'NumV))
(assert (= (get num-val :value) 12))

(def bool-val (BoolV true))
(assert (= (get bool-val :type) 'BoolV))
(assert (= (get bool-val :value) true))

(def str-val (StrV "testing"))
(assert (= (get str-val :type) 'StrV))
(assert (= (get str-val :value) "testing"))

# Test the reserved-word? function
(assert (reserved-word? 'if))
(assert (reserved-word? 'proc))
(assert (reserved-word? 'declare))
(assert (reserved-word? 'in))
(assert (not (reserved-word? 'x)))
(assert (not (reserved-word? 'hello)))

# Test the distinct? function
(assert (distinct? @[1 2 3 4]))
(assert (not (distinct? @[1 2 3 1])))
(assert (distinct? @['a 'b 'c]))
(assert (not (distinct? @['a 'b 'a])))

# Test the serialize function
(assert (= (serialize (NumV 12)) "12"))
(assert (= (serialize (BoolV true)) "true"))
(assert (= (serialize (BoolV false)) "false"))
(assert (= (serialize (StrV "hello")) "\"hello\""))
