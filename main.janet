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

# ==================
# Parser
# ==================

# Parses an S-Expression into an AST
(defn parse [sexp]
  (case (type sexp)
    :string (StrC sexp)
    :symbol (IdC sexp)
    :number (NumC sexp)))

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
# Test cases
# ==================

# top-interp
(assert (= (top-interp 5) 5))
(assert (= (top-interp "test") "test"))
