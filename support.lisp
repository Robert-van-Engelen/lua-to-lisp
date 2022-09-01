; Lua-compatible operator names (suggested), with operator names changed with lua.hpp LUA_OPERATOR_FORMAT "L%s"
(defmacro alias (to fn)
    `(setf (fdefinition ',to) #',fn))
(alias L+ +)
(alias L- -)
(alias L* *)
(alias L/ /)
(defun L// (x y) (car (truncate (/ x y))))
(alias L% mod)
(alias L^ expt)
(alias L| logior)
(alias L~ logxor)
(alias L& logand)
(alias L.. concatenate)
(alias L# length)
(alias Lor or)
(alias Land and)
(alias Lnot not)
(defun L== (x y) (if (and (stringp x) (stringp y)) (string= x y) (eq x y)))
(defun L~= (x y) (not (L== x y)))
(defun L< (x y) (if (and (stringp x) (stringp y)) (string< x y) (< x y)))
(defun L> (x y) (if (and (stringp x) (stringp y)) (string> x y) (> x y)))
(defun L<= (x y) (if (and (stringp x) (stringp y)) (string<= x y) (<= x y)))
(defun L>= (x y) (if (and (stringp x) (stringp y)) (string>= x y) (>= x y)))

; table indexing (suggested)
(defun index (field table)
    (when table
        (if (or (symbolp field) (stringp field))
            (if (string= field (car (car table)))
                (cdr (car table))
                (index field (cdr table)))
            (if (equal field (car (car table)))
                (cdr (car table))
                (index field (cdr table))))))

; assign (suggested)
(defmacro assign (lhs rhs)
    (list 'let (list (list 'evrhs (list 'mapcar (list 'quote 'eval) rhs)))
        ; TODO traverse lhs to assign evrhs list of evaluated rhs
        'evrhs
    )
)

; lookup variable, return nil when undefined (suggested), lua.hpp enable macro NAME_LOOKUP
(defmacro lookup (var)
    (list 'ignore-errors var))

; begin (suggested if begin is not a primitive, but progn is)
(defmacro begin body)
    (cons 'progn body))

; while (suggested if while is not a primitive, but loop and progn are)
(defmacro while (x . body)
    (list 'loop 'while x 'do (cons 'progn body)))

; until (suggested if while is not a primitive, but loop and progn are)
(defmacro until (x . body)
    (list 'loop 'until x 'do (cons 'progn body)))
