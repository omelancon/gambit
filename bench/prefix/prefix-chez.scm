;INSERTCODE
;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
    (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
        (loop (- i 1) (run))
        result)))

(define (run-benchmark name count ok? run-maker . args)
    (newline)
    (let* ((run (apply run-maker args))
        (result (time (run-bench name count ok? run))))
    (if (not (ok? result))
        (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline)))))

(define (fatal-error . args)
    (for-each display args)
    (newline)
    (exit 1))

(define (call-with-output-file/truncate filename proc)
 (call-with-output-file filename proc))

(define (unknown r x)
    (call-with-values
        (lambda ()
            (values (vector values (lambda (x) x))
                    (if (< r 100) 0 1)))
        (lambda (v i) ((vector-ref v i) x))))

;------------------------------------------------------------------------------

; Macros...

(if-fixflo

(begin)

(begin

; Don't specialize fixnum and flonum arithmetic.

(define-syntax FLOATvector-const
(syntax-rules ()
  ((FLOATvector-const x ...) '#(x ...))))

(define-syntax FLOATvector?
(syntax-rules ()
    ((_ x)
    (vector? x))))

(define-syntax FLOATvector
(syntax-rules ()
    ((_ elem ...)
    (vector elem ...))))

(define-syntax FLOATmake-vector
(syntax-rules ()
    ((_ n init ...)
    (make-vector n init ...))))

(define-syntax FLOATvector-ref
(syntax-rules ()
    ((_ v i)
    (vector-ref v i))))

(define-syntax FLOATvector-set!
(syntax-rules ()
    ((_ v i x)
    (vector-set! v i x))))

(define-syntax FLOATvector-length
(syntax-rules ()
    ((_ v)
    (vector-length v))))

(define-syntax FLOATvector-length
(syntax-rules ()
    ((_ v)
    (vector-length v))))

(define-syntax FLOATvector-const
(syntax-rules ()
    ((_ elem ...)
    (vector elem ...))))

(define-syntax nuc-const
(syntax-rules ()
  ((FLOATnuc-const x ...) '#(x ...))))

;; float-and-generic-macros.scm
;; Define macros for floating-point and generic arithmetic operations using `define-syntax`

;; Floating-point operations
(define-syntax FLOAT+
(syntax-rules ()
    ((_ arg ...) (+ arg ...))))

(define-syntax FLOAT-
(syntax-rules ()
    ((_ arg ...) (- arg ...))))

(define-syntax FLOAT*
(syntax-rules ()
    ((_ arg ...) (* arg ...))))

(define-syntax FLOAT/
(syntax-rules ()
    ((_ arg ...) (/ arg ...))))

(define-syntax FLOAT=
(syntax-rules ()
    ((_ arg ...) (= arg ...))))

(define-syntax FLOAT<
(syntax-rules ()
    ((_ arg ...) (< arg ...))))

(define-syntax FLOAT<=
(syntax-rules ()
    ((_ arg ...) (<= arg ...))))

(define-syntax FLOAT>
(syntax-rules ()
    ((_ arg ...) (> arg ...))))

(define-syntax FLOAT>=
(syntax-rules ()
    ((_ arg ...) (>= arg ...))))

(define-syntax FLOATnegative?
(syntax-rules ()
    ((_ arg ...) (negative? arg ...))))

(define-syntax FLOATpositive?
(syntax-rules ()
    ((_ arg ...) (positive? arg ...))))

(define-syntax FLOATzero?
(syntax-rules ()
    ((_ arg ...) (zero? arg ...))))

(define-syntax FLOATabs
(syntax-rules ()
    ((_ arg ...) (abs arg ...))))

(define-syntax FLOATsin
(syntax-rules ()
    ((_ arg ...) (sin arg ...))))

(define-syntax FLOATcos
(syntax-rules ()
    ((_ arg ...) (cos arg ...))))

(define-syntax FLOATatan
(syntax-rules ()
    ((_ arg ...) (atan arg ...))))

(define-syntax FLOATsqrt
(syntax-rules ()
    ((_ arg ...) (sqrt arg ...))))

(define-syntax FLOATmin
(syntax-rules ()
    ((_ arg ...) (min arg ...))))

(define-syntax FLOATmax
(syntax-rules ()
    ((_ arg ...) (max arg ...))))

(define-syntax FLOATround
(syntax-rules ()
    ((_ arg ...) (round arg ...))))

(define-syntax FLOATinexact->exact
(syntax-rules ()
    ((_ arg ...) (inexact->exact arg ...))))

;; Generic arithmetic operations
(define-syntax GENERIC+
(syntax-rules ()
    ((_ arg ...) (+ arg ...))))

(define-syntax GENERIC-
(syntax-rules ()
    ((_ arg ...) (- arg ...))))

(define-syntax GENERIC*
(syntax-rules ()
    ((_ arg ...) (* arg ...))))

(define-syntax GENERIC/
(syntax-rules ()
    ((_ arg ...) (/ arg ...))))

(define-syntax GENERICquotient
(syntax-rules ()
    ((_ arg ...) (quotient arg ...))))

(define-syntax GENERICremainder
(syntax-rules ()
    ((_ arg ...) (remainder arg ...))))

(define-syntax GENERICmodulo
(syntax-rules ()
    ((_ arg ...) (modulo arg ...))))

(define-syntax GENERIC=
(syntax-rules ()
    ((_ arg ...) (= arg ...))))

(define-syntax GENERIC<
(syntax-rules ()
    ((_ arg ...) (< arg ...))))

(define-syntax GENERIC<=
(syntax-rules ()
    ((_ arg ...) (<= arg ...))))

(define-syntax GENERIC>
(syntax-rules ()
    ((_ arg ...) (> arg ...))))

(define-syntax GENERIC>=
(syntax-rules ()
    ((_ arg ...) (>= arg ...))))

(define-syntax GENERICexpt
(syntax-rules ()
    ((_ arg ...) (expt arg ...))))

;; Bitwise arithmetic shift
(define-syntax arithmetic-shift
(syntax-rules ()
    ((_ x y) (bitwise-arithmetic-shift x y))))

)
)

;------------------------------------------------------------------------------
