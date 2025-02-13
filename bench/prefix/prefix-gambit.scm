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

(define (unknown x)
  (##first-argument x))

;------------------------------------------------------------------------------

; Macros...

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

(define __RESULT-BOX__ (cons #f '()))
(define (boxify-tail size . rest)
  (let* ((default (if (null? rest) #f (car rest)))
        (filler (make-list size default)))
    (set-cdr! __RESULT-BOX__ filler)
    filler))

(if-boxify
  (begin
    (def-macro (BOXIFY op . lst)
      `(begin
        (set-car! __RESULT-BOX__ (,op ,@lst))
        (car __RESULT-BOX__))))
  (begin
    (def-macro (BOXIFY op . lst) `(,op ,@lst))))

(if-fixflo

(begin

; Specialize fixnum and flonum arithmetic.

;; This code should be used when f64vectors are available.
(def-macro (FLOATvector-const . lst)   `',(list->f64vector lst))
(def-macro (FLOATvector? x)            `(BOXIFY f64vector? ,x))
(def-macro (FLOATvector . lst)         `(BOXIFY f64vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(BOXIFY make-f64vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(BOXIFY f64vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(BOXIFY f64vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(BOXIFY f64vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector
       (map (lambda (x)
              (if (vector? x)
                (list->f64vector (vector->list x))
                x))
            lst)))

;(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
;(def-macro (FLOATvector? x)            `(vector? ,x))
;(def-macro (FLOATvector . lst)         `(vector ,@lst))
;(def-macro (FLOATmake-vector n . init) `(make-vector ,n ,@init))
;(def-macro (FLOATvector-ref v i)       `(vector-ref ,v ,i))
;(def-macro (FLOATvector-set! v i x)    `(vector-set! ,v ,i ,x))
;(def-macro (FLOATvector-length v)      `(vector-length ,v))
;
;(def-macro (nuc-const . lst)
;  `',(list->vector lst))

(def-macro (FLOAT+ . lst) `(BOXIFY fl+ ,@lst))
(def-macro (FLOAT- . lst) `(BOXIFY fl- ,@lst))
(def-macro (FLOAT* . lst) `(BOXIFY fl* ,@lst))
(def-macro (FLOAT/ . lst) `(BOXIFY fl/ ,@lst))
(def-macro (FLOAT= . lst)  `(BOXIFY fl= ,@lst))
(def-macro (FLOAT< . lst)  `(BOXIFY fl< ,@lst))
(def-macro (FLOAT<= . lst) `(BOXIFY fl<= ,@lst))
(def-macro (FLOAT> . lst)  `(BOXIFY fl> ,@lst))
(def-macro (FLOAT>= . lst) `(BOXIFY fl>= ,@lst))
(def-macro (FLOATnegative? . lst) `(BOXIFY flnegative? ,@lst))
(def-macro (FLOATpositive? . lst) `(BOXIFY flpositive? ,@lst))
(def-macro (FLOATzero? . lst)     `(BOXIFY flzero? ,@lst))
(def-macro (FLOATabs . lst) `(BOXIFY flabs ,@lst))
(def-macro (FLOATsin . lst) `(BOXIFY flsin ,@lst))
(def-macro (FLOATcos . lst) `(BOXIFY flcos ,@lst))
(def-macro (FLOATatan . lst) `(BOXIFY flatan ,@lst))
(def-macro (FLOATsqrt . lst) `(BOXIFY flsqrt ,@lst))
(def-macro (FLOATmin . lst) `(BOXIFY flmin ,@lst))
(def-macro (FLOATmax . lst) `(BOXIFY flmax ,@lst))
(def-macro (FLOATround . lst) `(BOXIFY flround ,@lst))
(def-macro (FLOATinexact->exact . lst) `(BOXIFY inexact->exact ,@lst))

(define (GENERIC+ x y) (BOXIFY + x y))
(define (GENERIC- x y) (BOXIFY - x y))
(define (GENERIC* x y) (BOXIFY * x y))
(define (GENERIC/ x y) (BOXIFY / x y))
(define (GENERICquotient x y) (BOXIFY quotient x y))
(define (GENERICremainder x y) (BOXIFY remainder x y))
(define (GENERICmodulo x y) (BOXIFY modulo x y))
(define (GENERIC= x y) (BOXIFY = x y))
(define (GENERIC< x y) (BOXIFY < x y))
(define (GENERIC<= x y) (BOXIFY <= x y))
(define (GENERIC> x y) (BOXIFY > x y))
(define (GENERIC>= x y) (BOXIFY >= x y))
(define (GENERICexpt x y) (BOXIFY expt x y))

(def-macro (+ . lst) `(BOXIFY fxwrap+ ,@lst))
(def-macro (- . lst) `(BOXIFY fxwrap- ,@lst))
(def-macro (* . lst) `(BOXIFY fxwrap* ,@lst))
(def-macro (quotient . lst) `(BOXIFY fxwrapquotient ,@lst))
(def-macro (modulo . lst) `(BOXIFY fxmodulo ,@lst))
(def-macro (remainder . lst) `(BOXIFY fxremainder ,@lst))
(def-macro (= . lst)  `(BOXIFY fx= ,@lst))
(def-macro (< . lst)  `(BOXIFY fx< ,@lst))
(def-macro (<= . lst) `(BOXIFY fx<= ,@lst))
(def-macro (> . lst)  `(BOXIFY fx> ,@lst))
(def-macro (>= . lst) `(BOXIFY fx>= ,@lst))
(def-macro (negative? . lst) `(BOXIFY fxnegative? ,@lst))
(def-macro (positive? . lst) `(BOXIFY fxpositive? ,@lst))
(def-macro (zero? . lst) `(BOXIFY fxzero? ,@lst))
(def-macro (odd? . lst) `(BOXIFY fxodd? ,@lst))
(def-macro (even? . lst) `(BOXIFY fxeven? ,@lst))
(def-macro (bitwise-or . lst) `(BOXIFY fxior ,@lst))
(def-macro (bitwise-and . lst) `(BOXIFY fxand ,@lst))
(def-macro (bitwise-not . lst) `(BOXIFY fxnot ,@lst))
)

(begin

; Don't specialize fixnum and flonum arithmetic.

(def-macro (FLOATvector-const . lst)   `',(BOXIFY list->vector lst))
(def-macro (FLOATvector? x)            `(BOXIFY vector? ,x))
(def-macro (FLOATvector . lst)         `(BOXIFY vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(BOXIFY make-vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(BOXIFY vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(BOXIFY vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(BOXIFY vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector lst))

(def-macro (FLOAT+ . lst) `(BOXIFY + ,@lst))
(def-macro (FLOAT- . lst) `(BOXIFY - ,@lst))
(def-macro (FLOAT* . lst) `(BOXIFY * ,@lst))
(def-macro (FLOAT/ . lst) `(BOXIFY / ,@lst))
(def-macro (FLOAT= . lst)  `(BOXIFY = ,@lst))
(def-macro (FLOAT< . lst)  `(BOXIFY < ,@lst))
(def-macro (FLOAT<= . lst) `(BOXIFY <= ,@lst))
(def-macro (FLOAT> . lst)  `(BOXIFY > ,@lst))
(def-macro (FLOAT>= . lst) `(BOXIFY >= ,@lst))
(def-macro (FLOATnegative? . lst) `(BOXIFY negative? ,@lst))
(def-macro (FLOATpositive? . lst) `(BOXIFY positive? ,@lst))
(def-macro (FLOATzero? . lst)     `(BOXIFY zero? ,@lst))
(def-macro (FLOATabs . lst) `(BOXIFY abs ,@lst))
(def-macro (FLOATsin . lst) `(BOXIFY sin ,@lst))
(def-macro (FLOATcos . lst) `(BOXIFY cos ,@lst))
(def-macro (FLOATatan . lst) `(BOXIFY atan ,@lst))
(def-macro (FLOATsqrt . lst) `(BOXIFY sqrt ,@lst))
(def-macro (FLOATmin . lst) `(BOXIFY min ,@lst))
(def-macro (FLOATmax . lst) `(BOXIFY max ,@lst))
(def-macro (FLOATround . lst) `(BOXIFY round ,@lst))
(def-macro (FLOATinexact->exact . lst) `(BOXIFY inexact->exact ,@lst))

(def-macro (GENERIC+ . lst) `(BOXIFY + ,@lst))
(def-macro (GENERIC- . lst) `(BOXIFY - ,@lst))
(def-macro (GENERIC* . lst) `(BOXIFY * ,@lst))
(def-macro (GENERIC/ . lst) `(BOXIFY / ,@lst))
(def-macro (GENERICquotient . lst)  `(BOXIFY quotient ,@lst))
(def-macro (GENERICremainder . lst) `(BOXIFY remainder ,@lst))
(def-macro (GENERICmodulo . lst)    `(BOXIFY modulo ,@lst))
(def-macro (GENERIC= . lst)  `(BOXIFY = ,@lst))
(def-macro (GENERIC< . lst)  `(BOXIFY < ,@lst))
(def-macro (GENERIC<= . lst) `(BOXIFY <= ,@lst))
(def-macro (GENERIC> . lst)  `(BOXIFY > ,@lst))
(def-macro (GENERIC>= . lst) `(BOXIFY >= ,@lst))
(def-macro (GENERICexpt . lst) `(BOXIFY expt ,@lst))
)
)

;------------------------------------------------------------------------------
