;------------------------------------------------------------------------------

(module prefix (main main-entry)
  (extern (include "sys/times.h")))

;INSERTCODE

(define (clock-to-msecs x)
  (quotient (* x 1000) (pragma::int "sysconf(_SC_CLK_TCK)")))

(define (process-times)
  (let ()
    (pragma "struct tms buf")
    (let ((real::long (pragma::long "times(&buf)")))
      (cons real
            (+ (pragma::long "buf.tms_utime")
               (pragma::long "buf.tms_stime"))))))

(define (time* thunk)
   (let ((start (process-times)))
     (let ((result (thunk)))
       (let ((end (process-times)))
         (let ((cpu (clock-to-msecs (- (cdr end) (cdr start))))
               (real (clock-to-msecs (- (car end) (car start)))))
           (display "cpu time: ")
           (display cpu)
           (display " real time: ")
           (display real)
           (newline)
           result)))))

(define (run-bench name count ok? run)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (time* (lambda () (run-bench name count ok? run)))))
    (if (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline))))
  (exit 0))

(define (fatal-error . args)
  (for-each display args)
  (newline)
  (exit 1))

 (define (call-with-output-file/truncate filename proc)
   (call-with-output-file filename proc))

(define (unknown x)
  ((car (list (lambda () x)))))

(define (main-entry args)
  (main))

;------------------------------------------------------------------------------

; Macros...

(define-macro (def-macro form . body)
  `(define-macro ,form (let () ,@body)))

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

(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
(def-macro (FLOATvector? x)            `(BOXIFY vector? ,x))
(def-macro (FLOATvector . lst)         `(BOXIFY vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(BOXIFY make-vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(BOXIFY vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(BOXIFY vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(BOXIFY vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector lst))

(def-macro (FLOAT+ . lst)
  (cond ((null? lst)       `0.0)
        ((null? (cdr lst)) (car lst))
        (else              `(BOXIFY +fl ,(car lst) (FLOAT+ ,@(cdr lst))))))

(def-macro (FLOAT- . lst)
  (cond ((null? (cdr lst)) `(negfl ,(car lst)))
        (else              `(BOXIFY -fl ,(car lst) (FLOAT+ ,@(cdr lst))))))

(def-macro (FLOAT* . lst)
  (cond ((null? lst)       `1.0)
        ((null? (cdr lst)) (car lst))
        (else              `(BOXIFY *fl ,(car lst) (FLOAT* ,@(cdr lst))))))

(def-macro (FLOAT/ . lst)
  (cond ((null? (cdr lst)) `(BOXIFY /fl 1.0 ,(car lst)))
        (else              `(BOXIFY /fl ,(car lst) (FLOAT* ,@(cdr lst))))))

(def-macro (FLOAT= . lst)  `(BOXIFY =fl ,@lst))
(def-macro (FLOAT< . lst)  `(BOXIFY <fl ,@lst))
(def-macro (FLOAT<= . lst) `(BOXIFY <=fl ,@lst))
(def-macro (FLOAT> . lst)  `(BOXIFY >fl ,@lst))
(def-macro (FLOAT>= . lst) `(BOXIFY >=fl ,@lst))
(def-macro (FLOATnegative? . lst) `(BOXIFY negativefl? ,@lst))
(def-macro (FLOATpositive? . lst) `(BOXIFY positivefl? ,@lst))
(def-macro (FLOATzero? . lst)     `(BOXIFY zerofl? ,@lst))
(def-macro (FLOATabs . lst) `(BOXIFY abs ,@lst))
(def-macro (FLOATsin . lst) `(BOXIFY sin ,@lst))
(def-macro (FLOATcos . lst) `(BOXIFY cos ,@lst))
(def-macro (FLOATatan . lst) `(BOXIFY atan ,@lst))
(def-macro (FLOATsqrt . lst) `(BOXIFY sqrt ,@lst))
(def-macro (FLOATmin . lst) `(BOXIFY minfl ,@lst))
(def-macro (FLOATmax . lst) `(BOXIFY maxfl ,@lst))
(def-macro (FLOATround . lst) `(BOXIFY roundfl ,@lst))
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

(def-macro (+ . lst)
  (cond ((null? lst)       `0)
        ((null? (cdr lst)) (car lst))
        (else              `(BOXIFY +fx ,(car lst) (+ ,@(cdr lst))))))

(def-macro (- . lst)
  (cond ((null? (cdr lst)) `(negfx ,(car lst)))
        (else              `(BOXIFY -fx ,(car lst) (+ ,@(cdr lst))))))

(def-macro (* . lst)
  (cond ((null? lst)       `1)
        ((null? (cdr lst)) (car lst))
        (else              `(BOXIFY *fx ,(car lst) (* ,@(cdr lst))))))

;(def-macro (quotient . lst) `(quotient ,@lst))
;(def-macro (modulo . lst) `(modulo ,@lst))
;(def-macro (remainder . lst) `(remainder ,@lst))
(def-macro (= . lst)  `(BOXIFY =fx ,@lst))
(def-macro (< . lst)  `(BOXIFY <fx ,@lst))
(def-macro (<= . lst) `(BOXIFY <=fx ,@lst))
(def-macro (> . lst)  `(BOXIFY >fx ,@lst))
(def-macro (>= . lst) `(BOXIFY >=fx ,@lst))
(def-macro (negative? . lst) `(BOXIFY negativefx? ,@lst))
(def-macro (positive? . lst) `(BOXIFY positivefx? ,@lst))
(def-macro (zero? . lst) `(BOXIFY zerofx? ,@lst))
;(def-macro (odd? . lst) `(BOXIFY odd? ,@lst))
;(def-macro (even? . lst) `(BOXIFY even? ,@lst))
(def-macro (bitwise-or . lst) `(BOXIFY bit-or ,@lst))
(def-macro (bitwise-and . lst) `(BOXIFY bit-and ,@lst))
(def-macro (bitwise-not . lst) `(BOXIFY bit-not ,@lst))
(def-macro (arithmetic-shift x y) `(BOXIFY (if (negativefx? y) (bit-rsh x (- y)) (bit-lsh x y))))
)

(begin

; Don't specialize fixnum and flonum arithmetic.

(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
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

(def-macro (bitwise-or . lst) `(BOXIFY bit-or ,@lst))
(def-macro (bitwise-and . lst) `(BOXIFY bit-and ,@lst))
(def-macro (bitwise-not . lst) `(BOXIFY bit-not ,@lst))
(def-macro (arithmetic-shift x y)
  `(let ((x ,x) (y ,y))
    (BOXIFY (if (negativefx? y) (bit-rsh x (- y)) (bit-lsh x y)))))
)
)

;------------------------------------------------------------------------------
