#lang racket

(provide simulate)

(define cmds (make-hash))

(define-syntax define-cmd
  (syntax-rules (:)
    [(_ code : (name) action ...)
     (hash-set! cmds code (λ () action ...))]
    {(_ code : (name param) action ...)
     (hash-set! cmds code (λ (param) action ...))}))

(define-cmd 00 : (const c) (void))
(define-cmd 01 : (get)     (current-acc (read)))
(define-cmd 02 : (put)     (displayln (current-acc)))
(define-cmd 03 : (ld M)    (current-acc (vector-ref (current-mem) M)))
(define-cmd 04 : (st M)    (vector-set! (current-mem) M (current-acc)))
(define-cmd 05 : (add M)   (current-acc (+ (current-acc) (vector-ref (current-mem) M))))
(define-cmd 06 : (sub M)   (current-acc (+ (current-acc) (vector-ref (current-mem) M))))
(define-cmd 07 : (jpos M)  (when (positive? (current-acc)) (current-pc M)))
(define-cmd 08 : (jz M)    (when (zero? (current-acc)) (current-pc M)))
(define-cmd 09 : (j M)     (current-pc M))
(define-cmd 10 : (halt)    (currently-running #f))
  
(define current-acc  (make-parameter (void)))
(define current-pc   (make-parameter (void)))
(define current-mem  (make-parameter (void)))
(define currently-running (make-parameter #f))
(define current-tick (make-parameter (void)))

(define (simulate-error msg)
  (error 'simulate 
         (string-append msg " on tick ~a:\nacc: ~a\nmem: ~a\n pc: ~a")
         (current-tick)
         (current-acc)
         (current-mem)
         (current-pc)))

(define (simulate code #:debug [debug #f] #:fuel [fuel +inf.0])
  (parameterize ([current-acc  0]
                 [current-mem  (list->vector code)]
                 [current-pc   0]
                 [currently-running #t]
                 [current-tick 0])
    (define memory-size (vector-length (current-mem)))
    
    (for ([tick (in-naturals 1)]
          #:break (not (currently-running)))
      (current-tick tick)
      
      ; If we're debugging, print out the current machine state
      (when debug
        (printf "~a: acc = ~a, pc = ~a, mem = ~a\n"
                (current-tick)
                (current-acc)
                (current-pc)
                (current-mem)))
      
      ; Stop execution if/when the machine runs out of fuel
      (when (> (current-tick) fuel)
        (simulate-error "out of fuel"))
      
      ; Stop execution if the pc runs out of bounds
      (when (not (<= 0 (current-pc) (- memory-size 1)))
        (simulate-error "pc out of bounds"))
      
      ; Fetch the next instruction
      (define instruction (vector-ref (current-mem) (current-pc)))
      (define function (hash-ref cmds (quotient instruction 1000) #f))
      (when (not function)
        (simulate-error "undefined opcode"))
      
      ; Fetch the argument(s) (if necessary)
      (define params
        (if (zero? (procedure-arity function))
            `()
            `(,(remainder instruction 1000))))
      
      ; Cache the current pc; if it doesn't change, we'll offset it automatically
      (define cached-pc (current-pc))
      (apply function params)
      (when (= cached-pc (current-pc))
        (current-pc (+ (current-pc) 1))))))