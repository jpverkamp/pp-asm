#lang racket

(provide assemble)

(struct op (code name paramater? description) #:transparent)

(define ops (make-hash))

(define-syntax define-op
  (syntax-rules (:)
    [(_ code : name  description)
     (hash-set! ops 'name (op code 'name #f description))]
    [(_ code : name (param) description)
     (hash-set! ops 'name (op code 'name #t description))]))

(define-op 00 : const (c) "assembler pseudo-operator to define a constant C")
(define-op 01 : get       "read a number from the input to the accumulator")
(define-op 02 : put       "write the number in the accumulator to the output")
(define-op 03 : ld (M)    "load accumulator with contents of memory location M")
(define-op 04 : st (M)    "store contents of accumulator in memory location M")
(define-op 05 : add (M)   "add contents of memory location M to the accumulator")
(define-op 06 : sub (M)   "subtract contents of memory location M from the accumulator")
(define-op 07 : jpos (M)  "jump to memory location M if the accumulator is positive")
(define-op 08 : jz (M)    "jump to memory location M if the accumulator is zero")
(define-op 09 : j (M)     "jump to memory location M, unconditionally")
(define-op 10 : halt      "stop program execution")

(define (assemble [in (current-input-port)])
  ; Store the current parse location
  ; Each operation and each argument takes one location
  (define index  0)
  
  ; Store the location of each label as we come across it
  (define labels (make-hash))
  
  ; Parse to a list of instructions / variable locations
  (define code
    (apply
     append
     (for/list ([line-number (in-naturals 1)]
                [line (in-lines in)])
       (match line
         ; Lines are of the form: label? op-code param? comment?
         [(regexp #px"^(\\w+)?\\s*(\\w+)\\s*(\\w+)?\\s*(?:#\\s*(.*))?$" (list _ label name param comment))
          ; Store the current location as a label target
          (when label
            (when (hash-has-key? labels label)
              (error 'assemble "duplicate label detected on line ~a: ~a" line-number label))
            
            (hash-set! labels label index))
          
          ; Verify that opcode exists and that param exists or not as it should
          (define op (hash-ref ops (string->symbol name) #f))
          (when (not op)
            (error 'assemble "unknown opcode on line ~a: ~a" line-number name))
          (when (xor (op-paramater? op) param)
            (error 'assemble "extra/missing parameter on line ~a: ~a" line-number line))
          
          ; Finally, add the instruction (and maybe parameter) to the output list and recur
          ; Also, update the index for the next instruction
          (set! index (+ index 1))
          (list (op-code op)
                (cond [(eq? 'const (op-name op)) (string->number param)]
                      [param                     param]
                      [else                      0]))]
         
         ; Ignore comments / blank lines
         [(regexp #px"^\\s*(?:#\\s*(.*))?$" (list _ comment))
          (list)]))))
  
  ; Rewrite labels and vars
  (define code-with-vars
    (for/list ([each (in-list code)])
      (cond
        [(number? each) each]
        [(hash-ref labels each #f) => identity])))
  
  ; Cluster pairs
  (let loop ([ls code-with-vars])
    (if (null? ls) 
        '()
        (cons (+ (* 1000 (car ls)) (cadr ls)) (loop (cddr ls))))))