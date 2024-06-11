#lang rosette
(require rosette/lib/destruct)
(require rosette/lib/synthax)
(require rosette/solver/smt/z3)

(require rosette/lib/angelic)
(require racket/list)
;; (require racket/function)

(define (write-proc-helper fun-using-action)
  (lambda (str port mode)
    (let ([action (case mode
                          [(#t) write]
                          [(#f) display]
                          [else (lambda (p port) (print p port mode))])])
      (fun-using-action action str port mode))))


(struct colors-node ([prev #:mutable] self [next #:mutable]) #:transparent
  #:guard (lambda (prev self next _)
            (unless (and (or (colors-node? prev) (not prev))
                     (or (colors-node? next) (not next)))
              (error "prev and next must either be #f or a colors-node"))
            (values prev self next))
  #:methods gen:custom-write
  [(define write-proc
     (write-proc-helper
      (lambda (action cn port mode)
        (let ([colors-list (colors-node-to-list cn)])
          (write-string "(colors" port)
          (unless (empty? colors-list)
            (write-string " " port)
            (action (car colors-list) port)
            (for-each (lambda (e)
                        (write-string " " port)
                        (action e port))
                      (cdr colors-list)))
          (write-string ")" port))))
     #;(lambda (cn port mode)
       ;; (when mode (write-string "<" port))
       (let ([action (case mode
                          [(#t) write]
                          [(#f) display]
                          [else (lambda (p port) (print p port mode))])]
                ;; [helper (lambda (first-go fst nxt)
                          ;; (unless (or (and (eq? fst nxt) (not first-go))
                                      ;; (not nxt))
                ;; (action (colors-node-
             [colors-list (colors-node-to-list cn)])
         ;; (printf "colors list: ~a~n" colors-list)
         ;; (displayln colors-list)
         (write-string "(colors" port)
         (unless (empty? colors-list)
           (write-string " " port)
           (action (car colors-list) port)
           (for-each (lambda (e)
                       (write-string " " port)
                       (action e port))
                     (cdr colors-list)))
         (write-string ")" port))))])


(define (last-colors-node cn)
  (let ([nxt (colors-node-next cn)])
    (if nxt
        (last-colors-node nxt)
        cn)))

(define (reverse-colors-node colors)
  (letrec ([helper (lambda (res fst old-prev)
                     (cond
                       [(eq? old-prev colors)
                        fst]
                       [(not old-prev) res]
                       [else
                     ;; (displayln res)
                     ;; (displayln old-prev)
                         (let ([new-next (colors-node res (colors-node-self old-prev) #f)])
                           (set-colors-node-next! res new-next)
                           (helper new-next fst (colors-node-prev old-prev)))]))])
    (let* ([res (colors-node #f (colors-node-self colors) #f)]
           [result (helper res res
                        (colors-node-prev colors))]
           [res-last (last-colors-node res)])
      (begin
        ;; (displayln result)
        (set-colors-node-next! res-last res)
        (set-colors-node-prev! res res-last)
        res))))
      


(define (list-to-colors-nodes colors)
  (letrec ([helper (lambda (cs)
                     (if (empty? cs)
                         #f
                         (let ([res (helper (cdr cs))])
                           (if res
                               (let ([cur (colors-node #f (car cs) res)])
                                 (begin
                                   (set-colors-node-prev! res cur)
                                   cur))
                               (colors-node #f (car cs) #f)))))])
    (let* ([cn (helper colors)]
           [lastt (last-colors-node cn)])
      (begin
        (set-colors-node-next! lastt cn)
        (set-colors-node-prev! cn lastt)
        cn))))

(define (colors-node-to-list nodes)
  (letrec ([helper (lambda (n res)
                     (if (or (eq? n nodes)
                             (not n))
                         (reverse res)
                         (helper (colors-node-next n) (cons (colors-node-self n) res))))])
    (helper (colors-node-next nodes) (cons (colors-node-self nodes) '()))))


(displayln (list-to-colors-nodes (list "A" "B" "C" "D")))

(displayln (colors-node-to-list (list-to-colors-nodes (list "A" "C" "B" "D"))))

(displayln (reverse-colors-node (list-to-colors-nodes (list "A" "B" "C" "D"))))

                             

; represents a card weaving card
(struct card (twist builtUpTwist colors) #:transparent
  #:guard (lambda (twist builtUpTwist colors _)
            (values twist
                    builtUpTwist
                    (cond [(list? colors)
                           (list-to-colors-nodes colors)]
                          [(colors-node? colors)
                           colors]
                          [else (error "card colors should be either a list or a colors node")]))))
                            
(struct z-twist () #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (write-proc-helper
      (lambda (_ _a port _mode)
        (write-string "Z" port))))]
  ;; #:methods gen:custom-write
  ;; [(define write-proc
  ;;    (make-constructor-style-printer
  ;;     (lambda (obj) 'point)
  ;;     (lambda (obj) (list (point-x obj) (point-y obj)))))])
  )
(struct s-twist () #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (write-proc-helper
      (lambda (_ _a port _b)
        (write-string "S" port))))])

(struct Forward (card-expr) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (write-proc-helper
      (lambda (action fwd port mode)
        (action (Forward-card-expr fwd) port)
        (write-string " F" port))))])
(struct Backward (card-expr) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (write-proc-helper
      (lambda (action fwd port mode)
        (action (Backward-card-expr fwd) port)
        (write-string " B" port))))])

(struct Flip (card-expr) #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (write-proc-helper
      (lambda (action fwd port mode)
        (action (Flip-card-expr fwd) port)
        (write-string " <flip>" port))))])

(define (interpret card-expr)
  (destruct card-expr
            [(Forward ce)
             (fwd (interpret ce))]
            [(Backward ce)
             (bwd (interpret ce))]
            [(Flip ce)
             (flip (interpret ce))]
            [(card _ _ _)
             card-expr]))

(define (interpret-colors card-expr)
  (letrec ([helper
            (lambda (card-expr)
              (destruct
               card-expr
               [(Forward ce)
                (let* ([res (helper ce)]
                       [card (car res)]
                       [colors (cdr res)]
                       [new-card (fwd card)]
                       [new-colors (cons (fwd-top-color new-card) colors)])
                  (cons new-card new-colors))]
               [(Backward ce)
                (let* ([res (interpret-colors ce)]
                       [card (car res)]
                       [colors (cdr res)]
                       [new-card (bwd card)]
                       [new-colors (cons (bwd-top-color new-card) colors)])
                  (cons new-card new-colors))]
               [(card _ _ _)
                (cons card-expr (list (fwd-top-color card-expr)))]))])
    (cdr (helper card-expr))))
                       

(define (turnForward colors)
  (if (colors-node? colors)
      (colors-node-prev colors)
      (let* ([rcolors (reverse colors)]
             [lst (car rcolors)]
             [rst (reverse (cdr rcolors))])
        (cons lst rst))))

(displayln (colors-node-to-list (turnForward (list-to-colors-nodes (list "A" "B" "C" "D")))))
(displayln (turnForward (list "A" "B" "C" "D")))

(define (turnBackward colors)
  (if (colors-node? colors)
      (colors-node-next colors)
      (let* ([fst (car colors)]
             [rst (cdr colors)])
        (append rst (list fst)))))

(displayln (colors-node-to-list (turnBackward (list-to-colors-nodes (list "A" "B" "C" "D")))))
(displayln (turnBackward (list "A" "B" "C" "D")))

(define (flip-twist t)
  (destruct t
            [(z-twist)
             (s-twist)]
            [(s-twist)
             (z-twist)]))
             

(define (flip c)
  (destruct c
            [(card twist builtUpTwist colors)
             (card (flip-twist twist) (- builtUpTwist 1) (cond
                                                           [(list? colors)
                                                            (reverse colors)]
                                                           [(colors-node? colors)
                                                            (reverse-colors-node colors)]))]))
                                                           

(define (fwd c)
  (destruct c
            [(card twist builtUpTwist colors)
             (begin
               ;; (displayln colors)
               (card twist (+ builtUpTwist 1) (turnForward colors)))]))

(define (bwd c)
  (destruct c
            [(card twist builtUpTwist colors)
             (begin
               ;; (displayln colors)
               (card twist (- builtUpTwist 1) (turnBackward colors)))]))
(define (top-colors-list c)
  (destruct c
            [(card _ _ colors)
             (begin
               ;; (displayln colors)
               (cond
                 [(list? colors)                  
                  (cons (car (reverse colors))  (car colors))]
                 [(colors-node? colors)
                  (cons (colors-node-self (colors-node-prev colors))
                        (colors-node-self colors))]))]))

(define (card-expression crd n)
  (if (= n 0)
      crd
      (let ([smaller-exprs (card-expression crd (- n 1))])
        (choose* (Forward smaller-exprs) (Backward smaller-exprs)))))

;; (define-grammar (card-expression crd)
;;   [card-rule
;;    (choose ((op-rule) (card-rule))
;;            crd)]
;;   [op-rule
;;    (choose* Forward Backward)]
;;   )

(define (fwd-top-color c)
  (destruct c
            [(card _ _ colors)
             (cond
               [(list? colors)
                (car colors)]
               [(colors-node? colors)
                (colors-node-self colors)])]))

(define (bwd-top-color c)
  (destruct c
            [(card _ _ colors)
             (cond
               [(list? colors)
                (car (reverse colors))]
               [(colors-node? colors)
                (colors-node-self (colors-node-prev colors))])]))

(define (top-color card-expr)
  (destruct card-expr
            [(Forward ce)
             (fwd-top-color (interpret ce))]
            [(Backward ce)
             (bwd-top-color (interpret ce))]
            [(card _ _ _)
             (fwd-top-color card-expr)]))

(define (top-colors card-expr)
  (letrec ([helper
            (lambda (card-expr)
              (destruct card-expr
                        [(card _ _ _)
                         (cons card-expr (list))]
                        [(Forward ce)
                         (let* ([res (helper ce)]
                                [current-card (car res)]
                                [current-result (cdr res)])
                           (cons (interpret (Forward current-card))
                                 (cons (fwd-top-color current-card)
                                       current-result)))]
                        [(Backward ce)
                         (let* ([res (helper ce)]
                                [current-card (car res)]
                                [current-result (cdr res)])
                           (cons (interpret (Backward current-card))
                                 (cons (bwd-top-color current-card)
                                       current-result)))]))])
    (reverse (cdr (helper card-expr)))))
                          
                                            

(struct solution-container ([synthesized #:mutable]) #:transparent)

(define (synthesize-instruction card-state color1 color2 color3 color4)
  
  (let* ([container (solution-container #f)]
         [ref (Forward card-state)]
         [maybe-solution
          (solve
           (assert
            (let* ([synth (card-expression card-state 1)])
              (begin
                (println "Union Contents")
                (displayln (union-contents synth))
                (println "Desired Output")
                (displayln ref)
                (set-solution-container-synthesized! container synth)
                (equal? (begin
                          #;(set-solution-container-synthesized! container synth)
                          synth) ref)))))]
         [synth (solution-container-synthesized container)])
        (cond
          [(unsat? maybe-solution) (unsat)]
          [(sat? maybe-solution)
           (cons synth maybe-solution)
           #;(evaluate synth maybe-solution)])))
   ;; (assert (equal? (interpret (card-expression card-state)) (fwd card-state #;(card (s-twist) 0 (list color1 color2 color3 color4)))))
#;(assert (equal? (top-color (interpret (card-expression card-state))) color))

(define (synthesize-instruction-to-color card-state color limit)
  (let* ([card-size-range (range 1 limit)]
         [container (solution-container #f)]
         [maybe-solution
          (foldl
           (lambda (v l)
             (if (and l (not (unsat? l)))
                 l
                 (begin
                   (displayln v)
                   (solve
                    (assert
                     (let* ([synth (card-expression card-state v)])
                       (begin
                         (println "Union Contents")
                         (displayln (union-contents synth))
                         (println "Desired Output")
                         (displayln color)
                         (set-solution-container-synthesized! container synth)
                         (equal?
                          (top-color synth)
                          color))))))))
           #f
           card-size-range)])
    (cond
      [(unsat? maybe-solution) (unsat)]
      [(sat? maybe-solution)
       (cons (solution-container-synthesized container) maybe-solution)])))

(define (synthesize-instruction-to-colors-list card-state colors)
  (let* ([container (solution-container #f)]
         [maybe-solution
          (solve
           (assert
            (let* ([synth (card-expression card-state (length colors))])
              (begin
                (println "Union Contents")
                (displayln (union-contents synth))
                (println "Desired Output")
                (displayln colors)
                (set-solution-container-synthesized! container synth)
                (equal?
                 (top-colors synth)
                 colors)))))])
    (cons (solution-container-synthesized container)
          (cond
            [(unsat? maybe-solution) (unsat)]
            [(sat? maybe-solution)
             maybe-solution]))))

(define basic-card
  (card (s-twist) 0 (list "A" "B" "C" "D")))

(displayln (union-contents ((choose* Forward Backward) basic-card)))

(println "Solve")

(displayln (solve (assert (equal? ((choose* Flip Forward Backward) basic-card)
                                  (Forward basic-card)))))

(displayln (verify (assert (equal? (card-expression basic-card 1) (Forward basic-card)))))

;; (define card-solver (solve+))
;; (solver-assert (z3) (list (equal? (card-expression basic-card) (Forward basic-card))))
;; (displayln (solver-check (z3)))
;; (displayln (solver-debug (z3)))

(displayln (equal? (interpret (Forward basic-card)) (fwd basic-card)))

#;(define (synthesize-instruction2 card-state color)
  (synthesize
   #:forall (list card-state)
   #:guarantee (assert (equal? (interp(card-expression card-state))))))
(displayln (card-expression basic-card 1))
(displayln (union-contents (card-expression basic-card 1)))

(define-symbolic* a b c d integer?)

(displayln (synthesize-instruction basic-card a b c d))

(displayln (let* ([soln (synthesize-instruction-to-color basic-card "B" 3)])
             (cons (union-contents (car soln))
                   (cdr soln))))

(displayln (top-colors (Forward basic-card)))
(displayln (let* ([soln (synthesize-instruction-to-colors-list basic-card (list "D" "A"))])
             
             (cons (union-contents (car soln))
                   (cdr soln))))
                  

(top-colors-list (let ([res (bwd (fwd (fwd (card (z-twist) 0 (list "A" "B" "C" "D")))))])
              (begin
                ;; (displayln res)
                res)))

(flip (fwd (fwd (flip (fwd (fwd (card (s-twist) 0 (list "A" "B" "C" "D"))))))))




(interpret (Forward (Flip basic-card)))

(interpret (Forward (Forward basic-card)))


(displayln (Forward (Forward basic-card)))

(displayln (interpret (Backward (Backward (Backward basic-card)))))
