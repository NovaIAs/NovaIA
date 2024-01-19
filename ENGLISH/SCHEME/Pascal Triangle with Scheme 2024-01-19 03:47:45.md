(define (generate-pascal-triangle n)
  (define (pascal-row i)
    (if (= i 0) '(1))
      (map (lambda (x y) (+ x y)) (pascal-row (- i 1)) (pascal-row i))))
  (map pascal-row (range 0 n)))

(define (print-pascal-triangle n)
  (map println (generate-pascal-triangle n)))

(print-pascal-triangle 6)

; Output:
; [1]
; [1 1]
; [1 2 1]
; [1 3 3 1]
; [1 4 6 4 1]
; [1 5 10 10 5 1]