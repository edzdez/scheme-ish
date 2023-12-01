(define arr (cons 1 (cons 3 (cons 2 (cons 6 (cons 7 nil))))))

(define nil?
  (lambda (l)
    (equal? l nil)))

(define len
  (lambda (l)
    (if (nil? l)
      0
      (+ 1 (len (cdr l))))))

(define take
  (lambda (l start end)
    (define take-start
      (lambda (l n)
        (if (< n 1)
          nil
          (cons (car l) (take-start (cdr l) (- n 1))))))
    (if (equal? start 0)
      (take-start l end)
      (take (cdr l) (- start 1) (- end 1)))))

(define split
  (lambda (l)
    (define length (len l))
    (define lower (/ length 2))
    (cons (take l 0 lower) (take l lower length))))

(define merge
  (lambda (l r)
    (cond
      ((nil? l) r)
      ((nil? r) l)
      ((< (car l) (car r))
        (cons (car l) (merge (cdr l) r)))
      (else (cons (car r) (merge l (cdr r)))))))

(define mergesort
  (lambda (l)
    (if (< (len l) 2)
      l
      (begin
        (define s (split l))
        (merge (mergesort (car s)) (mergesort (cdr s)))))))


