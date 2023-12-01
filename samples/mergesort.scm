(define nil?
  (lambda (l)
    (equal? l nil)))

(define merge
  (lambda (l r)
    (cond
      ((nil? l) r)
      ((nil? r) l)
      ((< (car l) (car r))
        (cons (car l) (merge (cdr l) r)))
      (else (cons (car r) (merge l (cdr r)))))))


