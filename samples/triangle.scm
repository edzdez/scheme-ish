(define triangle
  (lambda (n)
    (if (equal? n 0)
        0
        (+ n (triangle (- n 1))))))

