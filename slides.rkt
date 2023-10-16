#lang racket


(define (atom? x)
  (not (list? x)))

(define (non-atom? a)
  (if (atom? a)
      #f
      #t))

(define (lat? l)
  (cond
    [(null? l) #t]
    [(non-atom? (first l)) #f]
    [else (lat? (rest l))]
    ))

(define (soma-numeros l)
(if (null? l)
    0
    (+ (first l) (soma-numeros (rest l)))))


(define (multN l)
  (if (null? l)
      1
      (* (first l) (multN (rest l)))))

(define (rmembro a l)
  (cond
    [(null? l) '()]
    [(eq? a (first l)) (rest l)]
    [else (cons (first l)
                (rmembro a (rest l)))]
))


(define (substitui a n l)
  (cond
    [(null? l) '()]
    [(eq? a (first l))
     (cons n (substitui a n (rest l))
           )]
    [else (cons (first l)
                (substitui a n (rest l)))]
    )
  )

(define (substitui-2* a n l)
  (cond
    [(null? l) '()]
    [(list? (first l))
     (cons
      (substitui-2* a n (first l))
      (substitui-2* a n (rest l)))]
    [(eq? a (first l))
     (cons n (substitui-2* a n (rest l)))]
    [else (cons (first l)
                (substitui-2* a n (rest l)))]
    ))
  
(define (inverte l)
  (inverte-AC l '() ))
(define (inverte-AC l a)
  (if (null? l)
      a
      (inverte-AC (rest l) (cons (first l) a))
      ))
