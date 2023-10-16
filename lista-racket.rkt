#lang racket

; questao 1 - concatenar

(define (concatenar1 l1 l2)
  (if (null? l1)
      l2
      (cons (first l1) (concatenar1 (rest l1) l2))))

; questao 2 - concatenar inverso

(define (concatenarInv l1 l2)
  (if (null? l2)
      l1
      (cons (first l2) (concatenarInv l1 (rest l2)))))

; questao 3 - concatenar uma lista de listas

(define (concatenar2 l1)
  (if (null? l1)
      '()
      (concatenar1 (first l1) (concatenar2 (rest l1)))
      ))

; questao 3-v - concatenar N listas


(define (concatenar3 . ls)
  (if (null? ls)
      '()
      (concatenar1 (first ls) (concatenar2 (rest ls)))
      )
  )

; questao 4 - juntar

(define (juntar l1 l2)
  (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [else(cons (first l1) (juntar l2 (rest l1)))]
      ))

; questao 5 - adicionarFinal

(define (adicionarFinal e l)
  (if (null? l)
      (cons e '())
      (cons (first l) (adicionarFinal e (rest l)))
      )
  )

; questao 6 - inverter

(define (inverter l [a '()])
  (if (null? l)
      a
      (inverter (rest l) (cons (first l) a))))

; questao 7 - intercala e1 e e2 N vezes

(define (intercala n e1 e2)
  (cond
    [(eq? n 0) '()]
    [else(cons e1 (intercala (- n 1) e2 e1))]
    )
  )

; questao 8 - intercala N vezes e1...eM

(define (intercalaAux n l)
   (if (eq? n 0)
      '()
      (cons(first l) (intercalaAux (- n 1) (adicionarFinal (first l) (rest l))))
      )
  )

(define (intercala2 n . m)
  (if (eq? n 0)
      '()
      (cons(first m) (intercalaAux (- n 1) (adicionarFinal (first m) (rest m))))
      )
  )

; questao 9 - parear

(define (parear e l)
  (if (null? l)
      '()
      (cons (cons e (cons (first l) '())) (parear e (rest l)))
      )
)
                     

; questao 10

(define (pares l)
  (if (null? (rest l))
      '()
      (concatenar1 (parear (first l) (rest l)) (pares (rest l)))
      )
  )

; questao 11 - permutar (dificil)

; questao 12 - conjunto? (n tem elementos repetidos)

(define (conjunto? l)
  (cond
    [(null? l) #t]
    [(membro? (first l) (rest l)) #f]
    [else(conjunto? (rest l))]
    )
  )
 
(define (membro? e l)
  (cond
    [(null? l) #f]
    [(eq? e (first l)) #t]
    [else(membro? e (rest l))]
    )
  )

; questao 13 - prefixo?

(define (prefixo? l1 l2)
  (cond
    [(null? l1) #t]
    [(eq? (first l1) (first l2)) (prefixo? (rest l1) (rest l2))]
    [else #f]
    )
  )

; questao 14 - subsquencia?

(define (subsequencia? l1 l2)
  (cond
    [(null? l2) #f]
    [(eq? (first l1) (first l2))
     (if (prefixo? (rest l1) (rest l2))
         #t
         (subsequencia? l1 (rest l2)))]
    [else (subsequencia? l1 (rest l2))]
    )
  )

; questao 15 - iguais? lista generica

(define (iguais-lg? lg1 lg2)
  (cond
    [(and (null? lg1) (null? lg2)) #t]
    [(and(list? (first lg1)) (list? (first lg2)))
     (if (iguais-lg? (first lg1) (first lg2))
         (iguais-lg? (rest lg1) (rest lg2))
         #f)
     ]
    [(and (not(list? (first lg1))) (not(list? (first lg2))))
     (if (eq? (first lg1) (first lg2))
         (iguais-lg? (rest lg1) (rest lg2))
         #f)
     ]
    [else #f]
    )
  )

