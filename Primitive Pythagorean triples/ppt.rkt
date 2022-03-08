#lang racket

(provide (all-defined-out))

;; Un triplet pitagoreic primitiv (TPP) este format din 
;; 3 numere naturale nenule a, b, c cu proprietățile:
;;    a^2 + b^2 = c^2
;;    a, b, c prime între ele
;;
;; TPP pot fi generate sub formă de arbore (infinit) cu
;; rădăcina (3,4,5), pe baza a 3 transformări matriciale:
;;
;;      |-1 2 2|        |1 2 2|        |1 -2 2|
;; T1 = |-2 1 2|   T2 = |2 1 2|   T3 = |2 -1 2|
;;      |-2 2 3|        |2 2 3|        |2 -2 3|
;;
;;                         (3,4,5)
;;              ______________|______________
;;             |              |              |
;;         (15,8,17)      (21,20,29)     (5,12,13)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;; (35,12,37) ..........................................
;;
;; unde:
;; (15, 8,17) = T1·(3,4,5)
;; (21,20,29) = T2·(3,4,5)
;; ( 5,12,13) = T3·(3,4,5) etc.
;;
;; În această reprezentare, TPP sunt indexate "de sus în jos",
;; respectiv "de la stânga la dreapta", rezultând ordinea:
;; (3,4,5) (15,8,17) (21,20,29) (5,12,13) (35,12,37) ... etc.

;; Reprezentăm matricile T1, T2, T3 ca liste de liste:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Implementați o funcție care calculează produsul scalar
; a doi vectori X și Y (reprezentați ca liste).
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
; Utilizați recursivitate pe stivă.

; Returneaza produsul primelor elemente din fiecare lista inmultit cu apelarea
; recursiva pe stiva a celor doua liste fara primul element (cdr)
(define (dot-product X Y)
  (if (= (length X) 0) 0 (+ (* (car X) (car Y)) (dot-product (cdr X) (cdr Y)))))


; TODO
; Implementați o funcție care calculează produsul dintre
; o matrice M și un vector V (puneți V "pe verticală").
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
; Utilizați recursivitate pe coadă.

; Apeleaza functia get-matrix-multiplication-iter care returneaza raspunsul
(define (multiply M V)
  (get-matrix-multiplication-iter M V (list)))

; Ia fiecare rand din matrice si il inmulteste cu Vector folosind functia definita precedent
; salvand raspunsul in lista 'list-ans' ce va fi returnata in final
(define (get-matrix-multiplication-iter Matrix Vector list-ans)
  (if (= (length Vector) (length list-ans))
      list-ans
      (get-matrix-multiplication-iter (cdr Matrix) Vector (append list-ans (list (dot-product (car  Matrix) Vector))))))


; TODO
; Implementați o funcție care primește un număr n și
; întoarce o listă numerică (unde elementele au valoarea
; 1, 2 sau 3), reprezentând secvența de transformări prin
; care se obține, plecând de la (3,4,5), al n-lea TPP
; din arbore.
; Ex: (get-transformations 8) întoarce '(2 1), adică
; al 8-lea TPP din arbore se obține din T1·T2·(3,4,5).
; Sunteți încurajați să folosiți funcții ajutătoare
; (de exemplu pentru determinarea nivelului din arbore 
; pe care se află n, sau a indexului minim/maxim de pe 
; nivelul respectiv, etc.)

; Apeleaza functia 'get-list-of-transformations' cu valori nule
(define (get-transformations n)
  (get-list-of-transformations n 0 0 0))

; Aceasta functie va merge recursiv in calcularea nivelului pe care se va afla n si a nodurilor
; dinainte si din timpul acelui nivel, care, atunci cand gaseste nivelul dorit (cand nr de noduri totale
; e mai mare decat n, apeleaza functia care returneaza rezultatul)
(define (get-list-of-transformations n level total-prev-nodes total-lower-level-nodes)
  (if (= n 1)
      (list)
  (if (= n 2) ; default case
         (list 1)
         (if (= n 3) ; default case
             (list 2)
             (if (= n 4) ; default case
                 (list 3)
  (if (< n total-prev-nodes)
      ; For true, show result
      (get-result n (+ total-lower-level-nodes 1) total-prev-nodes (- level 2) (list))

      ; For false, go down the tree
      (get-list-of-transformations n (+ level 1) (+ total-prev-nodes (expt 3 level)) (quotient (+ total-prev-nodes (expt 3 level)) 3))))))))

; Daca nivelul este -1, returneaza lista
; Altfel parcurge recursiv, actualizand minimul si maximul sau chiar ambele in functie de unul dintre cele 3 cazuri
; pana gasim locul in care se incadreaza 'n'-ul nostru si returneaza lista dorita
(define (get-result n min max level list-ans)
  (if (= level -1)
      list-ans
      ; altfel pune in lista valoarea ceruta
      (cond
        ((< n (+ min (expt 3 level))) (get-result n min (- (+ min (expt 3 level)) 1) (- level 1) (append list-ans (list 1)))); actualizam maximul
        ((and (>= n (+ min (expt 3 level))) (< n (+ min (* (expt 3 level) 2)))) (get-result n (+ min (expt 3 level)) (- (+ min (* (expt 3 level) 2)) 1) (- level 1) (append list-ans (list 2)))) ; actualizam si minimul si maximul
        (else (get-result n (+ min (* (expt 3 level) 2)) max (- level 1) (append list-ans (list 3))))))) ; actualizam minimul

; TODO
; Implementați o funcție care primește o listă Ts de 
; tipul celei întoarsă de get-transformations, respectiv 
; un triplet de start ppt și întoarce tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Utilizați recursivitate pe coadă.
(define (apply-matrix-transformations Ts ppt)
  (get-answer (cdr (reverse Ts)) ppt (get-matrix (get-right-element-list Ts))))

(define (get-right-element-list L)
  (car (reverse L)))

(define (get-matrix index)
  (cond
    ((= index 1) T1)
    ((= index 2) T2)
    (else T3)))
    

(define (get-answer Ts ppt acc)
  (if (null? Ts)
      (multiply acc ppt)
      (get-answer (cdr Ts) ppt (multiply-matrix acc (get-matrix (car Ts))))))

(define (multiply-matrix m1 m2)
  (for/list ([r m1])
    (for/list ([c (apply map list m2)])
      (apply + (map * r c)))))


; TODO
; Implementați o funcție care calculează al n-lea TPP
; din arbore, folosind funcțiile anterioare.
(define (get-nth-ppt-from-matrix-transformations n) ; vom afla path-ul necesar si vom aplica functia care genereaza rezultatul dorit definita anterior
  (if (= n 1)
      (list 3 4 5) ; default case
      (apply-matrix-transformations (get-transformations n) (list 3 4 5))))
